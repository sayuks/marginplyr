#!/usr/bin/env python3
"""One user-wide admission boundary for the complete local Review-ready gate.

POSIX only. The flock file is permanent; the durable journal survives a killed
manager. Neither a free lock nor an absent PID is evidence of child shutdown.
"""
import argparse
import errno
import json
import os
from pathlib import Path
import re
import shutil
import signal
import subprocess
import sys
import time
import tempfile
import uuid

if os.name == 'posix':
    import fcntl
    import pwd
    USER_HOME = Path(pwd.getpwuid(os.getuid()).pw_dir)
else:
    USER_HOME = Path.home()
MARKER = 'MARGINPLYR_REVIEW_REQUEST'
CANCELLED = 130
STALE = 3
BLOCKED = 4


def say(message):
    print(message, flush=True)


def git(root, *args):
    return subprocess.check_output(['git', '-C', str(root), *args], text=True,
                                   stderr=subprocess.PIPE).strip()


def identity():
    root = Path(git(Path.cwd(), 'rev-parse', '--show-toplevel')).resolve()
    if root != Path.cwd().resolve():
        raise RuntimeError('Run marginplyr-review-ready from the target repository root.')
    if not re.search(r'^Package:\s*marginplyr\s*$', (root / 'DESCRIPTION').read_text(), re.M):
        raise RuntimeError('The target repository is not marginplyr.')
    sha = git(root, 'rev-parse', 'HEAD')
    if not re.fullmatch('[0-9a-f]{40}', sha):
        raise RuntimeError('A 40-character committed HEAD is required.')
    if git(root, 'status', '--porcelain=v1', '--untracked-files=all'):
        raise RuntimeError('The review-ready check requires a clean committed HEAD.')
    if not (root / 'tools/review-ready-check.R').is_file():
        raise RuntimeError('The target worktree has no fixed Review-ready check.')
    return root, sha


def boot_identity():
    if sys.platform == 'darwin':
        value = subprocess.check_output(['/usr/sbin/sysctl', '-n', 'kern.bootsessionuuid'], text=True).strip()
    else:
        value = Path('/proc/sys/kernel/random/boot_id').read_text().strip()
    return str(uuid.UUID(value))


def processes():
    # Never print this output: process environments can contain credentials.
    result = subprocess.check_output(
        ['ps', 'eww', '-axo', 'pid=,ppid=,pgid=,uid=,stat=,lstart=,command='],
        text=True, errors='replace', stderr=subprocess.PIPE,
        env=dict(os.environ, LC_ALL='C'))
    table = {}
    for line in result.splitlines():
        fields = line.split(None, 10)
        if len(fields) < 11:
            raise RuntimeError('Unable to read complete process ownership evidence.')
        pid, parent, group, uid = map(int, fields[:4])
        if uid == os.getuid() and not fields[4].startswith('Z'):
            table[pid] = dict(pid=pid, parent=parent, group=group,
                              birth=' '.join(fields[5:10]), command=fields[10])
    return table


def evidence(proc):
    return {key: proc[key] for key in ('pid', 'birth')}


def same_process(table, item):
    return item and item['pid'] in table and table[item['pid']]['birth'] == item['birth']


def owned(table, record):
    marker = MARKER + '=' + record['token']
    found = {pid for pid, proc in table.items() if marker in proc['command'].split()}
    found.update(item['pid'] for item in record['children'] if same_process(table, item))
    # Groups supplement the inherited marker, including children that clear env.
    if record.get('group'):
        members = {pid for pid, proc in table.items() if proc['group'] == record['group']}
        if members and not members.intersection(found):
            raise RuntimeError('A process group has no confirmed owned member; cleanup is uncertain.')
        found.update(members)
    changed = True
    while changed:
        children = {pid for pid, proc in table.items() if proc['parent'] in found}
        changed = not children.issubset(found)
        found.update(children)
    return found


def save(path, record):
    temporary = path.with_suffix('.new')
    with temporary.open('w') as stream:
        json.dump(record, stream)
        stream.flush()
        os.fsync(stream.fileno())
    os.replace(temporary, path)
    descriptor = os.open(str(path.parent), os.O_RDONLY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


def read_record(path):
    if not path.exists():
        return None
    record = json.loads(path.read_text())
    if record.get('version') != 1:
        raise RuntimeError('Unknown admission journal version; recovery requires inspection.')
    return record


def recovery_message(record):
    return ('Admission blocked: shutdown is unconfirmed for ' + record['sha'] +
            ' in ' + record['root'] + '. Inspect with marginplyr-review-ready --status. '
            'Do not delete the lock or journal. After the owned work has stopped, run '
            'marginplyr-review-ready --recover; restart the OS if ownership cannot be confirmed.')


def recover(path, record, boot):
    if record is None:
        say('No unfinished request to recover. No check was run.')
        return 0
    if record['boot'] == boot:
        table = processes()
        if same_process(table, record['manager']) or owned(table, record):
            say(recovery_message(record))
            return BLOCKED
        # A management crash can hide descendants that detached and erased env.
        # Only an OS restart provides evidence that those unknown processes ended.
        say(recovery_message(record) + ' This request requires an OS restart before recovery.')
        return BLOCKED
    path.unlink()
    say('Recovered an unfinished request from an earlier OS boot. No check was run.')
    return 0


def run_gate(path, record, root, sha, cancellation, parent):
    job = None
    workspace = None
    try:
        # Persist uncertainty before spawning: even death between spawn and the
        # next write must leave a blocked journal rather than an apparently free slot.
        save(path, record)
        env = dict(os.environ, **{MARKER: record['token']})
        # The old gate also works: a private checkout pins its code and HEAD,
        # even when the caller edits or commits after admission.
        workspace = tempfile.mkdtemp(prefix='marginplyr-admitted-')
        checkout = Path(workspace) / 'source'
        job = subprocess.Popen([sys.executable, str(Path(__file__).resolve()),
                                '--run-pinned', str(path), str(root), sha, str(checkout)],
                               env=env, start_new_session=True)
        record['group'] = job.pid
        save(path, record)
        say('Admitted Review-ready commit: ' + sha + ' in ' + str(root))
        stopping = None
        while True:
            result = job.poll()
            table = processes()
            children = owned(table, record)
            known = {(item['pid'], item['birth']) for item in record['children']}
            new = [evidence(table[pid]) for pid in children
                   if (pid, table[pid]['birth']) not in known]
            if new:
                record['children'].extend(new)
                save(path, record)
            if os.getppid() != parent:
                cancellation[0] = signal.SIGTERM
            if cancellation[0] and stopping is None:
                say('Cancelling Review-ready commit: ' + sha + '; stopping owned processing.')
                stopping = time.monotonic()
            if stopping is not None:
                sig = signal.SIGTERM if time.monotonic() - stopping < 2 else signal.SIGKILL
                for pid in children:
                    if not same_process(processes(), evidence(table[pid])):
                        continue
                    try:
                        os.kill(pid, sig)
                    except ProcessLookupError:
                        pass
            if result is not None and not children:
                # A second complete observation avoids releasing on a transient
                # process-table view while a known child is exiting.
                if not owned(processes(), record):
                    status = CANCELLED if stopping is not None else (result if result >= 0 else 128 - result)
                    path.unlink()
                    say('Review-ready terminal outcome: ' + str(status) + '; checked commit: ' + sha)
                    return status
            if stopping is not None and time.monotonic() - stopping > 10:
                raise RuntimeError('Owned processing did not stop within the cleanup deadline.')
            time.sleep(0.5)
    except BaseException:
        # Errors retain the journal. Best effort shutdown is not proof of cleanup.
        if job is not None and job.poll() is None:
            try:
                job.kill()
                job.wait(timeout=5)
            except (OSError, subprocess.TimeoutExpired):
                pass
        say(recovery_message(record))
        raise
    finally:
        if workspace is not None and not path.exists():
            shutil.rmtree(workspace)
        elif workspace is not None:
            # Keep uncertain work's checkout available for inspection.
            say('Unfinished checkout retained: ' + workspace)


def run_pinned(args):
    """Internal worker; only a live lock-holding manager can authorize it."""
    if len(args) != 4 or MARKER not in os.environ:
        raise RuntimeError('The pinned worker requires an admitted manager.')
    path, root, sha, checkout = args
    record = read_record(Path(path))
    if (not record or record['token'] != os.environ[MARKER] or
            record['sha'] != sha or record['root'] != root or
            record['manager']['pid'] != os.getppid()):
        raise RuntimeError('The pinned worker has no matching admitted request.')
    # The worker must still be inside the shared admission boundary.
    with (Path(path).parent / 'admission.lock').open('a+') as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            pass
        else:
            raise RuntimeError('The pinned worker requires held admission capacity.')
    subprocess.run(['git', 'clone', '--quiet', '--shared', '--no-checkout', root, checkout], check=True)
    subprocess.run(['git', '-C', checkout, 'checkout', '--quiet', '--detach', sha], check=True)
    os.chdir(checkout)
    os.execvpe('Rscript', ['Rscript', 'tools/review-ready-check.R'], os.environ)


def main(args=None):
    args = list(sys.argv[1:] if args is None else args)
    if args and args[0] == '--run-pinned':
        return run_pinned(args[1:])
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--status', action='store_true', help='show admission state; does not run a check')
    mode.add_argument('--recover', action='store_true', help='recover an unfinished request after OS restart')
    options = parser.parse_args(args)
    if sys.platform not in ('darwin', 'linux'):
        say('Unsupported platform: this admission CLI requires macOS or Linux. No check was run.')
        return 2
    cancellation = [0]
    previous = {}
    for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        previous[sig] = signal.signal(sig, lambda number, frame: cancellation.__setitem__(0, number))
    lock = None
    try:
        state = USER_HOME / '.local/state/marginplyr/review-ready'
        state.mkdir(parents=True, exist_ok=True, mode=0o700)
        path = state / 'request.json'
        if options.status:
            record = read_record(path)
            if record:
                record['operation'] = 'status only; no check was run'
            say(json.dumps(record, indent=2) if record else 'No active or unfinished request. No check was run.')
            return 0
        root, sha = (None, None) if options.recover else identity()
        if not options.recover and not shutil.which('Rscript'):
            raise RuntimeError('Rscript is missing from PATH. Install the review prerequisites; no check was run.')
        # Check process visibility before waiting or starting any heavy stage.
        table = processes()
        boot = boot_identity()
        parent = os.getppid()
        manager = evidence(table[os.getpid()])
        lock = (state / 'admission.lock').open('a+')
        started = time.monotonic()
        displayed = -float('inf')
        while True:
            if cancellation[0] or os.getppid() != parent:
                say('Cancelled waiting request for ' + str(sha) + '. No check was run.')
                return CANCELLED
            try:
                fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except OSError as error:
                if error.errno not in (errno.EAGAIN, errno.EACCES):
                    raise
            if options.recover:
                say('An active manager holds capacity; recovery refused. No check was run.')
                return BLOCKED
            if time.monotonic() - displayed >= 5:
                active = read_record(path)
                detail = ('; active ' + active['sha'] + ' in ' + active['root']) if active else '; active identity pending'
                say('Waiting for Review-ready capacity: requested ' + sha + '; elapsed ' +
                    str(int(time.monotonic() - started)) + 's' + detail)
                displayed = time.monotonic()
            time.sleep(0.15)
        record = read_record(path)
        if options.recover:
            return recover(path, record, boot)
        if record:
            say(recovery_message(record))
            return BLOCKED
        try:
            current_root, current_sha = identity()
            if (current_root, current_sha) != (root, sha):
                raise RuntimeError('HEAD changed while waiting.')
        except (RuntimeError, subprocess.SubprocessError, OSError) as error:
            say('Stale request cancelled for ' + sha + ': ' + str(error) + ' Submit a fresh request.')
            return STALE
        if cancellation[0]:
            return CANCELLED
        record = dict(version=1, token=uuid.uuid4().hex, sha=sha, root=str(root),
                      boot=boot, manager=manager, group=None, children=[], started=time.time())
        return run_gate(path, record, root, sha, cancellation, parent)
    except (OSError, ValueError, KeyError, TypeError, RuntimeError, subprocess.SubprocessError) as error:
        say('Review-ready admission failed: ' + str(error) + '. No passing check evidence.')
        return 2
    finally:
        if lock is not None:
            lock.close()
        for sig, handler in previous.items():
            signal.signal(sig, handler)


if __name__ == '__main__':
    sys.exit(main())
