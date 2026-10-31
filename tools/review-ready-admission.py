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
SELECTION = 'MARGINPLYR_REVIEW_SELECTION'
PROTOCOL = b'# Review-ready selection protocol: 2'
ROXYGEN_PROTOCOL = b'# Review-ready selection protocol: 3'

# Supported prose and its verification routes live here. Other file formats in
# these directories have no exemption. Checking machinery remains Full.
PROSE = (r'(AGENTS\.md|CLAUDE\.md|CONTEXT\.md|CODE_OF_CONDUCT\.md|'
         r'(?:design|investigation)/(?:[^/]+/)*[^/]+\.md|tools/[^/]+\.md)')
# Executable vignette inputs match documentation_sources() in test-documentation.R.
DOCUMENTATION = (r'(README\.(?:md|qmd)|NEWS\.md|man/[^/]+\.Rd|'
                 r'man/figures/(?:[^/]+/)*[^/]+\.(?:png|jpg|jpeg|gif|svg|pdf)|'
                 r'vignettes/[^/]+\.qmd|vignettes/(?:[^/]+/)*[^/]+\.(?:png|jpg|jpeg|gif|svg|pdf|bib|csl))')
# Package code and metadata can feed README chunks or roxygen even on Full.
GENERATED_INPUTS = r'(README\.(?:md|qmd)|NAMESPACE|R/.*|DESCRIPTION|inst/.*|data/.*|man/.*|vignettes/.*)'
REFERENCE_CHECKS = ['.github/scripts/verify-doc-references.R',
                    '.github/scripts/verify-context-budget.R',
                    '.github/scripts/verify-verifier-invocation.R']
CONTRACTS = [
    (r'(tools/(?:review-ready[^/]*|test-review-ready[^/]*|install-review-ready\.py|dependency-requirements\.R|required-test-suggests\.R)|'
     r'\.github/scripts/verify-review-ready-check\.R|design/(?:agents/local-checks\.md|specs/review-ready[^/]*\.md))',
     ['.github/scripts/verify-review-ready-check.R', 'tools/test-review-ready-documentation.R',
      'tools/test-review-ready-roxygen.R']),
    (r'tools/(?:review-ready[^/]*\.(?:py|R)|test-review-ready[^/]*|install-review-ready\.py)',
     ['tools/test-review-ready-admission.py', 'tools/test-review-ready-progress.R']),
    (GENERATED_INPUTS, ['tools/review-ready-generated.R']),
    (r'tools/cran-release\.md', ['.github/scripts/verify-cran-release.R']),
    (r'tools/cran-preflight\.md', ['.github/scripts/verify-cran-preflight.R',
                                '.github/scripts/verify-agent-network-policy.R',
                                '.github/scripts/verify-codex-config.R']),
]


def git_bytes(root, *args):
    return subprocess.check_output(['git', '-C', str(root), *args], stderr=subprocess.PIPE)


def select_path(root, sha):
    """Pin local comparison identities and classify the cumulative final trees.

    Unavailable or unsupported evidence can only strengthen the gate to Full.
    Nothing here loads dependencies or starts verification before admission.
    """
    selection = dict(submitted=sha, main=None, base=None, path='Full', reasons=[], verifiers=[])
    try:
        selection['main'] = git(root, 'rev-parse', '--verify', 'refs/heads/main^{commit}')
        bases = git(root, 'merge-base', '--all', sha, selection['main']).splitlines()
        if len(bases) != 1 or not re.fullmatch('[0-9a-f]{40}', bases[0]):
            raise ValueError('missing or ambiguous merge-base')
        selection['base'] = bases[0]
        records = git_bytes(root, 'diff', '--raw', '-z', '--no-abbrev', '--no-ext-diff',
                            '--no-textconv', '--ignore-submodules=none', '--no-relative',
                            '--find-renames', bases[0], sha, '--').split(b'\0')
        if records == [b'']:
            raise ValueError('empty cumulative diff')
        if records.pop() != b'':
            raise ValueError('unterminated diff records')
        names = []
        documentation = False
        roxygen = False
        reasons = []
        index = 0
        while index < len(records):
            header = re.fullmatch(
                rb':([0-7]{6}) ([0-7]{6}) ([0-9a-f]{40}) ([0-9a-f]{40}) ([AMDTRCUXB])([0-9]*)',
                records[index])
            if header is None:
                raise ValueError('unrecognized raw diff record')
            before, after, old_blob, new_blob, status, score = header.groups()
            count = 2 if status in (b'R', b'C') else 1
            paths = records[index + 1:index + 1 + count]
            if len(paths) != count or not all(paths):
                raise ValueError('missing diff path')
            paths = [path.decode('utf-8', errors='strict') for path in paths]
            names.extend(paths)
            modes = {before, after} - {b'000000'}
            if modes != {b'100644'} or (before != after and status not in (b'A', b'D')):
                reasons.append('unsupported file mode/type: ' + repr(paths))
            if status not in (b'A', b'M', b'D', b'R') or (status == b'R' and not score):
                reasons.append('unsupported change status: ' + repr(paths))
            for name in paths:
                documentation |= bool(re.fullmatch(DOCUMENTATION, name))
                if (re.fullmatch(r'R/[^/]+\.R', name) and status == b'M' and
                        before == after == b'100644'):
                    roxygen = True
                    documentation = True
                    continue
                if not (re.fullmatch(PROSE, name) or re.fullmatch(DOCUMENTATION, name)):
                    reasons.append('unsupported path: ' + repr(name))
            supported = all(re.fullmatch(PROSE, name) or re.fullmatch(DOCUMENTATION, name)
                            for name in paths)
            if supported:
                for mode, blob, name in ((before, old_blob, paths[0]),
                                         (after, new_blob, paths[-1])):
                    if mode != b'100644' or re.search(r'\.(png|jpg|jpeg|gif|pdf)$', name):
                        continue
                    try:
                        text = git_bytes(root, 'cat-file', 'blob', blob.decode('ascii')).decode('utf-8')
                        if any(ord(char) < 32 and char not in '\n\r\t' for char in text):
                            raise ValueError('non-prose control character')
                    except (UnicodeError, ValueError):
                        reasons.append('unsupported prose content: ' + repr(paths))
            index += 1 + count
        selection['verifiers'] = list(REFERENCE_CHECKS)
        for pattern, checks in CONTRACTS:
            if any(re.fullmatch(pattern, name) for name in names):
                selection['verifiers'].extend(check for check in checks if check not in selection['verifiers'])
        entry = git_bytes(root, 'show', sha + ':tools/review-ready-check.R')
        if not any(protocol in entry.splitlines() for protocol in (PROTOCOL, ROXYGEN_PROTOCOL)):
            selection['verifiers'] = []
            reasons.append('submitted revision lacks selection protocol; existing Full gate')
        if roxygen and ROXYGEN_PROTOCOL not in entry.splitlines():
            reasons.append('submitted revision lacks R documentation eligibility protocol')
        if not reasons:
            selection['roxygen_pending'] = roxygen
            selection['path'] = 'Full' if roxygen else ('Documentation' if documentation else 'Repository')
            reasons = ['R documentation eligibility pending admission' if roxygen else
                       'all cumulative changes are supported documentation or repository prose'
                       if documentation else 'all cumulative changes are supported repository prose']
        selection['reasons'] = reasons
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        selection['reasons'] = ['comparison unavailable: ' + str(error)]
    return selection


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
    """Read active user processes, requiring the observer's PID/birth identity."""
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
    observer = table.get(os.getpid())
    if observer is None or not observer.get('birth'):
        raise RuntimeError('Unable to read complete process ownership evidence.')
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


def valid_recorded_process(item):
    """Validate the durable PID/birth fields before trusting their absence."""
    return (isinstance(item, dict) and type(item.get('pid')) is int and
            item['pid'] > 0 and isinstance(item.get('birth'), str) and
            bool(item['birth'].strip()))


def read_record(path):
    try:
        contents = path.read_text()
    except FileNotFoundError:
        return None
    record = json.loads(contents)
    if (not isinstance(record, dict) or type(record.get('version')) is not int or
            record['version'] != 1):
        raise RuntimeError('Unknown or invalid admission journal version; recovery requires inspection.')
    try:
        valid = (isinstance(record['token'], str) and
                 re.fullmatch('[0-9a-f]{32}', record['token']) and
                 isinstance(record['sha'], str) and
                 re.fullmatch('[0-9a-f]{40}', record['sha']) and
                 isinstance(record['root'], str) and Path(record['root']).is_absolute() and
                 isinstance(record['boot'], str) and
                 str(uuid.UUID(record['boot'])) == record['boot'] and
                 valid_recorded_process(record['manager']) and
                 isinstance(record['children'], list) and
                 all(valid_recorded_process(item) for item in record['children']) and
                 (record['group'] is None or
                  (type(record['group']) is int and record['group'] > 0)))
    except (KeyError, TypeError, ValueError):
        valid = False
    if not valid:
        raise RuntimeError('Invalid admission journal ownership metadata; recovery requires inspection.')
    return record


def recovery_message(record):
    return ('Admission blocked: shutdown is unconfirmed for ' + record['sha'] +
            ' in ' + record['root'] + '. Inspect with marginplyr-review-ready --status. '
            'Do not delete the lock or journal. After the owned work has stopped, run '
            'marginplyr-review-ready --recover. Recovery requires complete process ownership evidence.')


def recover(path, record, boot):
    if record is None:
        say('No unfinished request to recover. No check was run.')
        return 0
    if record['boot'] == boot:
        # The held admission lock excludes new requests while both complete
        # observations check the trusted gate's recorded and inherited ownership.
        for observation in range(2):
            table = processes()
            if same_process(table, record['manager']) or owned(table, record):
                say(recovery_message(record))
                return BLOCKED
            if observation == 0:
                time.sleep(0.2)
        reason = 'after confirmed shutdown'
    else:
        reason = 'from an earlier OS boot'
    path.unlink()
    say('Recovered unfinished request ' + record['sha'] + ' ' + reason + '. No check was run.')
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
                    resolved = Path(workspace) / 'resolved-path'
                    selected_path = resolved.read_text().strip() if resolved.is_file() else record['selection']['path']
                    if selected_path not in ('Full', 'Documentation', 'Repository'):
                        raise RuntimeError('Invalid resolved Review-ready path.')
                    path.unlink()
                    say('Review-ready terminal outcome: ' + str(status) + '; checked commit: ' + sha +
                        '; selected path: ' + selected_path)
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
    selection = record.get('selection', dict(path='Full', verifiers=[]))
    if selection.get('roxygen_pending'):
        result = Path(checkout).parent / 'roxygen-path'
        proof = subprocess.run(['Rscript', 'tools/review-ready-roxygen.R', selection['base'], sha, str(result)],
                               cwd=checkout)
        selection['path'] = result.read_text().strip() if proof.returncode == 0 and result.is_file() else 'Full'
        if selection['path'] not in ('Full', 'Documentation'):
            raise RuntimeError('Invalid R documentation eligibility result.')
        selection['roxygen_pending'] = False
        selection['reasons'] = ['R documentation proof passed; executable expressions and configuration unchanged'
                                if selection['path'] == 'Documentation' else
                                'R documentation proof unavailable or unsupported R/configuration change']
        say('Review-ready admitted selection: ' + json.dumps(selection, ensure_ascii=True))
    (Path(checkout).parent / 'resolved-path').write_text(selection['path'])
    transport = Path(checkout).parent / 'selection.dcf'
    transport.write_text('Path: ' + selection['path'] + '\nCommit: ' + sha +
                         '\nRequest: ' + record['token'] + '\nVerifiers: ' +
                         ' '.join(selection['verifiers']) + '\n')
    # Overwrite caller-provided selection. Only the lock-authorized worker
    # transports the manager's pinned decision into compatible source gates.
    os.environ[SELECTION] = str(transport)
    os.chdir(checkout)
    os.execvpe('Rscript', ['Rscript', 'tools/review-ready-check.R'], os.environ)


def main(args=None):
    args = list(sys.argv[1:] if args is None else args)
    if args and args[0] == '--run-pinned':
        return run_pinned(args[1:])
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--status', action='store_true', help='show admission state; does not run a check')
    mode.add_argument('--recover', action='store_true', help='recover an unfinished request after confirmed shutdown')
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
        selection = None if options.recover else select_path(root, sha)
        if selection is not None:
            say('Review-ready selection: ' + json.dumps(selection, ensure_ascii=True))
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
                      boot=boot, manager=manager, group=None, children=[], started=time.time(),
                      selection=selection)
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
