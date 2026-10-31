#!/usr/bin/env python3
"""Supervise Review-ready requests; admit one complete Full gate per OS user.

POSIX only. The flock file is permanent; the durable journal survives a killed
manager. Neither a free lock nor an absent PID is evidence of child shutdown.
"""
import argparse
import errno
import fnmatch
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import signal
import socket
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
ROXYGEN_PROTOCOL = b'# Review-ready roxygen proof protocol: 1'
SUPERVISION_PROTOCOL = 3
EXEMPT_PROTOCOL = b'# Review-ready supervision protocol: 3'
# The installer embeds fingerprints of the reviewed stage contracts. An older
# or directly copied launcher has no exemption capability and stays locked.
CAPABILITY_DIGESTS = {}  # installed capabilities
LAUNCHER_SOURCE = Path(__file__).read_bytes()

CORE_SOURCES = (
    'tools/review-ready-check.R', 'tools/review-ready-check-lib.R',
    'tools/review-ready-progress.R', 'tools/review-ready-progress-reporter.R',
    'tools/dependency-requirements.R', 'tools/required-test-suggests.R',
    '.github/scripts/cran-note-policy.R',
    'DESCRIPTION', 'NAMESPACE', '.Rbuildignore', 'jarl.toml', '.lintr*', '.Rprofile*',
    'tools/review-ready-roxygen.R',
    'inst/suggests/guard.R',
)
DOCUMENTATION_SOURCES = (
    'tools/review-ready-documentation.R', 'tests/testthat/test-documentation.R',
    'tests/testthat/test-duckdb-storage.R', 'tests/testthat/test-package-metadata.R',
    'tests/testthat/helper*.R', 'tests/testthat/setup*.R', 'tests/testthat/teardown*.R',
    'inst/suggests/guard.R',
)
# Policies describe reviewed workloads, never a guess from source spellings.
# Only the progress fixture exercises testthat workers outside Full capacity.
STAGE_CONTRACTS = {
    '.github/scripts/verify-doc-references.R': ('serial', ('.github/scripts/ci-helpers.R',)),
    '.github/scripts/verify-context-budget.R': ('serial', ('.github/scripts/ci-helpers.R',)),
    '.github/scripts/verify-verifier-invocation.R': ('serial', ('.github/scripts/ci-helpers.R',)),
    '.github/scripts/verify-review-ready-check.R': ('serial', CORE_SOURCES),
    'tools/test-review-ready-documentation.R': ('serial', CORE_SOURCES + (
        'tools/review-ready-documentation.R', 'tools/review-ready-generated.R')),
    'tools/test-review-ready-admission.py': ('process-fixture', (
        'tools/review-ready-admission.py', 'tools/review-ready-check.R', 'tools/install-review-ready.py',
        '.github/scripts/verify-doc-references.R', '.github/scripts/ci-helpers.R')),
    'tools/test-review-ready-progress.R': ('parallel-fixture-2', CORE_SOURCES + ('tests/testthat.R',)),
    'tools/test-review-ready-roxygen.R': ('serial', CORE_SOURCES),
    'tools/review-ready-generated.R': ('serial', ()),
    '.github/scripts/verify-cran-release.R': ('serial', (
        'tools/cran-release.R', 'tools/dependency-requirements.R')),
    '.github/scripts/verify-cran-preflight.R': ('serial', (
        '.github/scripts/cran-note-policy.R', '.github/scripts/checktor-baseline.R',
        '.github/scripts/verify-agent-network-policy.R',
        'tools/dependency-requirements.R', 'tools/cran-preflight-lib.R', 'tools/cran-preflight.R')),
    '.github/scripts/verify-agent-network-policy.R': ('serial', ()),
    '.github/scripts/verify-codex-config.R': ('serial', ()),
}


def capability_patterns():
    return dict(core=CORE_SOURCES, documentation=DOCUMENTATION_SOURCES,
                **{stage: (stage,) + dependencies
                   for stage, (_, dependencies) in STAGE_CONTRACTS.items()})


def installed_source(root):
    """Bind a standalone installation to the reviewed stage source bytes."""
    tracked = set(git_bytes(root, 'ls-files', '-z').decode('utf-8').split('\0'))
    digests = {}
    for group, patterns in capability_patterns().items():
        files = {path for pattern in patterns for path in root.glob(pattern)
                 if path.is_file() and path.relative_to(root).as_posix() in tracked}
        digests[group] = {
            path.relative_to(root).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in sorted(files) if any(fnmatch.fnmatchcase(path.relative_to(root).as_posix(), pattern)
                                    for pattern in patterns)
        }
        for pattern in patterns:
            if not any(char in pattern for char in '*?[') and pattern not in digests[group]:
                raise RuntimeError('Missing installation contract source: ' + pattern)
    source = (root / 'tools/review-ready-admission.py').read_bytes()
    return source.replace(b'CAPABILITY_DIGESTS = {}  # installed capabilities',
                          ('CAPABILITY_DIGESTS = ' + repr(digests)).encode(), 1)


def eligibility(root, sha, selection):
    """Return a reason to require Full when a stage lacks installed capability."""
    if selection['path'] == 'Full':
        return None
    entry = git_bytes(root, 'show', sha + ':tools/review-ready-check.R')
    if EXEMPT_PROTOCOL not in entry.splitlines():
        return 'source lacks compatible independent supervision; legacy locked Full contract'
    for stage in selection['verifiers']:
        contract = STAGE_CONTRACTS.get(stage)
        if not contract or contract[0] not in ('serial', 'process-fixture', 'parallel-fixture-2'):
            return 'stage workload requires Full capacity: ' + stage
    groups = ['core'] + selection['verifiers']
    if selection['path'] == 'Documentation':
        groups.append('documentation')
    tree = {}
    for entry in git_bytes(root, 'ls-tree', '-rz', sha).split(b'\0'):
        if entry:
            metadata, name = entry.split(b'\t', 1)
            mode, kind, oid = metadata.split()
            tree[name.decode('utf-8')] = (mode, kind, oid)
    required = {}
    for group in groups:
        if group not in capability_patterns() or group not in CAPABILITY_DIGESTS:
            return 'unclassified stage or unavailable installed capability: ' + group
        expected = CAPABILITY_DIGESTS[group]
        paths = {name for name in tree if any(fnmatch.fnmatchcase(name, pattern)
                                               for pattern in capability_patterns()[group])}
        if paths != set(expected):
            return 'unsupported stage source set: ' + group
        required.update(expected)
    oids = []
    for name in required:
        mode, kind, oid = tree[name]
        if kind != b'blob' or mode not in (b'100644', b'100755'):
            return 'unsupported stage source type: ' + name
        if oid not in oids:
            oids.append(oid)
    result = subprocess.run(['git', '-C', str(root), 'cat-file', '--batch'],
                            input=b'\n'.join(oids) + b'\n', stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, check=True).stdout
    blobs = {}
    offset = 0
    for oid in oids:
        boundary = result.index(b'\n', offset)
        actual, kind, size = result[offset:boundary].split()
        if actual != oid or kind != b'blob':
            raise RuntimeError('Unsupported capability object response.')
        end = boundary + 1 + int(size)
        blobs[oid] = hashlib.sha256(result[boundary + 1:end]).hexdigest()
        if result[end:end + 1] != b'\n':
            raise RuntimeError('Incomplete capability object response.')
        offset = end + 1
    for name, digest in required.items():
        if blobs[tree[name][2]] != digest:
            return 'unsupported stage source capability: ' + name
    return None

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
        if PROTOCOL not in entry.splitlines():
            selection['verifiers'] = []
            reasons.append('submitted revision lacks selection protocol; existing Full gate')
        if roxygen and ROXYGEN_PROTOCOL not in entry.splitlines():
            reasons.append('submitted revision lacks R documentation eligibility protocol')
        if not reasons:
            selection['roxygen_pending'] = roxygen
            selection['path'] = 'Documentation' if documentation else 'Repository'
            reasons = ['R documentation eligibility pending supervised proof' if roxygen else
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
            record['version'] not in (1, 2)):
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
        if record['version'] == 2:
            valid = valid and (record['protocol'] == SUPERVISION_PROTOCOL and
                re.fullmatch('[0-9a-f]{32}', record['id']) and
                record['id'] == path.parent.name and
                record['state'] in ('preparing', 'waiting', 'acquiring', 'running',
                                    'promoting', 'cancelling', 'recovery', 'terminal') and
                record['capacity'] in ('exempt', 'Full') and
                type(record['may_own_full']) is bool and
                type(record['owned_capacity']) is bool and
                type(record['epoch']) is int and record['epoch'] > 0 and
                type(record['authorized']) is bool and
                type(record.get('gate_started', False)) is bool and
                type(record['requires_proof']) is bool and
                type(record['channel_owned']) is bool and
                isinstance(record['selection'], dict) and
                record['selection']['submitted'] == record['sha'] and
                record['selection']['path'] in ('Repository', 'Documentation', 'Full') and
                (record['selection']['path'] == 'Full') == (record['capacity'] == 'Full') and
                (not record['owned_capacity'] or
                 (record['may_own_full'] and record['capacity'] == 'Full')) and
                (record['capacity'] != 'exempt' or not record['may_own_full']) and
                (record['state'] != 'running' or record['capacity'] != 'Full' or
                 record['owned_capacity']) and
                record['workspace'] == str(path.parent / 'workspace') and
                record['channel'] == '/tmp/marginplyr-review-' + record['id'] + '.sock')
    except (KeyError, TypeError, ValueError):
        valid = False
    if not valid:
        raise RuntimeError('Invalid admission journal ownership metadata; recovery requires inspection.')
    return record


def active_records(state):
    """Enumerate independent requests; malformed evidence remains visible."""
    records = []
    requests = state / 'requests'
    if requests.exists():
        for directory in sorted(requests.iterdir()):
            path = directory / 'record.json'
            try:
                record = read_record(path)
                if record is None:
                    raise RuntimeError('Request record missing (possibly interrupted durable write).')
                if record['state'] != 'terminal':
                    records.append((path, record, None))
            except (OSError, ValueError, KeyError, TypeError, RuntimeError) as error:
                records.append((path, None, str(error)))
    legacy = state / 'request.json'
    try:
        record = read_record(legacy)
        # A mirrored v1 journal protects capacity against older launchers too.
        if record and not any(item and item.get('id') == record.get('id')
                              for _, item, _ in records):
            records.append((legacy, record, None))
    except (OSError, ValueError, RuntimeError) as error:
        records.append((legacy, None, str(error)))
    return records


def recovery_message(record):
    request = record.get('id', 'legacy')
    return ('Admission blocked: shutdown is unconfirmed for request ' + request + ' (' +
            record['sha'] + ') in ' + record['root'] + '. Inspect with '
            'marginplyr-review-ready --status. Do not delete the lock or journal. '
            'After the owned work has stopped, run marginplyr-review-ready --recover ' + request +
            '. Recovery requires complete process ownership evidence.')


def lock_file(path):
    lock = path.open('a+')
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BaseException:
        lock.close()
        raise
    return lock


def persist(path, record):
    save(path, record)
    if record['owned_capacity']:
        legacy = dict(record, version=1)
        save(path.parents[2] / 'request.json', legacy)


def remove_full_mirror(state, record):
    path = state / 'request.json'
    mirror = read_record(path)
    if mirror and mirror.get('id') == record['id']:
        path.unlink()


def clean_workspace(record):
    workspace = Path(record['workspace'])
    if workspace.exists():
        shutil.rmtree(workspace)
    if record.get('channel_owned'):
        Path(record['channel']).unlink(missing_ok=True)


def recover_request(state, requested, boot):
    candidates = active_records(state)
    if not candidates:
        say('No unfinished request to recover. No check was run.')
        return 0
    ids = [record.get('id', 'legacy') if record else
           ('legacy' if path == state / 'request.json' else path.parent.name)
           for path, record, _ in candidates]
    if requested is None or requested not in ids or ids.count(requested) != 1:
        say('Recovery requires one explicit request ID; candidates: ' + ', '.join(ids) +
            '. No check was run; no state changed.')
        return BLOCKED
    path, record, error = candidates[ids.index(requested)]
    if error:
        raise RuntimeError(error)
    locks = []
    ownership_records = [record]
    try:
        if record['version'] == 2:
            locks.append(lock_file(path.parent / 'manager.lock'))
        if record['version'] == 1 or record['may_own_full']:
            locks.append(lock_file(state / 'admission.lock'))
            # Validate the compatibility fence before changing independent state.
            mirror = read_record(state / 'request.json')
            if mirror and mirror.get('id') == record.get('id'):
                ownership_records.append(mirror)
        if record['boot'] == boot:
            for observation in range(2):
                table = processes()
                if any(same_process(table, item['manager']) or owned(table, item)
                       for item in ownership_records):
                    say(recovery_message(record))
                    return BLOCKED
                if observation == 0:
                    time.sleep(0.2)
            reason = 'after confirmed shutdown'
        else:
            reason = 'from an earlier OS boot'
        if record['version'] == 2:
            clean_workspace(record)
            record.update(state='terminal', outcome='interrupted; recovered, no check evidence',
                          cleanup='confirmed', owned_capacity=False, finished=time.time())
            save(path, record)
            remove_full_mirror(state, record)
        else:
            path.unlink()
        say('Recovered unfinished request ' + requested + ' ' + reason + '. No check was run.')
        return 0
    except BlockingIOError:
        say('An active manager holds request/capacity ownership; recovery refused. No check was run.')
        return BLOCKED
    finally:
        for lock in reversed(locks):
            lock.close()


def full_blocker(state, request):
    for path, record, error in active_records(state):
        if error:
            return 'Full recovery required for ' + str(path) + ': ' + error
        if record.get('id') == request:
            continue
        if record['version'] == 1 or record['may_own_full']:
            return recovery_message(record)
    return None


def acquire_full(state, path, record, cancellation, parent):
    """Keep one indivisible Full slot, including cleanup, with legacy fencing."""
    record.update(state='waiting', capacity='Full', owned_capacity=False)
    persist(path, record)
    started = time.monotonic()
    displayed = -float('inf')
    while True:
        if cancellation[0] or os.getppid() != parent:
            return None, CANCELLED
        lock = None
        try:
            lock = lock_file(state / 'admission.lock')
            blocker = full_blocker(state, record['id'])
            if blocker:
                say(blocker)
                lock.close()
                return None, BLOCKED
            # Persist uncertainty before any capacity or spawn side effect.
            record.update(state='acquiring', may_own_full=True)
            persist(path, record)
            try:
                current_root, current_sha = identity()
                if (str(current_root), current_sha) != (record['root'], record['sha']):
                    raise RuntimeError('HEAD changed while waiting.')
            except (RuntimeError, subprocess.SubprocessError, OSError) as error:
                say('Stale request cancelled for ' + record['sha'] + ': ' + str(error) +
                    ' Submit a fresh request.')
                return lock, STALE
            if cancellation[0] or os.getppid() != parent:
                return lock, CANCELLED
            record.update(owned_capacity=True)
            persist(path, record)
            return lock, 0
        except BlockingIOError:
            if time.monotonic() - displayed >= 5:
                active = read_record(state / 'request.json')
                detail = ('; active ' + active['sha'] + ' in ' + active['root']) if active else '; active identity pending'
                say('Waiting for Review-ready capacity: requested ' + record['sha'] +
                    '; request ' + record['id'] + '; elapsed ' +
                    str(int(time.monotonic() - started)) + 's' + detail)
                displayed = time.monotonic()
            time.sleep(0.15)
        except BaseException:
            if lock:
                lock.close()
            raise


def rpc_request(operation, reason=''):
    """Request-private manager channel, also used by the R gate's proof helper."""
    path = Path(os.environ['MARGINPLYR_REVIEW_RECORD'])
    record = read_record(path)
    message = dict(protocol=SUPERVISION_PROTOCOL, request=record['id'],
                   token=os.environ[MARKER], sha=os.environ['MARGINPLYR_REVIEW_COMMIT'],
                   epoch=int(os.environ['MARGINPLYR_REVIEW_EPOCH']), operation=operation,
                   expected='running', reason=reason)
    if operation == 'authorize':
        message['transport'] = hashlib.sha256(Path(os.environ[SELECTION]).read_bytes()).hexdigest()
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as channel:
        channel.settimeout(10)
        channel.connect(record['channel'])
        channel.sendall(json.dumps(message).encode() + b'\n')
        return json.loads(receive_message(channel))


def receive_message(channel):
    data = b''
    while b'\n' not in data:
        chunk = channel.recv(4096)
        if not chunk:
            raise RuntimeError('Incomplete supervisor message.')
        data += chunk
        if len(data) > 16384:
            raise RuntimeError('Oversized supervisor message.')
    if data.count(b'\n') != 1 or not data.endswith(b'\n'):
        raise RuntimeError('Malformed supervisor message.')
    return data.decode('utf-8')


def selection_transport(record):
    selection = record['selection']
    return ('Path: ' + selection['path'] + '\nCommit: ' + record['sha'] +
            '\nRequest: ' + record['token'] + '\nProtocol: 3\nCapacity: ' + record['capacity'] +
            '\nVerifiers: ' + ' '.join(selection['verifiers']) + '\n')


def serve_control(server, path, record, cancellation, parent):
    """Authenticate one state transition; rejected messages never change state."""
    try:
        channel, _ = server.accept()
    except BlockingIOError:
        return False
    with channel:
        channel.settimeout(0.2)
        try:
            message = json.loads(receive_message(channel))
            expected = dict(protocol=SUPERVISION_PROTOCOL, request=record['id'], token=record['token'],
                            sha=record['sha'], epoch=record['epoch'], expected='running')
            if (not isinstance(message, dict) or
                    any(message.get(key) != value for key, value in expected.items()) or
                    record['state'] != 'running' or cancellation[0] or os.getppid() != parent):
                raise RuntimeError('Stale, mismatched or cancelled supervisor request.')
            operation = message.get('operation')
            if operation == 'begin-gate':
                if record.get('gate_started'):
                    raise RuntimeError('Duplicate gate start.')
                if record['selection'].get('roxygen_pending'):
                    raise RuntimeError('R documentation eligibility is still pending.')
                record['gate_started'] = True
                persist(path, record)
                reply = dict(ok=True)
            elif operation == 'authorize':
                if not record.get('gate_started'):
                    raise RuntimeError('The supervised worker has not begun the gate.')
                if record['authorized']:
                    raise RuntimeError('Duplicate gate authorization.')
                if record['selection']['path'] == 'Full' and not record['owned_capacity']:
                    raise RuntimeError('Full execution requires owned capacity.')
                digest = hashlib.sha256(selection_transport(record).encode()).hexdigest()
                if message.get('transport') != digest:
                    raise RuntimeError('Selection transport does not match the supervised contract.')
                record['authorized'] = True
                persist(path, record)
                reply = dict(ok=True, path=record['selection']['path'], capacity=record['capacity'])
            elif operation == 'resolve-documentation':
                if record.get('gate_started') or not record['selection'].get('roxygen_pending'):
                    raise RuntimeError('R documentation eligibility is not pending.')
                result = Path(record['workspace']) / 'roxygen-path'
                if result.read_text() != 'Documentation\n':
                    raise RuntimeError('Invalid R documentation proof transport.')
                record['selection']['roxygen_pending'] = False
                record['selection']['reasons'].append('R documentation proof passed without evaluation')
                persist(path, record)
                reply = dict(ok=True)
            elif operation == 'promote':
                if record['capacity'] != 'exempt':
                    raise RuntimeError('Promotion is only valid for running exempt work.')
                if record.get('gate_started'):
                    raise RuntimeError('Eligibility promotion must precede gate execution; validation failures are terminal.')
                reason = message.get('reason')
                if not isinstance(reason, str) or not reason.strip() or len(reason) > 2000:
                    raise RuntimeError('Promotion requires a bounded eligibility reason.')
                record['promotions'].append(dict(epoch=record['epoch'], reason=reason, at=time.time(),
                                                 source=record['selection']['path'], target='Full'))
                record.update(state='promoting')
                persist(path, record)
                channel.sendall(b'{"ok": true, "promoting": true}\n')
                return True
            else:
                raise RuntimeError('Unsupported supervisor operation.')
            channel.sendall(json.dumps(reply).encode() + b'\n')
        except (ValueError, RuntimeError, socket.timeout) as error:
            try:
                channel.sendall(json.dumps(dict(ok=False, error=str(error))).encode() + b'\n')
            except OSError:
                pass
    return False


def stop_owned(table, children, stopping):
    sig = signal.SIGTERM if time.monotonic() - stopping < 2 else signal.SIGKILL
    fresh = processes()
    for pid in children:
        if same_process(fresh, evidence(table[pid])):
            try:
                os.kill(pid, sig)
            except ProcessLookupError:
                pass


def supervise(path, record, server, cancellation, parent):
    """Observe and collect one worker epoch before finishing or changing capacity."""
    workspace = Path(record['workspace'])
    record.update(state='running', authorized=False)
    persist(path, record)
    env = dict(os.environ, **{MARKER: record['token'],
        'MARGINPLYR_REVIEW_RECORD': str(path), 'MARGINPLYR_REVIEW_COMMIT': record['sha'],
        'MARGINPLYR_REVIEW_EPOCH': str(record['epoch']),
        'MARGINPLYR_REVIEW_SUPERVISOR': str(workspace / 'supervisor.py')})
    with (path.parent / 'output.log').open('ab', buffering=0) as output:
        job = subprocess.Popen([sys.executable, str(workspace / 'supervisor.py'), '--run-pinned',
                                str(path), record['root'], record['sha'], str(workspace / 'source')],
                               env=env, start_new_session=True, stdout=output, stderr=output)
    record['group'] = job.pid
    persist(path, record)
    say('Admitted Review-ready commit: ' + record['sha'] + ' in ' + record['root'] +
        '; request: ' + record['id'] + '; path: ' + record['selection']['path'] +
        '; capacity: ' + record['capacity'] + '; worker policy: ' + record['worker_policy'])
    stopping = None
    promoting = False
    offset = record.get('output_offset', 0)
    try:
        while True:
            with (path.parent / 'output.log').open('rb') as output:
                output.seek(offset)
                chunk = output.read()
                offset = output.tell()
                record['output_offset'] = offset
            if chunk:
                sys.stdout.write(chunk.decode('utf-8', errors='replace'))
                sys.stdout.flush()
            result = job.poll()
            table = processes()
            children = owned(table, record)
            known = {(item['pid'], item['birth']) for item in record['children']}
            new = [evidence(table[pid]) for pid in children if (pid, table[pid]['birth']) not in known]
            if new:
                record['children'].extend(new)
                persist(path, record)
            if os.getppid() != parent:
                cancellation[0] = signal.SIGTERM
            if serve_control(server, path, record, cancellation, parent):
                promoting = True
            if (cancellation[0] or promoting) and stopping is None:
                say(('Promoting' if promoting and not cancellation[0] else 'Cancelling') +
                    ' Review-ready commit: ' + record['sha'] + '; stopping owned processing.')
                stopping = time.monotonic()
                if cancellation[0]:
                    record['state'] = 'cancelling'
                    persist(path, record)
            if stopping is not None:
                stop_owned(table, children, stopping)
            if result is not None and not children and not owned(processes(), record):
                # Drain output only after the last owned writer has stopped.
                with (path.parent / 'output.log').open('rb') as output:
                    output.seek(offset)
                    sys.stdout.write(output.read().decode('utf-8', errors='replace'))
                    sys.stdout.flush()
                    record['output_offset'] = output.tell()
                if cancellation[0]:
                    return CANCELLED, False
                if promoting:
                    return 0, True
                if result == 0 and record['requires_proof'] and not record['authorized']:
                    say('Compatible source omitted live gate authorization; no passing check evidence.')
                    return 2, False
                return (result if result >= 0 else 128 - result), False
            if stopping is not None and time.monotonic() - stopping > 10:
                raise RuntimeError('Owned processing did not stop within the cleanup deadline.')
            time.sleep(0.1)
    except BaseException:
        # The Popen handle identifies this unreaped child even when process
        # inspection failed. Other descendants remain protected by the record.
        if job.poll() is None:
            try:
                job.kill()
                job.wait(timeout=5)
            except (OSError, subprocess.TimeoutExpired):
                pass
        raise


def finish_request(state, path, record, status):
    clean_workspace(record)
    record.update(state='terminal', outcome=status, cleanup='confirmed', finished=time.time())
    # Capacity stays owned until disposable-source cleanup is complete.
    persist(path, record)
    remove_full_mirror(state, record)
    record['owned_capacity'] = False
    save(path, record)
    say('Review-ready terminal outcome: ' + str(status) + '; request: ' + record['id'] +
        '; checked commit: ' + record['sha'] + '; selected path: ' + record['selection']['path'] +
        '; capacity: ' + record['capacity'] + '; cleanup: confirmed; promotions: ' +
        json.dumps(record['promotions']))
    return status


def run_request(state, path, record, cancellation, parent):
    capacity = None
    server = None
    try:
        workspace = Path(record['workspace'])
        workspace.mkdir(mode=0o700)
        script = workspace / 'supervisor.py'
        script.write_bytes(LAUNCHER_SOURCE)
        record['launcher'] = hashlib.sha256(LAUNCHER_SOURCE).hexdigest()
        persist(path, record)
        server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        server.bind(record['channel'])
        os.chmod(record['channel'], 0o600)
        record['channel_owned'] = True
        persist(path, record)
        server.listen()
        server.setblocking(False)
        while True:
            if record['capacity'] == 'Full':
                capacity, status = acquire_full(state, path, record, cancellation, parent)
                if status:
                    return finish_request(state, path, record, status)
            elif cancellation[0] or os.getppid() != parent:
                return finish_request(state, path, record, CANCELLED)
            else:
                current_root, current_sha = identity()
                if (str(current_root), current_sha) != (record['root'], record['sha']):
                    return finish_request(state, path, record, STALE)
            status, promoting = supervise(path, record, server, cancellation, parent)
            if not promoting:
                return finish_request(state, path, record, status)
            # All preparatory work and owned children are stopped and collected.
            record.update(capacity='Full', state='waiting', group=None, children=[],
                          epoch=record['epoch'] + 1, authorized=False,
                          gate_started=False,
                          worker_policy='Full suite: 4 test workers')
            record['selection']['path'] = 'Full'
            record['selection']['roxygen_pending'] = False
            persist(path, record)
            shutil.rmtree(workspace / 'source', ignore_errors=False)
    except BaseException:
        record.update(state='recovery', cleanup='unconfirmed')
        try:
            persist(path, record)
        finally:
            say(recovery_message(record))
            say('Unfinished checkout retained: ' + record['workspace'])
        raise
    finally:
        if server is not None:
            server.close()
        if capacity is not None:
            capacity.close()


def run_pinned(args):
    """Run pinned source only inside its live, matching supervision generation."""
    if len(args) != 4 or MARKER not in os.environ:
        raise RuntimeError('The pinned worker requires an admitted manager.')
    path, root, sha, checkout = args
    record = read_record(Path(path))
    if (not record or record['version'] != 2 or record['protocol'] != SUPERVISION_PROTOCOL or
            record['token'] != os.environ[MARKER] or record['sha'] != sha or record['root'] != root or
            record['manager']['pid'] != os.getppid() or record['state'] != 'running' or
            record['epoch'] != int(os.environ['MARGINPLYR_REVIEW_EPOCH']) or
            record['launcher'] != hashlib.sha256(LAUNCHER_SOURCE).hexdigest()):
        raise RuntimeError('The pinned worker has no matching supervised request.')
    if record['capacity'] == 'Full':
        if not record['owned_capacity']:
            raise RuntimeError('Full execution requires proof of owned capacity.')
        with (Path(path).parents[2] / 'admission.lock').open('a+') as lock:
            try:
                fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError:
                pass
            else:
                raise RuntimeError('The pinned worker requires held admission capacity.')
    subprocess.run(['git', 'clone', '--quiet', '--shared', '--no-checkout', root, checkout], check=True)
    subprocess.run(['git', '-C', checkout, 'checkout', '--quiet', '--detach', sha], check=True)
    selection = record['selection']
    os.environ['TESTTHAT_PARALLEL'] = 'false'
    os.environ['TESTTHAT_CPUS'] = '1'
    if record['capacity'] == 'exempt':
        reason = eligibility(Path(checkout), sha, selection)
        if reason:
            reply = rpc_request('promote', reason)
            if not reply.get('ok'):
                raise RuntimeError('Supervised promotion refused: ' + str(reply))
            # The supervisor stops this epoch and all its children before waiting.
            while True:
                time.sleep(1)
    if selection.get('roxygen_pending'):
        result = Path(checkout).parent / 'roxygen-path'
        proof = subprocess.run(['Rscript', 'tools/review-ready-roxygen.R', selection['base'], sha, str(result)],
                               cwd=checkout)
        if proof.returncode != 0 or not result.is_file():
            raise RuntimeError('R documentation eligibility failed; no Full fallback.')
        resolved = result.read_text()
        if resolved == 'Full\n':
            reply = rpc_request('promote', 'R documentation proof requires Full')
            if not reply.get('ok'):
                raise RuntimeError('Supervised promotion refused: ' + str(reply))
            while True:
                time.sleep(1)
        if resolved != 'Documentation\n':
            raise RuntimeError('Invalid R documentation eligibility result; no Full fallback.')
        reply = rpc_request('resolve-documentation')
        if not reply.get('ok'):
            raise RuntimeError('Supervised R documentation resolution refused: ' + str(reply))
        record = read_record(Path(path))
        selection = record['selection']
        say('Review-ready resolved selection: ' + json.dumps(selection, ensure_ascii=True))
    transport = Path(checkout).parent / 'selection.dcf'
    transport.write_text(selection_transport(record))
    os.environ[SELECTION] = str(transport)
    reply = rpc_request('begin-gate')
    if not reply.get('ok'):
        raise RuntimeError('Supervised gate start refused: ' + str(reply))
    os.chdir(checkout)
    os.execvpe('Rscript', ['Rscript', 'tools/review-ready-check.R'], os.environ)


def main(args=None):
    args = list(sys.argv[1:] if args is None else args)
    if args and args[0] == '--run-pinned':
        return run_pinned(args[1:])
    if args == ['--gate-proof']:
        reply = rpc_request('authorize')
        if not reply.get('ok'):
            raise RuntimeError('Supervision proof refused: ' + str(reply))
        say('Path: ' + reply['path'] + '\nCapacity: ' + reply['capacity'])
        return 0
    if len(args) == 2 and args[0] == '--promote':
        reply = rpc_request('promote', args[1])
        return 0 if reply.get('ok') else 2
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--status', action='store_true', help='list requests; does not run a check')
    mode.add_argument('--recover', nargs='?', const='', metavar='REQUEST',
                      help='recover one explicit stopped request; does not run a check')
    options = parser.parse_args(args)
    if sys.platform not in ('darwin', 'linux'):
        say('Unsupported platform: this admission CLI requires macOS or Linux. No check was run.')
        return 2
    cancellation = [0]
    previous = {}
    for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        previous[sig] = signal.signal(sig, lambda number, frame: cancellation.__setitem__(0, number))
    manager_lock = None
    try:
        state = USER_HOME / '.local/state/marginplyr/review-ready'
        state.mkdir(parents=True, exist_ok=True, mode=0o700)
        if options.status:
            records = []
            table = processes()
            for path, record, error in active_records(state):
                if error:
                    records.append(dict(id=path.parent.name, state='recovery', error=error,
                                        capacity='unknown; Full protected', record=str(path)))
                else:
                    fields = ('id', 'sha', 'root', 'boot', 'manager', 'group', 'workspace',
                              'selection', 'state', 'capacity', 'owned_capacity', 'may_own_full',
                              'worker_policy', 'promotions', 'cleanup')
                    item = {key: record[key] for key in fields if key in record}
                    item.update(record=str(path), recorded_children=len(record['children']))
                    if record['version'] == 1:
                        item.setdefault('id', 'legacy')
                        item.setdefault('state', 'running')
                        item.setdefault('capacity', 'Full')
                        item.setdefault('selection', dict(path='Full'))
                        item['ownership'] = 'legacy fence; Full shutdown must be confirmed'
                    if not same_process(table, record['manager']):
                        item['state'] = 'recovery'
                    records.append(item)
            say(json.dumps(dict(requests=records, operation='status only; No check was run.'), indent=2))
            return 0
        if options.recover is not None:
            processes()
            return recover_request(state, options.recover or None, boot_identity())
        root, sha = identity()
        selection = select_path(root, sha)
        reason = eligibility(root, sha, selection)
        if reason:
            selection['path'] = 'Full'
            selection['roxygen_pending'] = False
            selection['reasons'].append(reason)
        say('Review-ready selection: ' + json.dumps(selection, ensure_ascii=True))
        if not shutil.which('Rscript'):
            raise RuntimeError('Rscript is missing from PATH. Install the review prerequisites; no check was run.')
        table = processes()
        boot = boot_identity()
        parent = os.getppid()
        request = uuid.uuid4().hex
        directory = state / 'requests' / request
        directory.mkdir(parents=True, mode=0o700)
        manager_lock = lock_file(directory / 'manager.lock')
        path = directory / 'record.json'
        record = dict(version=2, protocol=SUPERVISION_PROTOCOL, id=request, token=uuid.uuid4().hex,
                      sha=sha, root=str(root), boot=boot, manager=evidence(table[os.getpid()]),
                      group=None, children=[], started=time.time(), selection=selection,
                      initial_selection=dict(selection),
                      state='preparing', capacity='Full' if selection['path'] == 'Full' else 'exempt',
                      may_own_full=False, owned_capacity=False, epoch=1, authorized=False,
                      gate_started=False,
                      promotions=[], workspace=str(directory / 'workspace'), cleanup='pending',
                      channel='/tmp/marginplyr-review-' + request + '.sock', channel_owned=False,
                      worker_policy=('Full suite: 4 test workers' if selection['path'] == 'Full' else
                                     'serial; registered parallel fixtures <=2'),
                      requires_proof=EXEMPT_PROTOCOL in git_bytes(
                          root, 'show', sha + ':tools/review-ready-check.R').splitlines())
        save(path, record)
        say('Review-ready request: ' + request + '; management record: ' + str(path))
        return run_request(state, path, record, cancellation, parent)
    except (OSError, ValueError, KeyError, TypeError, RuntimeError, subprocess.SubprocessError) as error:
        say('Review-ready admission failed: ' + str(error) + '. No passing check evidence.')
        return 2
    finally:
        if manager_lock is not None:
            manager_lock.close()
        for sig, handler in previous.items():
            signal.signal(sig, handler)


if __name__ == '__main__':
    sys.exit(main())
