"""Registered stage evidence and follow-up contracts (ADR 0038).

Only the common supervisor issues sealed records, after terminal cleanup.
The local user's private state is the cooperative trust boundary, not a
defence against that user replacing the checker or its records deliberately.
"""
import hashlib
import hmac
import importlib.util
import json
import math
import os
from pathlib import Path
import platform
import re
import shutil
import stat
import subprocess
import sys
import sysconfig
import time

SCHEMA = 1
PROTOCOL = b'# Review-ready evidence protocol: 1'
EXPECTATION = '.github/scripts/verify-cran-preflight.R'
# Only this literal vector may vary. Everything around it, including the
# strict comparison helper and all other fixtures, remains byte-identical.
VECTOR = re.compile(
    rb'(expect_identical\(\s*review_requirements\$package,\s*)'
    rb'(c\(\s*"[A-Za-z][A-Za-z0-9.]*"(?:\s*,\s*"[A-Za-z][A-Za-z0-9.]*")*\s*\))'
    rb'(,\s*"the review dependency set"\s*\))')
# Each exclusion has an executable artifact route in the admission registry.
# All other tracked paths, including new paths and all of DESCRIPTION, are
# inputs. Package tarball exclusions are deliberately not used as a shortcut.
NON_INPUTS = (
    r'AGENTS\.md|CLAUDE\.md|CONTEXT\.md|CODE_OF_CONDUCT\.md',
    r'(design|investigation)/(?:[^/]+/)*[^/]+\.md|tools/[^/]+\.md',
    r'\.github/workflows/(R-CMD-check|altdoc|document|lint|postgres-transactions|release-matrix|rhub|test-coverage)\.yaml',
    r'\.github/scripts/summarize-check-log\.R',
    r'altdoc/quarto_website\.yml|codecov\.yml|\.codex/config\.toml|\.claude/settings\.json',
)
UNITS = ('build-full', 'build-documentation', 'generation', 'documentation-assertions')
CHECKER = ('4.6.1', '2efcaa5190c038e60834bd5c4e502c82', '34ee7dea5eac7ba7138bd3c60651faee')


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(',', ':'), ensure_ascii=True).encode()


def digest(value):
    return hashlib.sha256(canonical(value)).hexdigest()


def protected_expectation(source):
    matches = list(VECTOR.finditer(source))
    if len(matches) != 1:
        return None
    match = matches[0]
    return source[:match.start(2)] + b'<review dependency vector>' + source[match.end(2):]


def recognize_expectation(before, after):
    protected = protected_expectation(before)
    return protected is not None and protected == protected_expectation(after)


def git(root, *args):
    return subprocess.check_output(['git', '-C', str(root), *args], stderr=subprocess.PIPE)


def repository(root):
    remote = git(root, 'remote', 'get-url', 'origin').decode().strip()
    remote = re.sub(r'^git@github.com:', 'https://github.com/', remote)
    return remote.removesuffix('.git').rstrip('/')


def source_inputs(root, sha, unit):
    """Identify every tracked input, using only audited non-input exclusions."""
    if unit not in UNITS:
        raise ValueError('Unknown evidence unit: ' + unit)
    entries = []
    for entry in git(root, 'ls-tree', '-rz', sha).split(b'\0'):
        if not entry:
            continue
        metadata, name = entry.split(b'\t', 1)
        mode, kind, oid = metadata.decode().split()
        name = name.decode('utf-8')
        entries.append((name, mode, kind, oid))
    needed = sorted({oid for name, mode, kind, oid in entries if mode == '100644' and kind == 'blob' and
                     (name == EXPECTATION or any(re.fullmatch(pattern, name) for pattern in NON_INPUTS))})
    blobs = {}
    if needed:
        data = subprocess.run(['git', '-C', str(root), 'cat-file', '--batch'],
                              input=('\n'.join(needed) + '\n').encode(), stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE, check=True).stdout
        offset = 0
        for oid in needed:
            end = data.index(b'\n', offset)
            actual, kind, size = data[offset:end].split()
            if actual.decode() != oid or kind != b'blob':
                raise ValueError('Unavailable stage input blob.')
            offset = end + 1 + int(size)
            blobs[oid] = data[end+1:offset]
            if data[offset:offset+1] != b'\n':
                raise ValueError('Incomplete stage input blob.')
            offset += 1
    inputs = []
    for name, mode, kind, oid in entries:
        # Unsafe types participate even when their names would be non-inputs.
        ordinary = mode == '100644' and kind == 'blob'
        if ordinary and any(re.fullmatch(pattern, name) for pattern in NON_INPUTS):
            try:
                text = blobs[oid].decode('utf-8')
                if not any(ord(char) < 32 and char not in '\n\r\t' for char in text):
                    continue
            except UnicodeError:
                pass
        if ordinary and name == EXPECTATION:
            protected = protected_expectation(blobs[oid])
            if protected is not None:
                oid = hashlib.sha256(protected).hexdigest()
        inputs.append([name, mode, kind, oid])
    return digest(inputs)


def file_identity(path):
    """Capture content and change metadata, refusing unobserved file changes."""
    path = Path(path)
    before = path.stat()
    if not stat.S_ISREG(before.st_mode):
        raise ValueError('Unregistered external file type: ' + str(path))
    content = hashlib.sha256()
    with path.open('rb') as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            content.update(chunk)
    after = path.stat()
    fields = lambda info: [info.st_mode, info.st_size, info.st_mtime_ns, info.st_ctime_ns,
                           info.st_dev, info.st_ino]
    if fields(before) != fields(after):
        raise ValueError('External input changed while identifying it: ' + str(path))
    return [str(path), str(path.resolve()), fields(after), content.hexdigest()]


def native_file(path):
    with Path(path).open('rb') as stream:
        return stream.read(4) in (b'\x7fELF', b'\xcf\xfa\xed\xfe', b'\xfe\xed\xfa\xcf',
                                  b'\xce\xfa\xed\xfe', b'\xfe\xed\xfa\xce',
                                  b'\xca\xfe\xba\xbe', b'\xbe\xba\xfe\xca')


def directory_identity(root, natives=None):
    """Inventory installed contents, retaining symlink targets and empty dirs."""
    root = Path(root)
    seen = set()
    rows = []

    def visit(path, relative):
        resolved = path.resolve(strict=True)
        if resolved.is_dir():
            if resolved in seen:
                raise ValueError('Cyclic external directory: ' + str(path))
            seen.add(resolved)
            before = resolved.stat()
            names = sorted(path.iterdir())
            rows.append([relative, 'directory', str(resolved), stat.S_IMODE(before.st_mode)])
            for child in names:
                visit(child, relative + '/' + child.name)
            after = resolved.stat()
            if before.st_mtime_ns != after.st_mtime_ns or before.st_ctime_ns != after.st_ctime_ns:
                raise ValueError('External directory changed while identifying it: ' + str(path))
            seen.remove(resolved)
        else:
            rows.append([relative, file_identity(path)])
            if natives is not None and (path.suffix in ('.so', '.dylib') or os.access(path, os.X_OK)):
                if native_file(path):
                    natives.add(str(resolved))

    visit(root, '.')
    return digest(rows)


def linked_runtime(natives, roots, executables):
    """Inventory resolved native dependencies outside the registered roots.

    macOS's sealed system runtime is bound to its OS build manifest. Unknown
    loader-relative resolution is refused rather than guessed from a basename.
    """
    roots = [Path(root).resolve() for root in roots]
    seen = set()
    identities = []
    asset_roots = set()
    pending = set(natives)
    loader_paths = {}

    def rpaths(binary):
        if binary not in loader_paths:
            commands = subprocess.check_output(['/usr/bin/otool', '-l', binary], text=True)
            loader_paths[binary] = re.findall(r'cmd LC_RPATH\n.*?\n\s*path (.*?) \(offset', commands, re.S)
        return loader_paths[binary]
    if sys.platform == 'darwin':
        identities.append(file_identity('/System/Library/CoreServices/SystemVersion.plist'))
    while pending:
        batch = sorted(pending - seen)
        if not batch:
            break
        pending = set()
        seen.update(batch)
        dependencies = []
        if sys.platform == 'darwin':
            identifiers = {}
            current = None
            ids = subprocess.check_output(['/usr/bin/otool', '-D', *batch], text=True, stderr=subprocess.PIPE)
            for line in ids.splitlines():
                if line.endswith(':'):
                    current = line.split(' (architecture')[0].rstrip(':')
                elif line.strip() and current:
                    identifiers.setdefault(current, set()).add(line.strip())
            output = subprocess.check_output(['/usr/bin/otool', '-L', *batch], text=True, stderr=subprocess.PIPE)
            current = None
            for line in output.splitlines():
                if line and not line.startswith('\t'):
                    current = line.split(' (architecture')[0].rstrip(':')
                elif line.startswith('\t'):
                    name = line.strip().split(' (compatibility')[0]
                    if name == Path(current).name or name in identifiers.get(current, set()):
                        continue
                    if name.startswith('@loader_path/'):
                        name = str(Path(current).parent / name.removeprefix('@loader_path/'))
                    if name.startswith('@executable_path/'):
                        candidates = {str((Path(executable).parent / name.removeprefix('@executable_path/')).resolve())
                                      for executable in set(executables) | {current}
                                      if (Path(executable).parent / name.removeprefix('@executable_path/')).exists()}
                        if len(candidates) != 1:
                            raise ValueError('Unregistered executable-relative dependency: ' + name)
                        name = candidates.pop()
                    if name.startswith('@rpath/'):
                        candidates = set()
                        # A dependent library inherits runpaths from its load
                        # chain. The registered natives conservatively cover
                        # all such parents; ambiguous resolutions stay unknown.
                        for owners in (set(executables) | {current}, set(natives) | seen):
                            for owner in owners:
                                for rpath in rpaths(owner):
                                    rpath = rpath.replace('@loader_path', str(Path(owner).parent))
                                    for executable in executables:
                                        expanded = rpath.replace('@executable_path', str(Path(executable).parent))
                                        if expanded.startswith('/'):
                                            candidate = Path(expanded) / name.removeprefix('@rpath/')
                                            if candidate.exists():
                                                candidates.add(str(candidate.resolve()))
                            if candidates:
                                break
                        if len(candidates) != 1:
                            raise ValueError('Unregistered native loader resolution: ' + name)
                        name = candidates.pop()
                    if not name.startswith('/'):
                        raise ValueError('Unregistered native dependency: ' + name)
                    if name.startswith(('/usr/lib/', '/System/Library/')):
                        continue
                    dependencies.append(name)
        else:
            for current in batch:
                result = subprocess.run(['ldd', current], text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                if result.returncode and not re.search('not a dynamic executable|statically linked', result.stdout):
                    raise ValueError('Unavailable native linkage identity: ' + current)
                if 'not found' in result.stdout:
                    raise ValueError('Unresolved native dependency: ' + current)
                dependencies.extend(re.findall(r'(?:=>\s+|^\s*)(/\S+)', result.stdout, re.M))
        for dependency in sorted(set(dependencies)):
            try:
                path = Path(dependency).resolve(strict=True)
            except FileNotFoundError:
                # Installed optional modules may link an absent runtime (for
                # example R's X11 module on a headless host). Appearance is an
                # environment change, just as replacement of a present file is.
                identities.append([dependency, 'absent'])
                continue
            if not any(path == root or root in path.parents for root in roots) and str(path) not in seen:
                bottle = re.match(r'^(/(?:opt/homebrew|usr/local)/Cellar/[^/]+/[^/]+)(?:/|$)', str(path))
                if bottle:
                    asset_roots.add(bottle[1])
                identities.append(file_identity(path))
                if native_file(path):
                    pending.add(str(path))
    # Native dependencies can read installed data and plugins as well as other
    # linked libraries (for example PROJ's database). Bind the bottle's assets.
    identities.extend([root, directory_identity(root)] for root in sorted(asset_roots))
    return digest(identities)


def environment(root):
    """Observe the registered cooperative environment without running a gate.

    Custom startup/build hooks and unregistered external-library overrides
    have unknown input closures, so cannot issue or consume reusable evidence.
    """
    overrides = ('LD_LIBRARY_PATH', 'DYLD_LIBRARY_PATH', 'DYLD_FALLBACK_LIBRARY_PATH')
    if any(os.environ.get(name) for name in overrides):
        raise ValueError('Unregistered caller loader override.')
    output = subprocess.check_output(['Rscript', 'tools/review-ready-environment.R'],
                                     cwd=root, stderr=subprocess.PIPE, text=True)
    probe = json.loads(output)
    if probe['unregistered']:
        raise ValueError('Unregistered external inputs: ' + ', '.join(probe['unregistered']))
    files = []
    natives = set()
    executables = []
    for name in ('Rscript', 'R', 'git', 'jarl', 'quarto', 'actionlint'):
        command = shutil.which(name)
        if not command:
            raise ValueError('Unavailable environment identity: ' + name)
        files.append(file_identity(command))
        if native_file(command):
            natives.add(str(Path(command).resolve()))
            executables.append(str(Path(command).resolve()))
    files.append(file_identity(sys.executable))
    executables.append(str(Path(sys.executable).resolve()))
    natives.add(str(Path(sys.executable).resolve()))
    executables += probe.get('executables', [])
    python_inputs = set()
    prefix = Path(sys.base_prefix).resolve()
    python_roots = sorted({sysconfig.get_path('stdlib'), sysconfig.get_path('platstdlib')})
    # Cache prefixes may place stdlib bytecode outside its installation root.
    # Register that complete set, independent of which modules this process has
    # already imported when it reaches the observation.
    for library in python_roots:
        for source in Path(library).rglob('*.py'):
            cached = Path(importlib.util.cache_from_source(str(source)))
            if cached.is_file():
                python_inputs.add(str(cached.resolve()))
    for name, module in list(sys.modules.items()):
        if name in ('__main__', environment.__module__) or hasattr(module, 'LAUNCHER_SOURCE'):
            continue
        source = getattr(module, '__file__', None)
        if source and Path(source).is_file() and prefix not in Path(source).resolve().parents:
            raise ValueError('Unregistered Python startup/module input: ' + str(source))
    files.extend(file_identity(path) for path in sorted(python_inputs))
    directories = []
    for path in probe['libraries']:
        directories.append([path, directory_identity(path, natives)])
    # The installation roots include shared runtime assets as well as launchers.
    runtime_roots = probe['runtime_roots'] + python_roots
    for path in runtime_roots:
        directories.append([path, directory_identity(path, natives)])
    for path in probe['configuration']:
        files.append(file_identity(path) if Path(path).exists() else [path, 'absent'])
    # Only manager-owned transport and shell bookkeeping are normalized. Other
    # variables (including library, rendering, locale and compiler settings)
    # remain inputs, hashed without recording their potentially secret values.
    internal = {'MARGINPLYR_REVIEW_REQUEST', 'MARGINPLYR_REVIEW_SELECTION',
                'MARGINPLYR_REVIEW_RECORD', 'MARGINPLYR_REVIEW_COMMIT',
                'MARGINPLYR_REVIEW_EPOCH', 'MARGINPLYR_REVIEW_SUPERVISOR',
                'MARGINPLYR_REVIEW_STAGE_REPORT', 'MARGINPLYR_REVIEW_BASE'}
    bookkeeping = {'PWD', 'OLDPWD', 'SHLVL', '_'}
    values = {key: value for key, value in os.environ.items()
              if key not in internal | bookkeeping}
    values.update(TESTTHAT_PARALLEL='false', TESTTHAT_CPUS='1')
    return dict(digest=digest(dict(probe=probe, files=files, directories=directories,
                                  linkage=linked_runtime(natives, probe['libraries'] + runtime_roots, executables),
                                  variables=digest(values), platform=platform.uname()._asdict())),
                clock=probe['clock'], r=probe['r'], libraries=probe['libraries'])


def snapshot(root, sha, units):
    observed = environment(root)
    source = source_inputs(root, sha, units[0]) if units else None
    return dict(environment=observed,
                sources={unit: source for unit in units})


def clock_valid(clock, inventory, commit_time, now):
    """Reevaluate R's strict future boundary and reject clock rollback."""
    if ((clock.get('r'), clock.get('checker'), clock.get('timeout')) != CHECKER or
            not isinstance(inventory, dict) or not inventory.get('paths')):
        return False
    try:
        leeway = float(clock['leeway'])
        observed = float(inventory['observed'])
        times = list(inventory['paths'].values()) + [float(commit_time)]
        return (math.isfinite(leeway) and leeway > 0 and now >= observed and
                all(math.isfinite(value) and value <= now + leeway for value in times))
    except (KeyError, TypeError, ValueError):
        return False


def signing_key(state, create=False):
    key = state / 'evidence.key'
    if create and not key.exists():
        try:
            descriptor = os.open(key, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        except FileExistsError:
            pass
        else:
            with os.fdopen(descriptor, 'wb') as stream:
                stream.write(os.urandom(32))
                stream.flush()
                os.fsync(stream.fileno())
    if key.stat().st_uid != os.getuid() or stat.S_IMODE(key.stat().st_mode) != 0o600:
        raise ValueError('Unavailable private evidence key.')
    value = key.read_bytes()
    if len(value) != 32:
        raise ValueError('Invalid evidence key.')
    return value


def seal(payload, key):
    return dict(payload=payload, integrity=hmac.new(key, canonical(payload), hashlib.sha256).hexdigest())


def read_evidence(path, key, seen=None):
    seen = set() if seen is None else seen
    if path.parent.name in seen:
        raise ValueError('Cyclic evidence reference.')
    seen = seen | {path.parent.name}
    envelope = json.loads(path.read_text())
    payload = envelope['payload']
    if not hmac.compare_digest(envelope['integrity'], seal(payload, key)['integrity']):
        raise ValueError('Corrupt stage evidence.')
    record = json.loads(path.with_name('record.json').read_text())
    if (payload['schema'] != SCHEMA or payload['request'] != path.parent.name or
            type(payload.get('sequence')) is not int or payload['sequence'] < 1 or
            payload['user'] != os.getuid() or payload['request'] != record['id'] or
            payload['submitted'] != record['sha'] or payload['selection'] != record['selection'] or
            payload['supervisor'] != record['launcher'] or payload['finished'] != record['finished'] or
            record['state'] != 'terminal' or record['cleanup'] != 'confirmed' or
            record['outcome'] != payload['outcome'] or record['owned_capacity'] or
            record['version'] != 2 or record['protocol'] != 3 or not record['authorized']):
        raise ValueError('Unavailable terminal evidence provenance.')
    for wanted, reference in payload['reused'].items():
        if not re.fullmatch('[0-9a-f]{32}', reference['request']):
            raise ValueError('Invalid execution origin.')
        origin = read_evidence(path.parent.parent / reference['request'] / 'evidence.json', key, seen)
        if (origin['repository'] != payload['repository'] or origin['outcome'] != 0 or
                origin['submitted'] != reference['submitted'] or
                origin['units'][reference['unit']]['status'] != 'passed' or
                not compatible_unit(reference['unit'], wanted)):
            raise ValueError('Invalid inherited execution origin.')
    return payload


def required_units(selection):
    units = []
    if selection['path'] != 'Repository':
        units.append('build-full' if selection['path'] == 'Full' else 'build-documentation')
    if selection['path'] == 'Documentation':
        units.append('documentation-assertions')
    if 'tools/review-ready-generated.R' in selection['verifiers']:
        units.append('generation')
    return units


def compatible_unit(candidate, wanted):
    return candidate == wanted or (candidate == 'build-full' and wanted == 'build-documentation')


def read_index(state, key):
    path = state / 'evidence-index.json'
    if not path.exists():
        if list((state / 'requests').glob('*/evidence.json')):
            raise ValueError('Missing common evidence index; records cannot be imported.')
        return []
    envelope = json.loads(path.read_text())
    payload = envelope['payload']
    if (not hmac.compare_digest(envelope['integrity'], seal(payload, key)['integrity']) or
            payload['schema'] != SCHEMA or not isinstance(payload['entries'], list)):
        raise ValueError('Invalid common evidence index.')
    entries = payload['entries']
    requests = set()
    previous = 0
    for entry in entries:
        if (not re.fullmatch('[0-9a-f]{32}', entry['request']) or entry['request'] in requests or
                type(entry['sequence']) is not int or entry['sequence'] <= previous or
                not re.fullmatch('[0-9a-f]{64}', entry['digest'])):
            raise ValueError('Invalid evidence issuance ordering.')
        requests.add(entry['request'])
        previous = entry['sequence']
    return entries


def records(state, key):
    """Read the complete signed inventory, including negative outcomes."""
    entries = read_index(state, key)
    actual = {path.parent.name for path in (state / 'requests').glob('*/evidence.json')}
    if actual != {entry['request'] for entry in entries}:
        raise ValueError('Missing or unindexed common stage evidence.')
    found = []
    for entry in entries:
        payload = read_evidence(state / 'requests' / entry['request'] / 'evidence.json', key)
        if payload['sequence'] != entry['sequence'] or digest(payload) != entry['digest']:
            raise ValueError('Evidence differs from its issuance index.')
        found.append(payload)
    return list(reversed(found))


def append_evidence(state, path, payload, writer):
    """Serialize issuance, independently of wall clocks and Full admission."""
    import fcntl
    with (state / 'evidence.lock').open('a+') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        key = signing_key(state, create=True)
        records(state, key)
        entries = read_index(state, key)
        payload['sequence'] = entries[-1]['sequence'] + 1 if entries else 1
        writer(path, seal(payload, key))
        entries.append(dict(request=payload['request'], sequence=payload['sequence'], digest=digest(payload)))
        writer(state / 'evidence-index.json', seal(dict(schema=SCHEMA, entries=entries), key))


def plan(root, sha, selection, state, observe=snapshot, now=None, repository_id=None):
    """Keep cumulative obligations; select origins without moving the base."""
    units = required_units(selection)
    result = dict(schema=SCHEMA, units={unit: dict(action='execute') for unit in units},
                  reasons=[], snapshot=None)
    if not units or PROTOCOL not in git(root, 'show', sha + ':tools/review-ready-check.R').splitlines():
        return result
    try:
        key = signing_key(state)
        origins = records(state, key)
        repo = repository_id or repository(root)
        origins = [origin for origin in origins if origin['repository'] == repo]
        if not origins:
            return result
        source = source_inputs(root, sha, units[0])
        if not any(entry['status'] == 'passed' and entry['source'] == source and
                   any(compatible_unit(candidate, unit) for unit in units)
                   for origin in origins for candidate, entry in origin['units'].items()):
            result['reasons'].append('No origin matches the registered source inputs.')
            return result
        observed = observe(root, sha, units)
        result['snapshot'] = observed
        commit_time = int(git(root, 'show', '-s', '--format=%ct', sha))
        current_time = time.time() if now is None else now
        for unit in units:
            for origin in origins:
                if subprocess.run(['git', '-C', str(root), 'merge-base', '--is-ancestor',
                                   origin['submitted'], sha], stderr=subprocess.DEVNULL).returncode:
                    continue
                for candidate, entry in origin['units'].items():
                    if not compatible_unit(candidate, unit) or entry['status'] != 'passed':
                        continue
                    if (entry['source'] != observed['sources'][unit] or
                            entry['environment'] != observed['environment']['digest']):
                        continue
                    # Later same-input failures cannot be bypassed by selecting
                    # an older successful record, even from another branch.
                    blocked = any(
                        later['sequence'] >= origin['sequence'] and any(
                            compatible_unit(failed, unit) and item['status'] in ('failed', 'unavailable') and
                            item['source'] == entry['source'] and item['environment'] == entry['environment']
                            for failed, item in later['units'].items()) for later in origins)
                    if blocked or origin['outcome'] != 0:
                        continue
                    if unit.startswith('build-') and not clock_valid(
                            observed['environment']['clock'], entry.get('timestamps'), commit_time, current_time):
                        continue
                    if unit.startswith('build-') and not valid_check(entry.get('check'), candidate):
                        continue
                    result['units'][unit] = dict(action='reuse', request=origin['request'],
                                                unit=candidate, submitted=origin['submitted'])
                    break
                if result['units'][unit]['action'] == 'reuse':
                    break
    except (OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
        result['reasons'].append('Reuse unavailable: ' + str(error))
        result['units'] = {unit: dict(action='execute') for unit in units}
    return result


def valid_check(check, unit):
    """Require explicit terminal counts, arguments, NOTE and retry disposition."""
    if not isinstance(check, dict):
        return False
    args = ['--as-cran'] + (['--no-tests'] if unit == 'build-documentation' else [])
    attempts = check.get('attempts', [])
    if (check.get('args') != args or check.get('status') != 0 or check.get('timeout') is not False or
            check.get('errors') != 0 or check.get('warnings') != 0 or
            not isinstance(check.get('notes'), list) or
            any(note != 'allowed-note' for note in check['notes']) or
            len(attempts) not in (1, 2)):
        return False
    if len(attempts) == 2:
        first = attempts[0]
        if not (check.get('retry_verified') is True and check.get('first_failure') and
                check.get('retry_identity') and first.get('status') == 1 and
                first.get('timeout') is False and first.get('errors') == 1 and
                first.get('warnings') == 0):
            return False
    final = attempts[-1]
    return (final.get('status') == 0 and final.get('timeout') is False and
            final.get('errors') == 0 and final.get('warnings') == 0 and
            final.get('notes') == check['notes'])


def reused_units(selection):
    return [unit for unit, entry in selection.get('plan', {}).get('units', {}).items()
            if entry['action'] == 'reuse']


def valid_plan(selection):
    """Reject corrupt capacity metadata without granting an exempt workload."""
    if 'plan' not in selection:
        return True
    plan = selection['plan']
    try:
        if plan['schema'] != SCHEMA or set(plan['units']) != set(required_units(selection)):
            return False
        for unit, entry in plan['units'].items():
            if entry['action'] not in ('execute', 'reuse'):
                return False
            if entry['action'] == 'reuse' and not (
                    re.fullmatch('[0-9a-f]{32}', entry['request']) and
                    re.fullmatch('[0-9a-f]{40}', entry['submitted']) and
                    compatible_unit(entry['unit'], unit) and
                    re.fullmatch('[0-9a-f]{64}', plan['snapshot']['sources'][unit]) and
                    re.fullmatch('[0-9a-f]{64}', plan['snapshot']['environment']['digest'])):
                return False
        return True
    except (KeyError, AttributeError, TypeError):
        return False


def full_required(selection):
    return selection['path'] == 'Full' and 'build-full' not in reused_units(selection)


def revalidate(root, sha, selection, state, repository_id=None):
    previous = selection.get('plan')
    if not previous or not reused_units(selection):
        return True
    current = plan(root, sha, selection, state, repository_id=repository_id)
    return (previous['units'] == current['units'] and previous['snapshot'] == current['snapshot'])


def issue(state, record, worker):
    """Seal only observed executions after the supervisor confirms cleanup."""
    if (record['state'] != 'terminal' or record['cleanup'] != 'confirmed' or
            record['owned_capacity'] or not record['authorized']):
        return None
    before, after = worker.get('before'), worker.get('after')
    if not before or before != after:
        return None
    report = worker.get('report', {})
    if report.get('request') != record['token']:
        return None
    units = {}
    invalid_origin = any(unit.startswith('build-') and status == 'passed' and
                         not valid_check(report.get('check'), unit)
                         for unit, status in report.get('units', {}).items())
    for unit, status in report.get('units', {}).items():
        if (unit not in before['sources'] or status not in ('passed', 'running') or
                record['selection'].get('plan', {}).get('units', {}).get(unit, {}).get('action') != 'execute'):
            continue
        passed = status == 'passed'
        entry = dict(source=before['sources'][unit], environment=before['environment']['digest'],
                     environment_contract=before['environment'],
                     status='unavailable' if invalid_origin else 'passed' if passed else 'failed')
        if unit.startswith('build-'):
            entry.update(check=report.get('check'), timestamps=report.get('timestamps'))
        units[unit] = entry
    if record['outcome'] == 0 and not all(
            report.get('units', {}).get(unit) == 'passed'
            for unit, entry in record['selection'].get('plan', {}).get('units', {}).items()
            if entry['action'] == 'execute'):
        return None
    required = ['Package spelling', 'jarl', 'package-aware lintr']
    required += ['Artifact verification: ' + stage for stage in record['selection']['verifiers']
                 if stage != 'tools/review-ready-generated.R' or
                 'generation' not in reused_units(record['selection'])]
    if record['selection']['path'] != 'Repository':
        required += ['Test dependency syntax', 'Required test Suggests']
    if record['outcome'] == 0 and not set(required).issubset(report.get('completed', [])):
        return None
    return dict(schema=SCHEMA, request=record['id'], user=os.getuid(),
                repository=worker['repository'], submitted=record['sha'],
                selection=record['selection'], supervisor=record['launcher'],
                finished=record['finished'], outcome=record['outcome'],
                units=units, reused={unit: entry for unit, entry in
                                    record['selection'].get('plan', {}).get('units', {}).items()
                                    if entry['action'] == 'reuse'},
                completed=report.get('completed', []), omitted=report.get('omitted', []))
