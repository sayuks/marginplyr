"""Exercise the installed CLI lifecycle with real repositories and light jobs.

Rscript is a PATH fixture at the external checker boundary; no production
argument can change capacity or replace the gate.
"""
import json
import importlib.machinery
import os
import shutil
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest

SOURCE = Path(__file__).with_name('review-ready-admission.py')


class Lifecycle(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.home = self.root / 'home'
        self.home.mkdir()
        self.command = self.home / 'marginplyr-review-ready'
        shutil.copyfile(SOURCE, self.command)
        self.jobs = []
        self.logs = []
        self.env = {key: value for key, value in os.environ.items()
                    if key not in ('MARGINPLYR_REVIEW_REQUEST', 'MARGINPLYR_REVIEW_SELECTION')}
        self.bin = self.root / 'bin'
        self.bin.mkdir()
        shim = self.bin / 'Rscript'
        shim.write_text('#!' + sys.executable + '\n' + '''
import fcntl, json, os, pathlib, socket, subprocess, sys, time
if os.environ.get("MARGINPLYR_REVIEW_SUPERVISOR"):
    subprocess.run([sys.executable, os.environ["MARGINPLYR_REVIEW_SUPERVISOR"], "--gate-proof"], check=True)
if os.environ.get("FIXTURE_PROMOTE") and os.environ["MARGINPLYR_REVIEW_EPOCH"] == "1":
    child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"], start_new_session=True)
    pathlib.Path(os.environ["FIXTURE_CHILD_FILE"]).write_text(str(child.pid))
    print("PREPARING promotion", flush=True)
    subprocess.run([sys.executable, os.environ["MARGINPLYR_REVIEW_SUPERVISOR"], "--promote", "fixture requires Full"], check=True)
    time.sleep(60)
lock = open(os.environ["FIXTURE_LOCK"], "a+")
fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
root = pathlib.Path.cwd()
print("HEAVY " + str(root), flush=True)
child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"]) if os.environ.get("FIXTURE_CHILD") else None
if child:
    pathlib.Path(os.environ["FIXTURE_CHILD_FILE"]).write_text(str(child.pid))
time.sleep(float(os.environ.get("FIXTURE_SECONDS", "0.6")))
if child: child.wait()
print("DONE", flush=True)
sys.exit(int(os.environ.get("FIXTURE_STATUS", "0")))
''')
        shim.chmod(0o755)
        self.env['FIXTURE_LOCK'] = str(self.root / 'heavy.lock')
        self.env['FIXTURE_CHILD_FILE'] = str(self.root / 'child.pid')
        self.env['PATH'] = str(self.bin) + os.pathsep + self.env['PATH']
        self.repo = self.root / 'repo'
        self.repo.mkdir()
        self.git(self.repo, 'init', '-q')
        self.git(self.repo, 'config', 'user.email', 'fixture@example.com')
        self.git(self.repo, 'config', 'user.name', 'Fixture')
        (self.repo / 'DESCRIPTION').write_text('Package: marginplyr\n')
        (self.repo / 'tools').mkdir()
        (self.repo / 'tools/review-ready-check.R').write_text('# fixture\n')
        self.git(self.repo, 'add', '.')
        self.git(self.repo, 'commit', '-qm', 'fixture')

    def git(self, root, *args):
        return subprocess.check_output(['git', '-C', str(root), *args], stderr=subprocess.STDOUT, text=True).strip()

    def start(self, repo=None, args=(), adapter='', **env):
        log = self.root / ('log-' + str(len(self.logs)))
        self.logs.append(log)
        code = ('import importlib.machinery, pathlib, sys; '
                'm=importlib.machinery.SourceFileLoader("admission", sys.argv[1]).load_module(); '
                'm.USER_HOME=pathlib.Path(sys.argv[2]); ' + adapter +
                '\nsys.exit(m.main(sys.argv[3:]))')
        with log.open('w') as output:
            job = subprocess.Popen([sys.executable, '-c', code, str(self.command), str(self.home), *args],
                                   cwd=repo or self.repo, env=dict(self.env, **env), stdout=output, stderr=output)
        self.jobs.append(job)
        return job, log

    def until(self, log, text, timeout=10):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if text in log.read_text():
                return
            time.sleep(0.04)
        self.fail('Missing ' + text + ': ' + log.read_text())

    def finish(self, job, log, status=0):
        self.assertEqual(job.wait(timeout=15), status, log.read_text())
        return log.read_text()

    def recovery_args(self):
        journal = self.home / '.local/state/marginplyr/review-ready/request.json'
        return ('--recover', json.loads(journal.read_text()).get('id', 'legacy')) if journal.exists() else ('--recover',)

    def tearDown(self):
        for job in self.jobs:
            if job.poll() is None:
                job.terminate()
                job.wait(timeout=15)
        # A killed manager deliberately leaves its fixture child for recovery.
        for pidfile in self.root.rglob('child.pid'):
            try:
                os.kill(int(pidfile.read_text()), signal.SIGKILL)
            except ProcessLookupError:
                pass
        journal = self.home / '.local/state/marginplyr/review-ready/request.json'
        if journal.exists():
            record = json.loads(journal.read_text())
            if record.get('group'):
                try:
                    os.killpg(record['group'], signal.SIGKILL)
                except ProcessLookupError:
                    pass
        requests = self.home / '.local/state/marginplyr/review-ready/requests'
        for path in requests.glob('*/record.json'):
            record = json.loads(path.read_text())
            if record.get('group') and record.get('state') != 'terminal':
                try:
                    os.killpg(record['group'], signal.SIGKILL)
                except ProcessLookupError:
                    pass
            Path(record['channel']).unlink(missing_ok=True)
        self.temp.cleanup()

    def repository_branch(self):
        self.git(self.repo, 'checkout', '-qb', 'selection-support')
        entry = self.repo / 'tools/review-ready-check.R'
        # The external checker fixture receives the same installed capabilities
        # as real source. No production option can declare an arbitrary stage safe.
        source_root = SOURCE.parent.parent
        module = importlib.machinery.SourceFileLoader('fixture_installer', str(SOURCE)).load_module()
        for patterns in module.capability_patterns().values():
            for pattern in patterns:
                for path in source_root.glob(pattern):
                    destination = self.repo / path.relative_to(source_root)
                    destination.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copyfile(path, destination)
        self.git(self.repo, 'add', '.')
        self.git(self.repo, 'commit', '-qm', 'selection support')
        self.command.write_bytes(module.installed_source(self.repo))
        self.git(self.repo, 'branch', '-f', 'main', 'HEAD')
        self.git(self.repo, 'checkout', '-qb', 'prose')
        note = self.repo / 'design/note.md'
        note.parent.mkdir()
        note.write_text('Repository prose\n')
        self.git(self.repo, 'add', '.')
        self.git(self.repo, 'commit', '-qm', 'prose')

    def test_supported_prose_selects_repository_with_pinned_comparison(self):
        self.repository_branch()
        main = self.git(self.repo, 'rev-parse', 'main')
        job, log = self.start()
        output = self.finish(job, log)
        self.assertIn('"path": "Repository"', output)
        self.assertIn('"main": "' + main + '"', output)
        self.assertIn('"base": "' + main + '"', output)
        self.assertIn('verify-doc-references.R', output)

    def test_exempt_requests_start_while_full_and_another_full_waits(self):
        self.repository_branch()
        exempt = self.root / 'exempt'
        self.git(self.repo, 'worktree', 'add', '-qb', 'exempt', str(exempt))
        (self.repo / 'code.R').write_text('1\n')
        self.commit_all()
        full, a = self.start(FIXTURE_SECONDS='60')
        self.until(a, 'HEAVY ')
        second, b = self.start()
        self.until(b, 'Waiting')
        one, c = self.start(exempt, FIXTURE_LOCK=str(self.root / 'exempt-one.lock'))
        two, d = self.start(exempt, FIXTURE_LOCK=str(self.root / 'exempt-two.lock'))
        for log in (c, d):
            self.until(log, 'HEAVY ')
            self.assertNotIn('Waiting for Review-ready capacity', log.read_text())
        self.finish(one, c)
        self.finish(two, d)
        self.assertIsNone(full.poll())
        self.assertIsNone(second.poll())
        full.terminate()
        self.finish(full, a, 130)
        self.finish(second, b)

    def test_supervised_promotion_collects_preparation_and_retains_request(self):
        self.repository_branch()
        exempt = self.root / 'exempt'
        self.git(self.repo, 'worktree', 'add', '-qb', 'exempt', str(exempt))
        sha = self.git(exempt, 'rev-parse', 'HEAD')
        (self.repo / 'code.R').write_text('1\n')
        self.commit_all()
        full, a = self.start(FIXTURE_SECONDS='60')
        self.until(a, 'HEAVY ')
        promoted, b = self.start(exempt, FIXTURE_PROMOTE='1')
        self.until(b, 'Waiting for Review-ready capacity')
        self.assertNotIn('HEAVY ', b.read_text())
        child = int((self.root / 'child.pid').read_text())
        result = subprocess.run(['ps', '-p', str(child), '-o', 'stat='], capture_output=True, text=True)
        self.assertTrue(not result.stdout.strip() or result.stdout.strip().startswith('Z'))
        full.terminate()
        self.finish(full, a, 130)
        output = self.finish(promoted, b)
        request = next(line.split(';')[0].split(': ')[1] for line in output.splitlines()
                       if line.startswith('Review-ready request: '))
        self.assertIn('request: ' + request + '; checked commit: ' + sha, output)
        self.assertIn('selected path: Full; capacity: Full', output)
        self.assertIn('fixture requires Full', output)

    def test_changed_verifier_cannot_hide_an_unclassified_suite_outside_full(self):
        self.repository_branch()
        path = self.repo / '.github/scripts/verify-doc-references.R'
        path.write_text('testthat::test_local()\n')
        self.commit_all()
        self.git(self.repo, 'branch', '-f', 'main', 'HEAD')
        (self.repo / 'design/note.md').write_text('More prose\n')
        self.commit_all()
        selection = self.selected()
        self.assertEqual(selection['path'], 'Full')
        self.assertIn('unsupported stage source capability', ' '.join(selection['reasons']))

    def test_compatible_source_cannot_pass_without_live_gate_authorization(self):
        self.repository_branch()
        (self.bin / 'Rscript').write_text('#!/bin/sh\necho UNAUTHORIZED\nexit 0\n')
        job, log = self.start()
        output = self.finish(job, log, 2)
        self.assertIn('gate authorization', output)

    def test_full_recovery_does_not_block_exempt_and_recovery_is_explicit(self):
        self.repository_branch()
        exempt = self.root / 'exempt'
        self.git(self.repo, 'worktree', 'add', '-qb', 'exempt', str(exempt))
        (self.repo / 'code.R').write_text('1\n')
        self.commit_all()
        full, a = self.start(FIXTURE_SECONDS='60')
        self.until(a, 'HEAVY ')
        full.kill()
        full.wait(timeout=5)
        one, b = self.start(exempt, FIXTURE_LOCK=str(self.root / 'exempt.lock'))
        self.assertIn('capacity: exempt', self.finish(one, b))
        blocked, c = self.start()
        self.finish(blocked, c, 4)
        status, d = self.start(args=('--status',))
        records = json.loads(self.finish(status, d))['requests']
        self.assertEqual(len(records), 1)
        self.assertEqual(records[0]['state'], 'recovery')
        bare, e = self.start(args=('--recover',))
        self.assertIn('explicit request ID', self.finish(bare, e, 4))
        os.killpg(records[0]['group'], signal.SIGKILL)
        recovered, f = self.start(args=('--recover', records[0]['id']))
        self.finish(recovered, f)

    def test_exempt_recovery_and_cancellation_are_isolated_by_request(self):
        self.repository_branch()
        one, a = self.start(FIXTURE_SECONDS='60')
        self.until(a, 'HEAVY ')
        two, b = self.start(FIXTURE_SECONDS='60', FIXTURE_LOCK=str(self.root / 'other.lock'))
        self.until(b, 'HEAVY ')
        status, c = self.start(args=('--status',))
        records = json.loads(self.finish(status, c))['requests']
        self.assertEqual(len(records), 2)
        self.assertEqual(len({record['workspace'] for record in records}), 2)
        self.assertTrue(all(record['capacity'] == 'exempt' for record in records))
        self.assertTrue(all('recorded_children' in record and 'record' in record for record in records))
        self.assertTrue(all('token' not in record and 'children' not in record for record in records))
        one.kill()
        one.wait(timeout=5)
        target = next(record for record in records if record['manager']['pid'] == one.pid)
        ambiguous, d = self.start(args=('--recover',))
        text = self.finish(ambiguous, d, 4)
        for record in records:
            self.assertIn(record['id'], text)
        self.assertIsNone(two.poll())
        three, e = self.start(FIXTURE_LOCK=str(self.root / 'third.lock'))
        self.finish(three, e)
        os.killpg(target['group'], signal.SIGKILL)
        recovered, f = self.start(args=('--recover', target['id']))
        self.finish(recovered, f)
        self.assertIsNone(two.poll())
        two.terminate()
        self.finish(two, b, 130)

    def test_corrupt_duplicate_and_mismatched_ipc_cannot_authorize_full(self):
        self.repository_branch()
        shim = self.bin / 'Rscript'
        probe = '''
record = json.loads(pathlib.Path(os.environ["MARGINPLYR_REVIEW_RECORD"]).read_text())
message = dict(protocol=3, request=record["id"], token=record["token"], sha=record["sha"],
               epoch=record["epoch"], operation="promote", expected="running", reason="invalid probe")
probes = [b"corrupt\\n"]
for key, value in (("protocol", 2), ("request", "different"), ("token", "wrong"),
                   ("sha", "0" * 40), ("epoch", 0), ("expected", "waiting")):
    probes.append(json.dumps(dict(message, **{key: value})).encode() + b"\\n")
probes.append(json.dumps(dict(message, operation="authorize")).encode() + b"\\n")
for payload in probes:
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as channel:
        channel.connect(record["channel"])
        channel.sendall(payload)
        reply = json.loads(channel.recv(4096))
        assert reply["ok"] is False, reply
print("INVALID IPC REJECTED", flush=True)
'''
        shim.write_text(shim.read_text().replace('lock = open(', probe + '\nlock = open(', 1))
        job, log = self.start()
        output = self.finish(job, log)
        self.assertIn('INVALID IPC REJECTED', output)
        self.assertIn('selected path: Repository; capacity: exempt', output)
        self.assertIn('promotions: []', output)

    def test_simultaneous_promotions_use_one_full_slot(self):
        self.repository_branch()
        exempt = self.root / 'exempt'
        self.git(self.repo, 'worktree', 'add', '-qb', 'exempt', str(exempt))
        (self.repo / 'code.R').write_text('1\n')
        self.commit_all()
        full, a = self.start(FIXTURE_SECONDS='60')
        self.until(a, 'HEAVY ')
        promoted = []
        for index in range(2):
            directory = self.root / ('preparation-' + str(index))
            directory.mkdir()
            job, log = self.start(exempt, FIXTURE_PROMOTE='1',
                                  FIXTURE_CHILD_FILE=str(directory / 'child.pid'))
            self.until(log, 'Waiting for Review-ready capacity')
            promoted.append((job, log))
        full.terminate()
        self.finish(full, a, 130)
        for job, log in promoted:
            output = self.finish(job, log)
            self.assertIn('selected path: Full; capacity: Full', output)
            self.assertEqual(output.count('PREPARING promotion'), 1)

    def test_promoted_waiter_cancellation_and_stale_head_do_not_run_full(self):
        self.repository_branch()
        exempt = self.root / 'exempt'
        self.git(self.repo, 'worktree', 'add', '-qb', 'exempt', str(exempt))
        (self.repo / 'code.R').write_text('1\n')
        self.commit_all()
        full, a = self.start(FIXTURE_SECONDS='60')
        self.until(a, 'HEAVY ')
        cancelled, b = self.start(exempt, FIXTURE_PROMOTE='1')
        self.until(b, 'Waiting for Review-ready capacity')
        cancelled.terminate()
        self.assertNotIn('HEAVY ', self.finish(cancelled, b, 130))
        stale, c = self.start(exempt, FIXTURE_PROMOTE='1')
        self.until(c, 'Waiting for Review-ready capacity')
        self.git(exempt, 'commit', '--allow-empty', '-qm', 'stale promoted request')
        full.terminate()
        self.finish(full, a, 130)
        self.assertNotIn('HEAVY ', self.finish(stale, c, 3))

    def test_interrupted_initial_durable_write_protects_full_but_not_exempt(self):
        self.repository_branch()
        adapter = '''
replace=m.os.replace
def interrupted(source, target):
    if pathlib.Path(target).name == 'record.json':
        raise OSError('fixture interrupted durable write')
    return replace(source, target)
m.os.replace=interrupted
'''
        failed, a = self.start(adapter=adapter)
        self.assertNotIn('HEAVY ', self.finish(failed, a, 2))
        exempt, b = self.start()
        self.finish(exempt, b)
        (self.repo / 'code.R').write_text('1\n')
        self.commit_all()
        full, c = self.start()
        self.assertIn('Full recovery required', self.finish(full, c, 4))

    def test_full_holds_capacity_through_disposable_source_cleanup(self):
        adapter = '''
import time
remove=m.shutil.rmtree
def delayed(path, *args, **kwargs):
    print('CLEANUP STARTED', flush=True)
    time.sleep(1.5)
    return remove(path, *args, **kwargs)
m.shutil.rmtree=delayed
'''
        full, a = self.start(adapter=adapter)
        self.until(a, 'CLEANUP STARTED')
        waiter, b = self.start()
        self.until(b, 'Waiting')
        self.assertNotIn('HEAVY ', b.read_text())
        self.finish(full, a)
        self.finish(waiter, b)

    def test_reinstallation_does_not_replace_an_active_private_worker(self):
        self.repository_branch()
        real_git = shutil.which('git')
        release = self.root / 'release-clone'
        shim = self.bin / 'git'
        shim.write_text('#!' + sys.executable + '\nimport os,pathlib,sys,time\n' +
            'if "clone" in sys.argv:\n' +
            ' print("CLONE STARTED",flush=True)\n' +
            ' while not pathlib.Path(' + repr(str(release)) + ').exists(): time.sleep(0.02)\n' +
            'os.execv(' + repr(real_git) + ', [' + repr(real_git) + '] + sys.argv[1:])\n')
        shim.chmod(0o755)
        original = self.command.read_bytes()
        active, a = self.start()
        self.until(a, 'CLONE STARTED')
        self.command.write_text('raise RuntimeError("different launcher generation")\n')
        release.touch()
        self.assertIn('selected path: Repository; capacity: exempt', self.finish(active, a))
        self.command.write_bytes(original)
        following, b = self.start()
        self.finish(following, b)

    def test_missing_corrupt_or_replaced_exempt_transport_never_falls_back_to_full(self):
        rscript = shutil.which('Rscript')
        if not rscript:
            self.skipTest('Rscript is required for supervised selection integration')
        self.repository_branch()
        shim = self.bin / 'Rscript'
        shim.write_text('#!' + sys.executable + '\n' + '''
import os, pathlib, subprocess, sys
transport = pathlib.Path(os.environ['MARGINPLYR_REVIEW_SELECTION'])
mode = os.environ['FIXTURE_TRANSPORT']
if mode == 'missing':
    os.environ.pop('MARGINPLYR_REVIEW_SELECTION')
elif mode == 'corrupt':
    transport.write_text('invalid transport')
else:
    transport.write_text(transport.read_text().replace('Path: Repository', 'Path: Full').replace('Capacity: exempt', 'Capacity: Full'))
result = subprocess.run([''' + repr(rscript) + ''', '-e', "source('tools/review-ready-check-lib.R'); read_review_ready_selection()"])
sys.exit(result.returncode)
''')
        for mode in ('missing', 'corrupt', 'replaced'):
            with self.subTest(mode=mode):
                job, log = self.start(FIXTURE_TRANSPORT=mode)
                output = self.finish(job, log, 1)
                self.assertNotIn('HEAVY ', output)
                self.assertIn('selected path: Repository; capacity: exempt', output)

    def test_legacy_unfinished_journal_protects_full_and_leaves_exempt_independent(self):
        self.repository_branch()
        exempt = self.root / 'exempt'
        self.git(self.repo, 'worktree', 'add', '-qb', 'exempt', str(exempt))
        (self.repo / 'code.R').write_text('1\n')
        self.commit_all()
        journal = self.stopped_request()
        current = json.loads(journal.read_text())
        fields = ('version', 'token', 'sha', 'root', 'boot', 'manager', 'group', 'children')
        journal.write_text(json.dumps({key: current[key] for key in fields}))
        shutil.rmtree(journal.parent / 'requests')
        Path(current['channel']).unlink(missing_ok=True)
        full, a = self.start()
        self.assertIn('request legacy', self.finish(full, a, 4))
        one, b = self.start(exempt)
        self.finish(one, b)
        status, c = self.start(args=('--status',))
        records = json.loads(self.finish(status, c))['requests']
        self.assertEqual(records[0]['id'], 'legacy')
        self.assertEqual(records[0]['capacity'], 'Full')
        recovery, d = self.start(args=('--recover', 'legacy'))
        self.finish(recovery, d)

    def test_installed_bytes_verify_reproducibly_across_python_processes(self):
        self.repository_branch()
        installer = self.repo / 'tools/install-review-ready.py'
        shutil.copyfile(SOURCE.with_name('install-review-ready.py'), installer)
        destination = self.home / '.local/bin/marginplyr-review-ready'
        destination.parent.mkdir(parents=True)
        destination.write_bytes(self.command.read_bytes())
        code = ('import pwd,runpy,sys,types; fixture_home=sys.argv[1]; '
                'pwd.getpwuid=lambda uid:types.SimpleNamespace(pw_dir=fixture_home); '
                'sys.argv=[sys.argv[2],"--verify"]; runpy.run_path(sys.argv[0],run_name="__main__")')
        for _ in range(2):
            result = subprocess.run([sys.executable, '-c', code, str(self.home), str(installer)],
                                    cwd=self.repo, env=self.env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn('Verified installed supervision protocol: 3', result.stdout)

    def selected(self, **env):
        job, log = self.start(FIXTURE_SECONDS='0', **env)
        output = self.finish(job, log)
        line = next(line for line in output.splitlines() if line.startswith('Review-ready selection: '))
        return json.loads(line.removeprefix('Review-ready selection: '))

    def commit_all(self):
        self.git(self.repo, 'add', '-A')
        self.git(self.repo, 'commit', '-qm', 'fixture change')

    def test_supported_prose_categories_keep_their_contract_routes(self):
        self.repository_branch()
        for name in ('AGENTS.md', 'CLAUDE.md', 'CONTEXT.md', 'CODE_OF_CONDUCT.md',
                     'design/adr/decision.md', 'investigation/note.md',
                     'tools/cran-release.md', 'tools/review-ready-admission.md'):
            path = self.repo / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text('Prose\n')
        self.commit_all()
        selection = self.selected()
        self.assertEqual(selection['path'], 'Repository')
        self.assertIn('.github/scripts/verify-cran-release.R', selection['verifiers'])
        self.assertIn('.github/scripts/verify-review-ready-check.R', selection['verifiers'])
        self.assertIn('.github/scripts/verify-context-budget.R', selection['verifiers'])
        (self.repo / 'tools/review-ready-admission.py').write_text('# machinery\n')
        self.commit_all()
        selection = self.selected()
        self.assertEqual(selection['path'], 'Full')
        self.assertIn('tools/test-review-ready-admission.py', selection['verifiers'])

    def test_documentation_formats_and_mixed_full_keep_generation_routes(self):
        self.repository_branch()
        baseline = self.git(self.repo, 'rev-parse', 'HEAD')
        for name in ('README.md', 'README.qmd', 'NEWS.md', 'man/topic.Rd',
                     'man/figures/README-plot.png', 'man/figures/logo.svg',
                     'vignettes/guide.qmd',
                     'vignettes/plot.jpg', 'vignettes/nested/plot.png'):
            with self.subTest(name=name):
                self.git(self.repo, 'reset', '--hard', baseline)
                path = self.repo / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(b'\x89PNG\0image' if name.endswith('.png') else b'Documentation\n')
                self.commit_all()
                selection = self.selected()
                self.assertEqual(selection['path'], 'Documentation')
                if name != 'NEWS.md':
                    self.assertIn('tools/review-ready-generated.R', selection['verifiers'])
                (self.repo / 'code.R').write_text('1\n')
                self.commit_all()
                self.assertEqual(self.selected()['path'], 'Full')
                if name != 'NEWS.md':
                    self.assertIn('tools/review-ready-generated.R', self.selected()['verifiers'])

    def test_unscanned_vignette_document_formats_remain_full(self):
        self.repository_branch()
        baseline = self.git(self.repo, 'rev-parse', 'HEAD')
        for name in ('vignettes/guide.Rmd', 'vignettes/guide.md', 'vignettes/nested/guide.qmd'):
            with self.subTest(name=name):
                self.git(self.repo, 'reset', '--hard', baseline)
                path = self.repo / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text('Unscanned documentation input\n')
                self.commit_all()
                self.assertEqual(self.selected()['path'], 'Full')

    def test_documentation_deletion_rename_and_reverted_code(self):
        self.repository_branch()
        (self.repo / 'NEWS.md').write_text('Release\n')
        self.commit_all()
        self.git(self.repo, 'branch', '-f', 'main', 'HEAD')
        self.git(self.repo, 'mv', 'NEWS.md', 'README.md')
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Documentation')
        code = self.repo / 'code.R'
        code.write_text('1\n')
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Full')
        code.unlink()
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Documentation')
        (self.repo / 'README.md').unlink()
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Documentation')

    def test_binary_or_unknown_encoding_markdown_is_full(self):
        self.repository_branch()
        for contents in (b'binary\0payload', b'unknown\xffencoding', b'control\x01character'):
            with self.subTest(contents=contents):
                (self.repo / 'design/note.md').write_bytes(contents)
                self.commit_all()
                selection = self.selected()
                self.assertEqual(selection['path'], 'Full')
                self.assertIn('unsupported prose content', ' '.join(selection['reasons']))

    def test_documentation_text_requires_utf8_and_protocol_two(self):
        self.repository_branch()
        readme = self.repo / 'README.md'
        readme.write_bytes(b'bad\x00text')
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Full')
        readme.write_text('Documentation\n')
        (self.repo / 'tools/review-ready-check.R').write_text('# Review-ready selection protocol: 1\n')
        self.commit_all()
        self.assertIn('lacks selection protocol', ' '.join(self.selected()['reasons']))

    def test_comparison_errors_and_unclassifiable_records_fall_back_to_full(self):
        self.repository_branch()
        real_git = shutil.which('git')
        shim = self.bin / 'git'
        for raw in (None, b'not a raw record\0', b':100644 100644 invalid\0name\0'):
            with self.subTest(raw=raw):
                code = '#!' + sys.executable + '\nimport os,sys\n'
                code += 'if "diff" in sys.argv:\n'
                if raw is None:
                    code += ' sys.exit(9)\n'
                else:
                    code += ' os.write(1, ' + repr(raw) + '); sys.exit(0)\n'
                code += 'os.execv(' + repr(real_git) + ', [' + repr(real_git) + '] + sys.argv[1:])\n'
                shim.write_text(code)
                shim.chmod(0o755)
                selection = self.selected()
                self.assertEqual(selection['path'], 'Full')
                self.assertIn('comparison unavailable', selection['reasons'][0])

    def test_cumulative_tree_includes_earlier_code_but_ignores_reverted_code(self):
        self.repository_branch()
        code = self.repo / 'code.R'
        code.write_text('1\n')
        self.commit_all()
        (self.repo / 'design/note.md').write_text('More prose\n')
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Full')
        code.unlink()
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Repository')

    def test_deletions_renames_and_nul_delimited_paths(self):
        self.repository_branch()
        main = self.git(self.repo, 'rev-parse', 'main')
        for name in ('design/a space.md', 'design/a\ttab.md', 'design/a\nnewline.md', 'design/日本語.md'):
            with self.subTest(name=name):
                self.git(self.repo, 'reset', '--hard', main)
                (self.repo / 'design').mkdir(exist_ok=True)
                (self.repo / name).write_text('Prose\n')
                self.commit_all()
                self.assertEqual(self.selected()['path'], 'Repository')
        self.git(self.repo, 'reset', '--hard', main)
        (self.repo / 'design').mkdir(exist_ok=True)
        (self.repo / 'design/a.md').write_text('Old prose\n')
        self.commit_all()
        self.git(self.repo, 'branch', '-f', 'main', 'HEAD')
        self.git(self.repo, 'mv', 'design/a.md', 'design/b.md')
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Repository')
        self.git(self.repo, 'mv', 'design/b.md', 'code.R')
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Full')
        self.git(self.repo, 'reset', '--hard', 'main')
        (self.repo / 'design/a.md').unlink()
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Repository')
        # An unsupported old name must not disappear behind a supported new name.
        self.git(self.repo, 'reset', '--hard', 'main')
        self.git(self.repo, 'mv', 'design/a.md', 'code.R')
        self.commit_all()
        self.git(self.repo, 'branch', '-f', 'main', 'HEAD')
        self.git(self.repo, 'mv', 'code.R', 'design/a.md')
        self.commit_all()
        self.assertEqual(self.selected()['path'], 'Full')

    def test_unknown_executable_documentation_ci_modes_links_and_submodules_are_full(self):
        self.repository_branch()
        baseline = self.git(self.repo, 'rev-parse', 'HEAD')
        for name in ('design/script.R', 'investigation/script.py', 'tools/unknown.py',
                     'man/unknown.R', 'man/figures/payload.exe', 'vignettes/unknown.py',
                     'R/help.R', 'DESCRIPTION', 'LICENSE.md', '.github/job.yaml'):
            with self.subTest(name=name):
                self.git(self.repo, 'reset', '--hard', baseline)
                path = self.repo / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text('Package: marginplyr\nTitle: Changed\n' if name == 'DESCRIPTION' else 'unknown\n')
                self.commit_all()
                self.assertEqual(self.selected()['path'], 'Full')
        self.git(self.repo, 'reset', '--hard', baseline)
        note = self.repo / 'design/note.md'
        note.chmod(0o755)
        self.commit_all()
        self.assertIn('mode/type', ' '.join(self.selected()['reasons']))
        self.git(self.repo, 'reset', '--hard', baseline)
        note.unlink()
        note.symlink_to('../DESCRIPTION')
        self.commit_all()
        self.assertIn('mode/type', ' '.join(self.selected()['reasons']))
        self.git(self.repo, 'reset', '--hard', baseline)
        self.git(self.repo, 'update-index', '--add', '--cacheinfo',
                 '160000,' + baseline + ',design/module.md')
        self.git(self.repo, 'commit', '-qm', 'gitlink')
        (self.repo / 'design/module.md').mkdir()
        self.assertIn('mode/type', ' '.join(self.selected()['reasons']))

    def test_missing_unrelated_ambiguous_bases_empty_diff_and_old_revision_are_full(self):
        self.repository_branch()
        main = self.git(self.repo, 'rev-parse', 'main')
        self.git(self.repo, 'branch', '-D', 'main')
        self.assertIn('comparison unavailable', self.selected()['reasons'][0])
        self.git(self.repo, 'branch', 'main', 'HEAD')
        self.assertIn('empty cumulative diff', self.selected()['reasons'][0])
        self.git(self.repo, 'branch', '-f', 'main', main)
        entry = self.repo / 'tools/review-ready-check.R'
        entry.write_text('# old gate\n')
        self.commit_all()
        selected = self.selected()
        self.assertEqual(selected['path'], 'Full')
        self.assertIn('lacks selection protocol', ' '.join(selected['reasons']))
        # Two commits with the same pair of independent parents have two bases.
        tree = self.git(self.repo, 'rev-parse', 'HEAD^{tree}')
        left = self.git(self.repo, 'commit-tree', tree, '-p', main, '-m', 'left')
        right = self.git(self.repo, 'commit-tree', tree, '-p', main, '-m', 'right')
        first = self.git(self.repo, 'commit-tree', tree, '-p', left, '-p', right, '-m', 'first')
        second = self.git(self.repo, 'commit-tree', tree, '-p', right, '-p', left, '-m', 'second')
        self.git(self.repo, 'reset', '--hard', first)
        self.git(self.repo, 'branch', '-f', 'main', second)
        selected = self.selected()
        self.assertEqual(selected['path'], 'Full')
        self.assertIn('ambiguous merge-base', selected['reasons'][0])
        unrelated = self.git(self.repo, 'commit-tree', tree, '-m', 'unrelated')
        self.git(self.repo, 'branch', '-f', 'main', unrelated)
        self.assertEqual(self.selected()['path'], 'Full')

    def test_later_main_does_not_replace_pinned_exempt_comparison(self):
        self.repository_branch()
        main = self.git(self.repo, 'rev-parse', 'main')
        active, a = self.start(FIXTURE_SECONDS='2')
        self.until(a, 'HEAVY ')
        waiter, b = self.start(FIXTURE_LOCK=str(self.root / 'other.lock'))
        self.until(b, 'HEAVY ')
        self.assertIn('"main": "' + main + '"', b.read_text())
        self.git(self.repo, 'branch', '-f', 'main', 'HEAD')
        self.finish(active, a)
        output = self.finish(waiter, b)
        self.assertIn('"path": "Repository"', output)
        self.assertIn('"main": "' + main + '"', output)

    def test_caller_cannot_select_repository_or_skip_verifiers(self):
        job, log = self.start(MARGINPLYR_REVIEW_SELECTION='/untrusted/skip')
        output = self.finish(job, log)
        self.assertIn('"path": "Full"', output)
        for arg in ('--repository', '--skip-check', '--path=Repository'):
            job, log = self.start(args=(arg,))
            self.assertNotIn('HEAVY ', self.finish(job, log, 2))

    def test_worktrees_and_clones_share_one_capacity(self):
        worktree = self.root / 'worktree'
        clone = self.root / 'clone'
        self.git(self.repo, 'worktree', 'add', '-qb', 'other', str(worktree))
        self.git(self.repo, 'clone', '-q', str(self.repo), str(clone))
        first, a = self.start(FIXTURE_SECONDS='2')
        self.until(a, 'HEAVY ')
        second, b = self.start(worktree)
        third, c = self.start(clone)
        for log in (b, c):
            self.until(log, 'Waiting')
            self.assertNotIn('HEAVY ', log.read_text())
            self.assertIn(self.git(self.repo, 'rev-parse', 'HEAD'), log.read_text())
        self.finish(first, a)
        self.finish(second, b)
        self.finish(third, c)
        # Each heavy fixture takes an independent nonblocking OS lock too.
        # Its failure exits nonzero, so all-success proves no overlap.

    def test_waiting_requests_cancel_when_head_changes_or_tree_gets_dirty(self):
        for change in ('head', 'dirty'):
            with self.subTest(change=change):
                active, a = self.start(FIXTURE_SECONDS='2')
                self.until(a, 'HEAVY ')
                waiter, b = self.start()
                self.until(b, 'Waiting')
                if change == 'head':
                    self.git(self.repo, 'commit', '--allow-empty', '-qm', 'new head')
                else:
                    (self.repo / 'untracked').write_text('new edit')
                self.finish(active, a)
                text = self.finish(waiter, b, 3)
                self.assertIn('Stale request cancelled', text)
                self.assertNotIn('HEAVY ', text)
                if change == 'dirty':
                    (self.repo / 'untracked').unlink()

    def test_waiter_cancellation_does_not_stop_active_check(self):
        active, a = self.start(FIXTURE_SECONDS='2')
        self.until(a, 'HEAVY ')
        waiter, b = self.start()
        self.until(b, 'Waiting')
        waiter.send_signal(signal.SIGINT)
        self.finish(waiter, b, 130)
        self.assertIsNone(active.poll())
        self.assertNotIn('HEAVY ', b.read_text())
        self.finish(active, a)

    def test_running_cancellation_stops_children_before_next_admission(self):
        active, a = self.start(FIXTURE_CHILD='1', FIXTURE_SECONDS='60')
        self.until(a, 'HEAVY ')
        deadline = time.monotonic() + 5
        while not (self.root / 'child.pid').exists() and time.monotonic() < deadline:
            time.sleep(0.04)
        child = int((self.root / 'child.pid').read_text())
        waiter, b = self.start()
        self.until(b, 'Waiting')
        active.terminate()
        self.finish(active, a, 130)
        self.finish(waiter, b)
        # A zombie has stopped and cannot hold the fixture's independent lock.
        state = subprocess.run(['ps', '-p', str(child), '-o', 'stat='], capture_output=True, text=True)
        self.assertTrue(not state.stdout.strip() or state.stdout.strip().startswith('Z'))

    def test_killed_manager_leaves_live_work_and_blocks_admission(self):
        active, a = self.start(FIXTURE_CHILD='1', FIXTURE_SECONDS='60')
        self.until(a, 'HEAVY ')
        active.kill()
        active.wait(timeout=5)
        next_job, b = self.start()
        text = self.finish(next_job, b, 4)
        self.assertIn('Admission blocked', text)
        self.assertNotIn('HEAVY ', text)
        recovery, c = self.start(args=self.recovery_args())
        self.finish(recovery, c, 4)
        # Stop the fixture through its own recorded manager-independent group.
        status, d = self.start(args=('--status',))
        self.finish(status, d)
        record = json.loads(d.read_text())['requests'][0]
        os.killpg(record['group'], signal.SIGKILL)
        time.sleep(0.2)
        stale, e = self.start(args=self.recovery_args())
        recovered = self.finish(stale, e)
        self.assertIn('No check was run', recovered)
        self.assertNotIn('HEAVY ', recovered)
        passing, f = self.start()
        self.finish(passing, f)

    def stopped_request(self):
        active, log = self.start(FIXTURE_SECONDS='60')
        self.until(log, 'HEAVY ')
        active.kill()
        active.wait(timeout=5)
        journal = self.home / '.local/state/marginplyr/review-ready/request.json'
        record = json.loads(journal.read_text())
        os.killpg(record['group'], signal.SIGKILL)
        time.sleep(0.2)
        return journal

    def test_recovery_rechecks_ownership_before_releasing_capacity(self):
        journal = self.stopped_request()
        original = journal.read_bytes()
        real_ps = shutil.which('ps')
        shim = self.bin / 'ps'
        count = self.root / 'ps-count'
        shim.write_text('#!' + sys.executable + '\n' +
            'import json,os,pathlib,subprocess,sys\n' +
            'counter=pathlib.Path(' + repr(str(count)) + ')\n' +
            'number=int(counter.read_text())+1 if counter.exists() else 1\n' +
            'counter.write_text(str(number))\n' +
            'snapshot=subprocess.check_output([' + repr(real_ps) + ']+sys.argv[1:])\n' +
            'if number == 2:\n' +
            ' record=json.loads(pathlib.Path(' + repr(str(journal)) + ').read_text())\n' +
            ' env=dict(os.environ,MARGINPLYR_REVIEW_REQUEST=record["token"])\n' +
            ' child=subprocess.Popen([sys.executable,"-c","import time; time.sleep(60)"],env=env,start_new_session=True,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)\n' +
            ' pathlib.Path(os.environ["FIXTURE_CHILD_FILE"]).write_text(str(child.pid))\n' +
            'sys.stdout.buffer.write(snapshot)\n')
        shim.chmod(0o755)
        recovery, log = self.start(args=self.recovery_args())
        self.assertIn('Admission blocked', self.finish(recovery, log, 4))
        self.assertEqual(journal.read_bytes(), original)
        self.assertNotIn('HEAVY ', log.read_text())
        shim.unlink()
        child = int((self.root / 'child.pid').read_text())
        os.kill(child, signal.SIGKILL)
        recovery, log = self.start(args=self.recovery_args())
        self.assertIn('No check was run', self.finish(recovery, log))

    def test_recovery_preserves_journal_if_final_inspection_fails(self):
        journal = self.stopped_request()
        original = journal.read_bytes()
        real_ps = shutil.which('ps')
        shim = self.bin / 'ps'
        count = self.root / 'ps-count'
        shim.write_text('#!' + sys.executable + '\n' +
            'import os,pathlib,sys\n' +
            'counter=pathlib.Path(' + repr(str(count)) + ')\n' +
            'number=int(counter.read_text())+1 if counter.exists() else 1\n' +
            'counter.write_text(str(number))\n' +
            'if number == 3: sys.exit(1)\n' +
            'os.execv(' + repr(real_ps) + ',[' + repr(real_ps) + ']+sys.argv[1:])\n')
        shim.chmod(0o755)
        recovery, log = self.start(args=self.recovery_args())
        self.assertIn('admission failed', self.finish(recovery, log, 2))
        self.assertEqual(journal.read_bytes(), original)
        shim.unlink()
        recovery, log = self.start(args=self.recovery_args())
        self.assertIn('No check was run', self.finish(recovery, log))

    def test_recovery_tracks_recorded_detached_child_without_marker(self):
        shim = self.bin / 'Rscript'
        shim.write_text('#!' + sys.executable + '\n' +
            'import os,pathlib,subprocess,sys,time\n' +
            'env=dict(os.environ); env.pop("MARGINPLYR_REVIEW_REQUEST",None)\n' +
            'child=subprocess.Popen([sys.executable,"-c","import time; time.sleep(60)"],env=env,start_new_session=True)\n' +
            'pathlib.Path(os.environ["FIXTURE_CHILD_FILE"]).write_text(str(child.pid))\n' +
            'print("HEAVY detached without marker",flush=True)\n' +
            'time.sleep(60)\n')
        shim.chmod(0o755)
        active, log = self.start()
        self.until(log, 'HEAVY ')
        child = int((self.root / 'child.pid').read_text())
        journal = self.home / '.local/state/marginplyr/review-ready/request.json'
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            record = json.loads(journal.read_text())
            if any(item['pid'] == child for item in record['children']):
                break
            time.sleep(0.04)
        else:
            self.fail('The manager did not record its detached descendant')
        active.kill()
        active.wait(timeout=5)
        os.killpg(record['group'], signal.SIGKILL)
        recovery, log = self.start(args=self.recovery_args())
        self.finish(recovery, log, 4)
        os.kill(child, signal.SIGKILL)
        recovery, log = self.start(args=self.recovery_args())
        self.assertIn('No check was run', self.finish(recovery, log))

    def test_recovery_does_not_confuse_reused_pid_with_owned_work(self):
        journal = self.stopped_request()
        unrelated = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)'])
        try:
            record = json.loads(journal.read_text())
            record['manager'] = dict(pid=unrelated.pid, birth='different birth identity')
            record['children'].append(dict(pid=unrelated.pid, birth='different birth identity'))
            journal.write_text(json.dumps(record))
            recovery, log = self.start(args=self.recovery_args())
            self.assertIn('No check was run', self.finish(recovery, log))
            self.assertIsNone(unrelated.poll())
        finally:
            unrelated.terminate()
            unrelated.wait(timeout=5)

    def test_recovery_refuses_a_live_recorded_manager_without_signalling_it(self):
        journal = self.stopped_request()
        unrelated = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)'])
        try:
            birth = subprocess.check_output(['ps', '-p', str(unrelated.pid), '-o', 'lstart='],
                text=True, env=dict(os.environ, LC_ALL='C'))
            record = json.loads(journal.read_text())
            record['manager'] = dict(pid=unrelated.pid, birth=' '.join(birth.split()))
            journal.write_text(json.dumps(record))
            recovery, log = self.start(args=self.recovery_args())
            self.finish(recovery, log, 4)
            self.assertTrue(journal.exists())
            self.assertIsNone(unrelated.poll())
        finally:
            unrelated.terminate()
            unrelated.wait(timeout=5)

    def test_recovery_refuses_malformed_ownership_metadata(self):
        journal = self.stopped_request()
        original = json.loads(journal.read_text())
        for field, value in (('manager', None), ('children', [None]),
                             ('boot', 'invalid-boot'), ('group', -1),
                             ('token', ''), ('sha', 'invalid-sha'),
                             ('root', 'relative/path'), ('version', True)):
            with self.subTest(field=field):
                record = dict(original, **{field: value})
                journal.write_text(json.dumps(record))
                before = journal.read_bytes()
                recovery, log = self.start(args=self.recovery_args())
                self.finish(recovery, log, 2)
                self.assertEqual(journal.read_bytes(), before)
                self.assertNotIn('No check was run', log.read_text())

    def test_recovery_refuses_empty_successful_observations_with_live_work(self):
        active, log = self.start(FIXTURE_SECONDS='60')
        self.until(log, 'HEAVY ')
        active.kill()
        active.wait(timeout=5)
        journal = self.home / '.local/state/marginplyr/review-ready/request.json'
        original = journal.read_bytes()
        record = json.loads(original)
        real_ps = shutil.which('ps')
        shim = self.bin / 'ps'
        count = self.root / 'ps-count'
        shim.write_text('#!' + sys.executable + '\n' +
            'import os,pathlib,sys\n' +
            'counter=pathlib.Path(' + repr(str(count)) + ')\n' +
            'number=int(counter.read_text())+1 if counter.exists() else 1\n' +
            'counter.write_text(str(number))\n' +
            'if number > 1: sys.exit(0)\n' +
            'os.execv(' + repr(real_ps) + ',[' + repr(real_ps) + ']+sys.argv[1:])\n')
        shim.chmod(0o755)
        try:
            recovery, log = self.start(args=self.recovery_args())
            self.assertIn('complete process ownership evidence', self.finish(recovery, log, 2))
            self.assertEqual(journal.read_bytes(), original)
            self.assertNotIn('No check was run', log.read_text())
        finally:
            shim.unlink()
            os.killpg(record['group'], signal.SIGKILL)

    def test_terminal_failure_is_not_admission_success(self):
        failure, a = self.start(FIXTURE_STATUS='17')
        text = self.finish(failure, a, 17)
        self.assertIn('terminal outcome: 17', text)
        self.assertIn(self.git(self.repo, 'rev-parse', 'HEAD'), text)
        passing, b = self.start()
        self.finish(passing, b)

    def test_normal_parent_exit_retains_capacity_until_detached_child_stops(self):
        shim = self.bin / 'Rscript'
        shim.write_text('#!' + sys.executable + "\n" +
                        'import os, subprocess, sys\n' +
                        'subprocess.Popen([sys.executable, "-c", "import time; time.sleep(2)"], start_new_session=True)\n' +
                        'print("HEAVY detached", flush=True)\n')
        active, a = self.start()
        self.until(a, 'HEAVY ')
        waiter, b = self.start()
        self.until(b, 'Waiting')
        self.assertIsNone(active.poll())
        self.finish(active, a)
        self.finish(waiter, b)

    def test_profiles_cannot_partition_capacity_and_edits_after_admission_do_not_change_sha(self):
        sha = self.git(self.repo, 'rev-parse', 'HEAD')
        active, a = self.start(FIXTURE_SECONDS='2', HOME=str(self.root / 'profile-a'))
        self.until(a, 'HEAVY ')
        self.git(self.repo, 'commit', '--allow-empty', '-qm', 'later commit')
        waiter, b = self.start(HOME=str(self.root / 'profile-b'), XDG_STATE_HOME=str(self.root / 'state-b'))
        self.until(b, 'Waiting')
        self.assertNotIn('HEAVY ', b.read_text())
        self.assertIn('checked commit: ' + sha, self.finish(active, a))
        self.finish(waiter, b)

    def test_missing_process_visibility_fails_without_fallback(self):
        shim = self.bin / 'ps'
        shim.write_text('#!/bin/sh\nexit 1\n')
        shim.chmod(0o755)
        job, log = self.start()
        text = self.finish(job, log, 2)
        self.assertIn('admission failed', text)
        self.assertNotIn('HEAVY ', text)

    def test_inspection_failure_during_work_blocks_following_requests(self):
        job, log = self.start(FIXTURE_SECONDS='60')
        self.until(log, 'HEAVY ')
        shim = self.bin / 'ps'
        shim.write_text('#!/bin/sh\nexit 1\n')
        shim.chmod(0o755)
        self.assertIn('shutdown is unconfirmed', self.finish(job, log, 2))
        shim.unlink()
        next_job, next_log = self.start()
        self.finish(next_job, next_log, 4)
        self.assertNotIn('HEAVY ', next_log.read_text())

    def test_status_and_empty_recovery_are_not_check_evidence(self):
        for mode in ('--status', '--recover'):
            job, log = self.start(args=(mode,))
            self.assertIn('No check was run', self.finish(job, log))
            self.assertNotIn('HEAVY ', log.read_text())

    def test_current_r_entry_delegates_and_preserves_status(self):
        real_rscript = shutil.which('Rscript')
        if not real_rscript:
            self.skipTest('Rscript is needed only for the entry-point integration fixture')
        shim = self.bin / 'marginplyr-review-ready'
        shim.write_text('#!/bin/sh\necho COMMON_ENTRY\nexit 17\n')
        shim.chmod(0o755)
        entry = SOURCE.with_name('review-ready-check.R')
        result = subprocess.run([real_rscript, str(entry)], cwd=self.repo, env=self.env,
                                capture_output=True, text=True)
        self.assertEqual(result.returncode, 17, result.stderr)
        self.assertIn('COMMON_ENTRY', result.stdout)
        self.assertNotIn('HEAVY ', result.stdout)

    def test_management_parent_termination_cancels_owned_work(self):
        # The execution backend is a real parent, separate from the CLI manager.
        log = self.root / 'backend.log'
        pidfile = self.root / 'manager.pid'
        code = ('import importlib.machinery, pathlib, sys; '
                'm=importlib.machinery.SourceFileLoader("admission", sys.argv[1]).load_module(); '
                'm.USER_HOME=pathlib.Path(sys.argv[2]); sys.exit(m.main([]))')
        command = [sys.executable, '-c', code, str(self.command), str(self.home)]
        backend_code = ('import pathlib, subprocess, sys, time; '
                        'p=subprocess.Popen(sys.argv[2:]); '
                        'pathlib.Path(sys.argv[1]).write_text(str(p.pid)); time.sleep(60)')
        with log.open('w') as output:
            backend = subprocess.Popen([sys.executable, '-c', backend_code, str(pidfile), *command],
                                       cwd=self.repo, env=dict(self.env, FIXTURE_SECONDS='60'),
                                       stdout=output, stderr=output)
        self.jobs.append(backend)
        self.until(log, 'HEAVY ')
        backend.terminate()
        backend.wait(timeout=5)
        self.until(log, 'terminal outcome: 130')
        following, output = self.start()
        self.finish(following, output)

    def test_recovery_after_a_new_boot_does_not_run_a_check(self):
        # OS boot identity is the only additional external adapter substituted.
        code = ('import importlib.machinery, pathlib, sys; '
                'm=importlib.machinery.SourceFileLoader("admission", sys.argv[1]).load_module(); '
                'm.USER_HOME=pathlib.Path(sys.argv[2]); m.boot_identity=lambda:sys.argv[3]; '
                'sys.exit(m.main(sys.argv[4:]))')
        def launch(boot, *args):
            log = self.root / ('boot-' + str(len(self.logs)))
            self.logs.append(log)
            with log.open('w') as output:
                job = subprocess.Popen([sys.executable, '-c', code, str(self.command), str(self.home), boot, *args],
                                       cwd=self.repo, env=dict(self.env, FIXTURE_SECONDS='60'),
                                       stdout=output, stderr=output)
            self.jobs.append(job)
            return job, log
        active, a = launch('00000000-0000-0000-0000-000000000001')
        self.until(a, 'HEAVY ')
        active.kill()
        active.wait(timeout=5)
        status, b = self.start(args=('--status',))
        self.finish(status, b)
        os.killpg(json.loads(b.read_text())['requests'][0]['group'], signal.SIGKILL)
        recovery, c = launch('00000000-0000-0000-0000-000000000002', *self.recovery_args())
        self.assertIn('No check was run', self.finish(recovery, c))
        self.assertNotIn('HEAVY ', c.read_text())
        normal, d = self.start()
        self.finish(normal, d)

    def test_term_resistant_child_is_stopped_without_touching_unrelated_process(self):
        shim = self.bin / 'Rscript'
        shim.write_text('#!' + sys.executable + "\n" +
                        'import os, pathlib, signal, subprocess, sys, time\n' +
                        'child=subprocess.Popen([sys.executable, "-c", "import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); time.sleep(60)"], start_new_session=True)\n' +
                        'time.sleep(0.2)\n' +
                        'pathlib.Path(os.environ["FIXTURE_CHILD_FILE"]).write_text(str(child.pid))\n' +
                        'print("HEAVY resistant", flush=True)\n' +
                        'time.sleep(60)\n')
        unrelated = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)'])
        self.jobs.append(unrelated)
        active, a = self.start()
        self.until(a, 'HEAVY ')
        active.terminate()
        self.finish(active, a, 130)
        self.assertIsNone(unrelated.poll())
        child = int((self.root / 'child.pid').read_text())
        state = subprocess.run(['ps', '-p', str(child), '-o', 'stat='], capture_output=True, text=True)
        self.assertTrue(not state.stdout.strip() or state.stdout.strip().startswith('Z'))

    def test_direct_r_entry_refuses_missing_common_command(self):
        real_rscript = shutil.which('Rscript')
        if not real_rscript:
            self.skipTest('Rscript is needed only for the entry-point integration fixture')
        env = dict(self.env, PATH=str(self.bin) + os.pathsep + '/usr/bin:/bin')
        result = subprocess.run([real_rscript, str(SOURCE.with_name('review-ready-check.R'))],
                                cwd=self.repo, env=env, capture_output=True, text=True)
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertIn('No check was run', result.stderr)
        self.assertNotIn('HEAVY ', result.stdout)

    def test_waiter_survives_journal_disappearing_during_its_read(self):
        active, a = self.start(FIXTURE_SECONDS='2')
        self.until(a, 'HEAVY ')
        # A delayed filesystem open places normal completion before the waiter
        # reads the journal. The test still enters via the CLI.
        code = ("import importlib.machinery, pathlib, sys, time; "
                "m=importlib.machinery.SourceFileLoader('admission', sys.argv[1]).load_module(); " +
                "\nclass DelayedPath(type(pathlib.Path())):\n"
                " def read_text(self, *args, **kwargs):\n"
                "  if self.name != 'request.json': return super().read_text(*args, **kwargs)\n"
                "  print('Reading shared state', flush=True)\n"
                "  deadline=time.monotonic()+8\n"
                "  while self.exists() and time.monotonic()<deadline: time.sleep(0.02)\n"
                "  return super().read_text(*args, **kwargs)\n" +
                "m.USER_HOME=DelayedPath(sys.argv[2]); sys.exit(m.main([]))")
        log = self.root / 'delayed-read.log'
        with log.open('w') as output:
            waiter = subprocess.Popen([sys.executable, '-c', code, str(self.command), str(self.home)],
                                      cwd=self.repo, env=self.env, stdout=output, stderr=output)
        self.jobs.append(waiter)
        self.until(log, 'Reading shared state')
        self.finish(active, a)
        self.assertIn('HEAVY ', self.finish(waiter, log))


class References(unittest.TestCase):
    def test_required_reference_verification_reads_unusual_tracked_names(self):
        rscript = shutil.which('Rscript')
        if not rscript:
            self.skipTest('Rscript is required for the reference-verifier fixture')
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary) / 'source'
            source = SOURCE.parent.parent
            subprocess.run(['git', 'clone', '--quiet', '--shared', str(source), str(repo)], check=True)
            verifier = '.github/scripts/verify-doc-references.R'
            shutil.copyfile(source / verifier, repo / verifier)
            names = ('design/reference\nprobe.md', 'design/日本語.md',
                     'design/reference\tprobe.md', 'design/a"quote.md', 'design/a\\slash.md')
            for name in names:
                (repo / name).write_text('`R/not-a-real-file.R`\n')
            subprocess.run(['git', '-C', str(repo), 'add', '.'], check=True)
            result = subprocess.run([rscript, verifier], cwd=repo, capture_output=True, text=True)
            self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
            self.assertIn('not-a-real-file', result.stderr)
            for name in names:
                self.assertIn(name, result.stderr)
                (repo / name).write_text('See `design/architecture.md`.\n')
            result = subprocess.run([rscript, verifier], cwd=repo, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == '__main__':
    unittest.main()
