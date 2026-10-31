"""Exercise the installed CLI lifecycle with real repositories and light jobs.

Rscript is a PATH fixture at the external checker boundary; no production
argument can change capacity or replace the gate.
"""
import json
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
import fcntl, os, pathlib, subprocess, sys, time
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

    def start(self, repo=None, args=(), **env):
        log = self.root / ('log-' + str(len(self.logs)))
        self.logs.append(log)
        code = ('import importlib.machinery, pathlib, sys; '
                'm=importlib.machinery.SourceFileLoader("admission", sys.argv[1]).load_module(); '
                'm.USER_HOME=pathlib.Path(sys.argv[2]); sys.exit(m.main(sys.argv[3:]))')
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
        self.temp.cleanup()

    def repository_branch(self):
        self.git(self.repo, 'checkout', '-qb', 'selection-support')
        entry = self.repo / 'tools/review-ready-check.R'
        entry.write_text('# Review-ready selection protocol: 1\n')
        self.git(self.repo, 'add', '.')
        self.git(self.repo, 'commit', '-qm', 'selection support')
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

    def test_binary_or_unknown_encoding_markdown_is_full(self):
        self.repository_branch()
        for contents in (b'binary\0payload', b'unknown\xffencoding', b'control\x01character'):
            with self.subTest(contents=contents):
                (self.repo / 'design/note.md').write_bytes(contents)
                self.commit_all()
                selection = self.selected()
                self.assertEqual(selection['path'], 'Full')
                self.assertIn('unsupported prose content', ' '.join(selection['reasons']))

    def test_ci_configuration_routes_survive_cumulative_package_changes(self):
        self.repository_branch()
        for name in ('.github/workflows/lint.yaml', '.github/scripts/verify-doc-references.R',
                     'altdoc/quarto_website.yml', 'codecov.yml', '.codex/config.toml',
                     '.claude/settings.json'):
            path = self.repo / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text('# supported artifact\n')
        self.commit_all()
        selected = self.selected()
        self.assertEqual(selected['path'], 'Repository')
        required = ('tools/verify-ci-artifacts.py', 'tools/test-ci-artifacts.py',
                    'tools/verify-site-artifacts.R', '.github/scripts/verify-coverage-policy.R',
                    '.github/scripts/verify-agent-network-policy.R',
                    '.github/scripts/verify-codex-config.R')
        for verifier in required:
            self.assertIn(verifier, selected['verifiers'])
        (self.repo / 'R').mkdir()
        (self.repo / 'R/change.R').write_text('1\n')
        self.commit_all()
        selected = self.selected()
        self.assertEqual(selected['path'], 'Full')
        for verifier in required:
            self.assertIn(verifier, selected['verifiers'])

    def test_checker_helpers_and_unsupported_ci_inputs_keep_full_contracts(self):
        self.repository_branch()
        baseline = self.git(self.repo, 'rev-parse', 'HEAD')
        for name in ('tools/review-ready-check-lib.R', 'tools/install-review-ready.py',
                     'tools/dependency-requirements.R', 'tools/required-test-suggests.R',
                     '.github/scripts/cran-note-policy.R', 'tools/verify-ci-artifacts.py',
                     'tools/test-ci-artifacts.py', 'tools/ci-workflow-contracts.json',
                     'tools/verify-site-artifacts.R'):
            with self.subTest(name=name):
                self.git(self.repo, 'reset', '--hard', baseline)
                path = self.repo / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text('# checking machinery\n')
                self.commit_all()
                selected = self.selected()
                self.assertEqual(selected['path'], 'Full')
                self.assertIn('tools/test-ci-artifacts.py', selected['verifiers'])
        for name in ('.github/workflows/unknown.yaml', 'altdoc/custom.py',
                     '.github/scripts/unknown.R', 'investigation/executable.R'):
            self.git(self.repo, 'reset', '--hard', baseline)
            path = self.repo / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text('# unsupported\n')
            self.commit_all()
            self.assertEqual(self.selected()['path'], 'Full')

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
                     'README.md', 'README.qmd', 'NEWS.md', 'LICENSE.md', '.github/job.yaml'):
            with self.subTest(name=name):
                self.git(self.repo, 'reset', '--hard', baseline)
                path = self.repo / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text('unknown\n')
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

    def test_later_main_does_not_replace_pinned_comparison_or_run_checks_while_waiting(self):
        self.repository_branch()
        main = self.git(self.repo, 'rev-parse', 'main')
        active, a = self.start(FIXTURE_SECONDS='2')
        self.until(a, 'HEAVY ')
        waiter, b = self.start()
        self.until(b, 'Waiting')
        self.assertNotIn('HEAVY ', b.read_text())
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
        recovery, c = self.start(args=('--recover',))
        self.finish(recovery, c, 4)
        # Stop the fixture through its own recorded manager-independent group.
        status, d = self.start(args=('--status',))
        self.finish(status, d)
        record = json.loads(d.read_text())
        os.killpg(record['group'], signal.SIGKILL)
        time.sleep(0.2)
        stale, e = self.start(args=('--recover',))
        self.assertIn('OS restart', self.finish(stale, e, 4))

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
        active, a = launch('first-boot')
        self.until(a, 'HEAVY ')
        active.kill()
        active.wait(timeout=5)
        status, b = self.start(args=('--status',))
        self.finish(status, b)
        os.killpg(json.loads(b.read_text())['group'], signal.SIGKILL)
        recovery, c = launch('next-boot', '--recover')
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
