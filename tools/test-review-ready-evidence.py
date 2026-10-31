"""Contracts for ADR 0038, including the #1107 expectation regression."""
import importlib.machinery
import copy
import fcntl
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time
import unittest
import uuid


ROOT = Path(__file__).resolve().parent.parent
policy = importlib.machinery.SourceFileLoader(
    'evidence_contract', str(ROOT / 'tools/review-ready-evidence.py')).load_module()
process_contract = importlib.machinery.SourceFileLoader(
    'evidence_lifecycle', str(ROOT / 'tools/test-review-ready-admission.py')).load_module()


class ExpectationContract(unittest.TestCase):
    def test_1107_correction_preserves_strict_assertion_and_other_code(self):
        current = (ROOT / '.github/scripts/verify-cran-preflight.R').read_bytes()
        broken = current.replace(b'"altdoc", "devtools",', b'"altdoc",').replace(
            b'"roxygen2", "spelling"', b'"spelling"')
        self.assertNotEqual(broken, current)
        self.assertTrue(policy.recognize_expectation(broken, current))
        self.assertFalse(policy.recognize_expectation(broken, current.replace(
            b'if (!identical(actual, expected))', b'if (FALSE)')))
        self.assertFalse(policy.recognize_expectation(broken, current + b'\nstop("unknown logic")\n'))
        self.assertFalse(policy.recognize_expectation(broken, current.replace(
            b'review_requirements$package,', b'sort(review_requirements$package),')))

    def test_timestamp_boundaries_unknown_r_and_rollback(self):
        clock = dict(zip(('r', 'checker', 'timeout'), policy.CHECKER), leeway=300)
        inventory = dict(observed=1000, paths={'DESCRIPTION': 1300})
        self.assertTrue(policy.clock_valid(clock, inventory, 1300, 1000))
        self.assertFalse(policy.clock_valid(clock, inventory, 1300.001, 1000))
        self.assertFalse(policy.clock_valid(clock, inventory, 1000, 999))
        self.assertFalse(policy.clock_valid(dict(clock, r='unknown'), inventory, 1000, 1000))
        self.assertFalse(policy.clock_valid(clock, dict(observed=1000, paths={}), 1000, 1000))
        self.assertFalse(policy.clock_valid(dict(clock, leeway=float('nan')), inventory, 1000, 1000))

    def test_r_probe_refuses_unregistered_date_and_clock_inputs(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'DESCRIPTION').write_text('Package: fixture\nDate: 2026-10-31\n')
            (root / 'R').mkdir()
            (root / 'R/clock.R').write_text('clock <- function() Sys.Date()\n')
            probe = json.loads(subprocess.check_output(
                ['Rscript', str(ROOT / 'tools/review-ready-environment.R')], cwd=root, text=True))
            self.assertIn('DESCRIPTION:Date', probe['unregistered'])
            self.assertIn('R/clock.R', probe['unregistered'])


class StageEvidence(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name) / 'repo'
        self.state = Path(self.temp.name) / 'state'
        self.root.mkdir()
        self.state.mkdir()
        self.git('init', '-q', '-b', 'main')
        self.git('config', 'user.email', 'fixture@example.com')
        self.git('config', 'user.name', 'Fixture')
        self.git('remote', 'add', 'origin', 'https://github.com/sayuks/marginplyr.git')
        for path, content in {
                'DESCRIPTION': 'Package: marginplyr\nConfig/Needs/review: devtools, roxygen2\n',
                'tools/review-ready-check.R': policy.PROTOCOL.decode() + '\n',
                'R/behavior.R': 'behavior <- function() 1\n',
                'tools/review-ready-progress-reporter.R': '# reporter\n',
                'tests/testthat/test-behavior.R': '# package test\n'}.items():
            self.write(path, content)
        self.write(policy.EXPECTATION, (ROOT / policy.EXPECTATION).read_text())
        self.commit()
        self.base = self.sha
        self.selection = dict(path='Full', base=self.base, main=self.base, submitted=self.sha,
                              verifiers=['tools/review-ready-generated.R'])
        self.clock = dict(zip(('r', 'checker', 'timeout'), policy.CHECKER), leeway=300)
        self.env = dict(digest='external-environment', clock=self.clock, r='4.6.1', libraries=[])
        self.key = policy.signing_key(self.state, create=True)
        self.counter = 0

    def git(self, *args):
        return subprocess.check_output(['git', '-C', str(self.root), *args], stderr=subprocess.PIPE).decode().strip()

    def write(self, path, content):
        target = self.root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(content)

    def commit(self):
        self.git('add', '.')
        self.git('commit', '-qm', 'fixture')
        self.sha = self.git('rev-parse', 'HEAD')

    def observe(self, root, sha, units):
        return dict(environment=copy.deepcopy(self.env),
                    sources={unit: policy.source_inputs(root, sha, unit) for unit in units})

    def origin(self, path='Full', status=0, notes=None, mutate=None):
        self.counter += 1
        request = uuid.uuid4().hex
        directory = self.state / 'requests' / request
        directory.mkdir(parents=True)
        selection = dict(self.selection, path=path)
        units = policy.required_units(selection)
        selection['plan'] = dict(units={unit: dict(action='execute') for unit in units})
        record = dict(id=request, sha=self.sha, token=uuid.uuid4().hex, launcher='trusted supervisor',
                      state='terminal', cleanup='confirmed', outcome=status, owned_capacity=False,
                      version=2, protocol=3, authorized=True,
                      finished=time.time() + self.counter, selection=selection)
        classifications = [] if notes is None else notes
        attempt = dict(status=0, timeout=False, errors=0, warnings=0, notes=classifications)
        check = dict(args=['--as-cran'] + (['--no-tests'] if path == 'Documentation' else []),
                     status=0, timeout=False, errors=0, warnings=0, notes=classifications, attempts=[attempt])
        report = dict(request=record['token'], units={unit: 'passed' for unit in units},
                      check=check, timestamps=dict(observed=time.time()-2, paths={'DESCRIPTION': time.time()-3}),
                      completed=['Package spelling', 'jarl', 'package-aware lintr',
                                 'Test dependency syntax', 'Required test Suggests',
                                 'Artifact verification: tools/review-ready-generated.R'])
        before = self.observe(self.root, self.sha, units)
        worker = dict(before=before, after=before, repository=policy.repository(self.root), report=report)
        if mutate:
            mutate(record, worker)
        payload = policy.issue(self.state, record, worker)
        self.assertIsNotNone(payload)
        (directory / 'record.json').write_text(json.dumps(record))
        evidence = directory / 'evidence.json'
        evidence.write_text(json.dumps(policy.seal(payload, self.key)))
        return request, evidence

    def plan(self, **kwargs):
        return policy.plan(self.root, self.sha, dict(self.selection, submitted=self.sha), self.state,
                           observe=self.observe, **kwargs)

    def test_mixed_branch_keeps_obligations_and_reuses_actual_origin(self):
        origin, _ = self.origin()
        self.write('design/followup.md', 'Verification prose\n')
        self.commit()
        plan = self.plan()
        self.assertEqual(plan['units']['build-full']['request'], origin)
        self.assertEqual(plan['units']['generation']['action'], 'reuse')
        self.assertEqual(self.selection['base'], self.base)
        self.write('tests/testthat/test-behavior.R', '# changed test\n')
        self.commit()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_description_reporter_unknown_paths_modes_and_assertion_weakening_invalidate(self):
        self.origin()
        for name in ('DESCRIPTION', 'tools/review-ready-progress-reporter.R', 'unknown-input',
                     'R/behavior.R', policy.EXPECTATION):
            with self.subTest(path=name):
                self.git('reset', '--hard', self.base)
                original = (self.root / name).read_text() if (self.root / name).exists() else ''
                self.write(name, original + '\n# executable input changed\n')
                self.commit()
                self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.git('reset', '--hard', self.base)
        self.write('design/note.md', 'safe prose\n')
        os.chmod(self.root / 'design/note.md', 0o755)
        self.commit()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_missing_corrupt_partial_incompatible_and_nonancestor_records(self):
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        _, evidence = self.origin()
        original = evidence.read_text()
        evidence.write_text(original.replace('trusted supervisor', 'corrupt supervisor'))
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        evidence.write_text(original)
        self.env['digest'] = 'different installed contents'
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.env['digest'] = 'external-environment'
        record = evidence.with_name('record.json')
        data = json.loads(record.read_text())
        data['cleanup'] = 'unconfirmed'
        record.write_text(json.dumps(data))
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        data['cleanup'] = 'confirmed'
        record.write_text(json.dumps(data))
        self.write('R/new.R', '1\n')
        self.commit()
        self.origin()
        self.git('reset', '--hard', self.base)
        self.sha = self.base
        self.env['clock']['r'] = 'unrecognized R checker'
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_documentation_never_discharges_full_repository_has_no_package_claim(self):
        self.origin(path='Documentation')
        plan = self.plan()
        self.assertEqual(plan['units']['build-full']['action'], 'execute')
        self.assertEqual(plan['units']['generation']['action'], 'reuse')
        self.selection['path'] = 'Documentation'
        self.assertTrue(all(unit['action'] == 'reuse' for unit in self.plan()['units'].values()))
        self.selection['path'] = 'Repository'
        self.selection['verifiers'] = []
        self.assertEqual(self.plan()['units'], {})

    def test_later_failure_blocks_older_pass_and_unknown_notes_are_not_reusable(self):
        self.origin()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'reuse')
        self.origin(status=1, mutate=lambda record, worker: worker['report']['units'].update({'build-full': 'running'}))
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.origin(notes=['unknown-note'])
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_retry_requires_complete_success_and_preserved_first_attempt(self):
        check = dict(args=['--as-cran'], status=0, timeout=False, errors=0, warnings=0, notes=[],
                     attempts=[dict(status=0, timeout=False, errors=0, warnings=0, notes=[])])
        self.assertTrue(policy.valid_check(check, 'build-full'))
        check['attempts'].insert(0, dict(status=1, timeout=False, errors=1, warnings=0, notes=[]))
        self.assertFalse(policy.valid_check(check, 'build-full'))
        check.update(retry_verified=True, first_failure='/retained/diagnostics', retry_identity={'hash': 'same tarball'})
        self.assertTrue(policy.valid_check(check, 'build-full'))
        check['attempts'][0]['warnings'] = 1
        self.assertFalse(policy.valid_check(check, 'build-full'))

    def test_same_version_installed_bytes_and_change_metadata_are_inputs(self):
        package = Path(self.temp.name) / 'installed-package'
        package.mkdir()
        (package / 'DESCRIPTION').write_text('Version: 1.0\n')
        code = package / 'implementation'
        code.write_text('correct')
        before = policy.directory_identity(package)
        code.write_text('changed')
        self.assertNotEqual(before, policy.directory_identity(package))
        code.write_text('correct')
        self.assertNotEqual(before, policy.directory_identity(package))

    def test_missing_referenced_origin_never_becomes_an_execution(self):
        request, evidence = self.origin()
        payload = json.loads(evidence.read_text())['payload']
        payload['reused'] = {'build-full': dict(request='f'*32, unit='build-full', submitted=self.sha)}
        evidence.write_text(json.dumps(policy.seal(payload, self.key)))
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.assertRaises((ValueError, FileNotFoundError), policy.read_evidence, evidence, self.key)

    def test_partial_completion_and_cleanup_cannot_issue_success(self):
        _, evidence = self.origin()
        record = json.loads(evidence.with_name('record.json').read_text())
        payload = json.loads(evidence.read_text())['payload']
        before = self.observe(self.root, self.sha, policy.required_units(record['selection']))
        report = dict(request=record['token'], units={'build-full': 'passed', 'generation': 'passed'},
                      check=payload['units']['build-full']['check'], completed=['Package spelling', 'jarl'])
        worker = dict(before=before, after=before, repository=policy.repository(self.root), report=report)
        self.assertIsNone(policy.issue(self.state, record, worker))
        record['cleanup'] = 'unconfirmed'
        self.assertIsNone(policy.issue(self.state, record, worker))

    def test_other_clone_is_eligible_but_same_tree_nonancestor_is_not(self):
        self.origin()
        clone = Path(self.temp.name) / 'clone'
        subprocess.run(['git', 'clone', '-q', str(self.root), str(clone)], check=True)
        subprocess.run(['git', '-C', str(clone), 'remote', 'set-url', 'origin',
                        'https://github.com/sayuks/marginplyr.git'], check=True)
        plan = policy.plan(clone, self.sha, self.selection, self.state, observe=self.observe)
        self.assertEqual(plan['units']['build-full']['action'], 'reuse')
        for path in self.state.glob('requests/*/evidence.json'):
            path.unlink()
        orphan = self.git('commit-tree', self.git('rev-parse', 'HEAD^{tree}'), '-m', 'unrelated same tree')
        self.sha = orphan
        self.origin()
        self.sha = self.base
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')


class EvidenceLifecycle(process_contract.Lifecycle):
    def finish(self, job, log, status=0):
        self.assertEqual(job.wait(timeout=60), status, log.read_text())
        return log.read_text()

    def prepare_evidence(self):
        real_rscript = shutil.which('Rscript')
        tracked = subprocess.check_output(['git', '-C', str(ROOT), 'ls-files', '-z']).decode().split('\0')[:-1]
        for name in tracked:
            source = ROOT / name
            if source.is_file():
                destination = self.repo / name
                destination.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(source, destination)
        self.git(self.repo, 'remote', 'add', 'origin', 'https://github.com/sayuks/marginplyr.git')
        current = (self.repo / policy.EXPECTATION).read_text()
        (self.repo / policy.EXPECTATION).write_text(current.replace('"altdoc", "devtools",', '"altdoc",').replace(
            '"roxygen2", "spelling"', '"spelling"'))
        self.commit_all()
        if self.git(self.repo, 'branch', '--show-current') != 'main':
            self.git(self.repo, 'branch', '-f', 'main', 'HEAD')
        self.git(self.repo, 'checkout', '-qb', 'mixed-followup')
        (self.repo / 'R/evidence-fixture.R').write_text('fixture <- function() 1\n')
        self.commit_all()
        admission = importlib.machinery.SourceFileLoader('evidence_installer', str(process_contract.SOURCE)).load_module()
        self.command.write_bytes(admission.installed_source(self.repo))
        self.env.update(FIXTURE_EVIDENCE='true', FIXTURE_REAL_RSCRIPT=real_rscript)
        return current

    def test_1107_new_format_baseline_then_actual_contract_without_full_replay(self):
        current = self.prepare_evidence()
        job, log = self.start()
        baseline = self.finish(job, log)
        self.assertIn('EXECUTED build-full', baseline)
        self.assertIn('Retained structured stage evidence:', baseline)
        (self.repo / policy.EXPECTATION).write_text(current)
        self.commit_all()
        state = self.home / '.local/state/marginplyr/review-ready'
        with (state / 'admission.lock').open('a+') as lock:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            job, log = self.start()
            followup = self.finish(job, log)
        self.assertIn('"path": "Full"', followup)
        self.assertIn('; capacity: exempt;', followup)
        self.assertIn('REUSED build-full', followup)
        self.assertNotIn('EXECUTED build-full', followup)
        self.assertIn('original mismatch and negative cases rejected', followup)
        note = self.repo / 'design/evidence-followup.md'
        note.write_text('Repeated follow-up\n')
        self.commit_all()
        job, log = self.start()
        repeated = self.finish(job, log)
        self.assertIn('REUSED build-full', repeated)
        retained = list(state.glob('requests/*/evidence.json'))
        self.assertEqual(len(retained), 3)
        original = next(json.loads(path.read_text())['payload']['request'] for path in retained
                        if 'build-full' in json.loads(path.read_text())['payload']['units'])
        for path in retained:
            payload = json.loads(path.read_text())['payload']
            if payload['reused']:
                self.assertEqual(payload['reused']['build-full']['request'], original)

    def test_missing_baseline_runs_full_and_reuse_cancellation_keeps_origin(self):
        current = self.prepare_evidence()
        job, log = self.start()
        self.finish(job, log)
        state = self.home / '.local/state/marginplyr/review-ready'
        (self.repo / policy.EXPECTATION).write_text(current)
        self.commit_all()
        job, log = self.start()
        self.until(log, 'HEAVY ', timeout=60)
        job.terminate()
        cancelled = self.finish(job, log, status=130)
        self.assertIn('; cleanup: confirmed;', cancelled)
        self.assertIn('; capacity: exempt;', cancelled)
        self.assertEqual(len(list(state.glob('requests/*/evidence.json'))), 1)
        for path in state.glob('requests/*/evidence.json'):
            path.unlink()
        job, log = self.start()
        output = self.finish(job, log)
        self.assertIn('; capacity: Full;', output)
        self.assertIn('EXECUTED build-full', output)

    def test_environment_change_during_reuse_fails_without_full_fallback(self):
        current = self.prepare_evidence()
        dependency = self.root / 'dependency'
        dependency.write_text('original installed bytes')
        trigger = self.root / 'mutate-environment'
        self.env.update(FIXTURE_ENVIRONMENT_FILE=str(dependency), FIXTURE_ENVIRONMENT_TRIGGER=str(trigger))
        job, log = self.start()
        self.finish(job, log)
        (self.repo / policy.EXPECTATION).write_text(current)
        self.commit_all()
        trigger.touch()
        job, log = self.start()
        output = self.finish(job, log, status=1)
        self.assertIn('Reuse invalidated during validation', output)
        self.assertIn('REUSED build-full', output)
        self.assertNotIn('EXECUTED build-full', output)
        self.assertIn('; cleanup: confirmed;', output)


def load_tests(loader, tests, pattern):
    suite = unittest.TestSuite()
    for case in (ExpectationContract, StageEvidence, EvidenceLifecycle):
        for name in case.__dict__:
            if name.startswith('test_'):
                suite.addTest(case(name))
    return suite


if __name__ == '__main__':
    unittest.main()
