"""Contracts for ADR 0038, including the #1107 expectation regression."""
import importlib.machinery
import copy
import fcntl
import json
import os
import re
import io
import tarfile
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
admission = importlib.machinery.SourceFileLoader(
    'evidence_admission_contract', str(ROOT / 'tools/review-ready-admission.py')).load_module()
process_contract = importlib.machinery.SourceFileLoader(
    'evidence_lifecycle', str(ROOT / 'tools/test-review-ready-admission.py')).load_module()


def missing_generation_tools(source):
    match = policy.VECTOR.search(source)
    body = re.sub(rb'"(?:devtools|roxygen2)"\s*,\s*', b'', match[2])
    return source[:match.start(2)] + body + source[match.end(2):]


class ExpectationContract(unittest.TestCase):
    def test_1107_correction_preserves_strict_assertion_and_other_code(self):
        current = (ROOT / '.github/scripts/verify-cran-preflight.R').read_bytes()
        broken = missing_generation_tools(current)
        self.assertNotEqual(broken, current)
        self.assertTrue(policy.recognize_expectation(broken, current))
        self.assertFalse(policy.recognize_expectation(broken, current.replace(
            b'if (!identical(actual, expected))', b'if (FALSE)')))
        self.assertFalse(policy.recognize_expectation(broken, current + b'\nstop("unknown logic")\n'))
        self.assertFalse(policy.recognize_expectation(broken, current.replace(
            b'review_requirements$package,', b'sort(review_requirements$package),')))

    def test_literal_vector_whitespace_preserves_the_dependency_regression(self):
        current = (ROOT / policy.EXPECTATION).read_bytes()
        match = policy.VECTOR.search(current)
        names = re.findall(rb'"[A-Za-z][A-Za-z0-9.]*"', match[2])
        formatted = current[:match.start(2)] + b'c(\n    ' + b',\n    '.join(names) + b'\n  )' + current[match.end(2):]
        broken = missing_generation_tools(formatted)
        self.assertNotEqual(broken, formatted)
        self.assertTrue(policy.recognize_expectation(broken, formatted))

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

    def test_r_probe_does_not_invent_an_empty_date_input(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'DESCRIPTION').write_text('Package: fixture\n')
            probe = json.loads(subprocess.check_output(
                ['Rscript', str(ROOT / 'tools/review-ready-environment.R')], cwd=root, text=True))
            self.assertNotIn('DESCRIPTION:', probe['unregistered'])

    def test_r_probe_refuses_package_reads_of_normalized_execution_ids(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'DESCRIPTION').write_text('Package: fixture\n')
            (root / 'R').mkdir()
            names = ('CODEX_TOOL_CALL_ID', 'CODEX_NETWORK_PROXY_ATTRIBUTION',
                     'CLAUDE_CODE_SESSION_ID', 'CLAUDE_PID')
            for number, name in enumerate(names):
                (root / ('R/session' + str(number) + '.R')).write_text(
                    'session <- function() Sys.getenv("' + name + '")\n')
            probe = json.loads(subprocess.check_output(
                ['Rscript', str(ROOT / 'tools/review-ready-environment.R'), *names], cwd=root, text=True))
            for number in range(len(names)):
                self.assertIn('R/session' + str(number) + '.R', probe['unregistered'])

    def test_r_probe_refuses_unresolved_environment_readers(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'DESCRIPTION').write_text('Package: fixture\n')
            (root / 'R').mkdir()
            readers = ('Sys.getenv()', 'Sys.getenv(paste0("CLAUDE", "_PID"))',
                       'Sys.getenv(c("SAFE_INPUT", paste0("CLAUDE", "_PID")))',
                       'reader <- Sys.getenv', 'get("Sys.getenv")',
                       'Sys.getenv(x = paste0("CLAUDE", "_PID"))',
                       'Sys.getenv("\\x43LAUDE_PID")', 'getenv(name)')
            for number, reader in enumerate(readers):
                (root / ('R/reader' + str(number) + '.R')).write_text(reader + '\n')
            (root / 'R/literal.R').write_text('Sys.getenv("SAFE_INPUT", unset = "")\n')
            probe = json.loads(subprocess.check_output(
                ['Rscript', str(ROOT / 'tools/review-ready-environment.R'),
                 *policy.EXECUTION_METADATA], cwd=root, text=True))
            for number in range(len(readers)):
                self.assertIn('R/reader' + str(number) + '.R', probe['unregistered'])
            self.assertNotIn('R/literal.R', probe['unregistered'])


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

    def environment_at(self, root, variables=None):
        code = ('import importlib.machinery,json,sys; '
                'm=importlib.machinery.SourceFileLoader("identity_probe",sys.argv[1]).load_module(); '
                'print(json.dumps(m.environment(sys.argv[2])))')
        return json.loads(subprocess.check_output(
            [process_contract.sys.executable, '-c', code, policy.__file__, str(root)],
            cwd=root, env=dict(os.environ, **(variables or {})), text=True, stderr=subprocess.PIPE))

    def origin(self, path='Full', status=0, notes=None, mutate=None):
        self.counter += 1
        request = uuid.uuid4().hex
        directory = self.state / 'requests' / request
        directory.mkdir(parents=True)
        selection = dict(self.selection, path=path, submitted=self.sha)
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
        policy.append_evidence(self.state, evidence, payload,
                               lambda path, value: path.write_text(json.dumps(value)))
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

    def test_clock_rollback_cannot_hide_a_later_same_input_failure(self):
        _, evidence = self.origin()
        finished = json.loads(evidence.read_text())['payload']['finished']
        def later_failure(record, worker):
            record['finished'] = finished - 100
            worker['report']['units']['build-full'] = 'running'
        self.origin(status=1, mutate=later_failure)
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_missing_later_failure_or_index_cannot_restore_an_older_success(self):
        self.origin()
        _, failure = self.origin(status=1, mutate=lambda record, worker:
                                 worker['report']['units'].update({'build-full': 'running'}))
        failure.unlink()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        (self.state / 'evidence-index.json').unlink()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_issuance_refuses_a_failure_after_the_last_worker_validation(self):
        _, origin = self.origin()
        record = json.loads(origin.with_name('record.json').read_text())
        record.update(id=uuid.uuid4().hex, token=uuid.uuid4().hex,
                      selection=dict(self.selection, plan=self.plan()))
        directory = self.state / 'requests' / record['id']
        directory.mkdir()
        before = self.observe(self.root, self.sha, policy.required_units(record['selection']))
        report = dict(request=record['token'], units={}, completed=[
            'Package spelling', 'jarl', 'package-aware lintr', 'Test dependency syntax', 'Required test Suggests'])
        worker = dict(before=before, after=before, repository=policy.repository(self.root), report=report)
        payload = policy.issue(self.state, record, worker)
        (directory / 'record.json').write_text(json.dumps(record))
        self.origin(status=1, mutate=lambda record, worker:
                    worker['report']['units'].update({'build-full': 'running'}))
        (directory / 'record.json').write_text(json.dumps(record))
        with self.assertRaisesRegex(ValueError, 'superseded'):
            policy.append_evidence(self.state, directory / 'evidence.json', payload,
                                   lambda path, value: path.write_text(json.dumps(value)))

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
        (self.state / 'evidence-index.json').unlink()
        orphan = self.git('commit-tree', self.git('rev-parse', 'HEAD^{tree}'), '-m', 'unrelated same tree')
        self.sha = orphan
        self.origin()
        self.sha = self.base
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_external_symlink_and_committed_attributes_cannot_supply_source_identity(self):
        external = Path(self.temp.name) / 'external.R'
        external.write_text('behavior <- function() 1\n')
        (self.root / 'R/linked.R').symlink_to(external)
        self.commit()
        with self.assertRaisesRegex(ValueError, 'Unsupported committed source type'):
            policy.source_inputs(self.root, self.sha, 'build-full')
        self.git('reset', '--hard', self.base)
        self.write('.gitattributes', 'R/behavior.R export-subst\n')
        self.commit()
        with self.assertRaisesRegex(ValueError, 'Unregistered committed Git attributes'):
            policy.source_inputs(self.root, self.sha, 'build-full')

    def test_git_replacement_cannot_hide_a_cumulative_package_change(self):
        self.git('checkout', '-qb', 'followup')
        self.write('R/behavior.R', 'behavior <- function() 2\n')
        self.commit()
        self.assertIn('R/behavior.R', self.git('diff', '--name-only', self.base, self.sha))
        self.git('replace', self.sha, self.base)
        self.assertEqual(self.git('diff', '--name-only', self.base, self.sha), '')
        selection = admission.select_path(self.root, self.sha)
        self.assertEqual(selection['path'], 'Full')
        self.assertIsNone(selection['base'])
        self.assertIn('replacement objects', selection['reasons'][0])

    def test_external_git_attributes_cannot_change_archive_behind_a_reused_unit(self):
        self.write('tools/review-ready-environment.R', (ROOT / 'tools/review-ready-environment.R').read_text())
        self.commit()
        config = Path(self.temp.name) / 'gitconfig'
        attributes = Path(self.temp.name) / 'attributes'
        attributes.write_text('')
        config.write_text('[core]\n attributesFile = ' + str(attributes) + '\n')
        previous = os.environ.get('GIT_CONFIG_GLOBAL')
        os.environ['GIT_CONFIG_GLOBAL'] = str(config)
        original = self.observe
        def observe(root, sha, units):
            snapshot = original(root, sha, units)
            snapshot['environment'] = self.environment_at(root)
            return snapshot
        self.observe = observe
        try:
            self.origin()
            self.assertEqual(self.plan()['units']['build-full']['action'], 'reuse')
            before = policy.git(self.root, 'archive', '--format=tar', self.sha)
            attributes.write_text('R/behavior.R export-ignore\n')
            after = policy.git(self.root, 'archive', '--format=tar', self.sha)
            with tarfile.open(fileobj=io.BytesIO(before)) as archive:
                self.assertIn('R/behavior.R', archive.getnames())
            with tarfile.open(fileobj=io.BytesIO(after)) as archive:
                self.assertNotIn('R/behavior.R', archive.getnames())
            self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        finally:
            if previous is None:
                os.environ.pop('GIT_CONFIG_GLOBAL', None)
            else:
                os.environ['GIT_CONFIG_GLOBAL'] = previous

    def test_execution_ids_do_not_invalidate_but_other_or_unknown_values_do(self):
        self.write('tools/review-ready-environment.R', (ROOT / 'tools/review-ready-environment.R').read_text())
        self.commit()
        variables = dict(CODEX_TOOL_CALL_ID='exec-' + str(uuid.uuid4()),
                         CODEX_NETWORK_PROXY_ATTRIBUTION=str(uuid.uuid4()), CODEX_REVIEW_BEHAVIOR='original')
        original = self.observe
        def observe(root, sha, units):
            snapshot = original(root, sha, units)
            snapshot['environment'] = self.environment_at(root, variables)
            return snapshot
        self.observe = observe
        self.origin()
        variables.update(CODEX_TOOL_CALL_ID='exec-' + str(uuid.uuid4()),
                         CODEX_NETWORK_PROXY_ATTRIBUTION=str(uuid.uuid4()))
        self.assertEqual(self.plan()['units']['build-full']['action'], 'reuse')
        variables['CODEX_REVIEW_BEHAVIOR'] = 'changed'
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        variables['CODEX_REVIEW_BEHAVIOR'] = 'original'
        variables['CODEX_NETWORK_PROXY_ATTRIBUTION'] = 'unregistered proxy configuration'
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_claude_session_ids_do_not_invalidate_but_shell_settings_do(self):
        self.write('tools/review-ready-environment.R', (ROOT / 'tools/review-ready-environment.R').read_text())
        self.commit()
        variables = dict(CLAUDE_CODE_SESSION_ID=str(uuid.uuid4()), CLAUDE_PID='32101',
                         CLAUDECODE='1', CLAUDE_CODE_SHELL_PREFIX='original shell wrapper')
        original = self.observe
        def observe(root, sha, units):
            snapshot = original(root, sha, units)
            snapshot['environment'] = self.environment_at(root, variables)
            return snapshot
        self.observe = observe
        self.origin()
        variables.update(CLAUDE_CODE_SESSION_ID=str(uuid.uuid4()), CLAUDE_PID='32102')
        self.assertEqual(self.plan()['units']['build-full']['action'], 'reuse')
        variables['CLAUDE_CODE_SHELL_PREFIX'] = 'changed shell wrapper'
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        variables['CLAUDE_CODE_SHELL_PREFIX'] = 'original shell wrapper'
        variables['CLAUDE_ENV_FILE'] = str(Path(self.temp.name) / 'changed-environment.sh')
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        variables.pop('CLAUDE_ENV_FILE')
        variables['CLAUDE_CODE_SESSION_ID'] = 'unregistered session identifier'
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_older_probe_cannot_silently_ignore_the_metadata_audit(self):
        self.write('tools/real-environment.R', (ROOT / 'tools/review-ready-environment.R').read_text())
        for legacy in ('NULL', 'list(version = 1L, names = as.list(commandArgs(trailingOnly = TRUE)))'):
            self.write('tools/review-ready-environment.R', '''
result <- system2("Rscript", c("tools/real-environment.R", commandArgs(trailingOnly = TRUE)), stdout = TRUE)
snapshot <- jsonlite::fromJSON(paste(result, collapse = "\\n"), simplifyVector = FALSE)
snapshot$metadata_audit <- ''' + legacy + '''
cat(jsonlite::toJSON(snapshot, auto_unbox = TRUE, null = "null"))
''')
            with self.assertRaises(subprocess.CalledProcessError) as failure:
                self.environment_at(self.root)
            self.assertIn('Unavailable execution metadata audit', failure.exception.stderr)

    def stage_request(self, status):
        _, origin = self.origin()
        record = json.loads(origin.with_name('record.json').read_text())
        request = uuid.uuid4().hex
        directory = self.state / 'requests' / request
        directory.mkdir()
        workspace = directory / 'workspace'
        workspace.mkdir()
        record.update(id=request, token=uuid.uuid4().hex, root=str(self.root),
            boot=str(uuid.uuid4()), manager=dict(pid=99999999, birth='fixture process'),
            group=None, children=[], state='running', capacity='Full', may_own_full=True,
            owned_capacity=True, epoch=1, gate_started=True, requires_proof=True,
            workspace=str(workspace), cleanup='pending', promotions=[],
            channel='/tmp/marginplyr-review-' + request + '.sock', channel_owned=False,
            evidence_supported=True, repository=policy.repository(self.root))
        record['selection']['plan'].update(schema=1, snapshot=None, reasons=[])
        path = directory / 'record.json'
        admission.save(path, record)
        before = self.observe(self.root, self.sha, policy.required_units(record['selection']))
        original = json.loads(origin.read_text())['payload']
        build = original['units']['build-full']
        report = dict(request=record['token'], units={'build-full': 'running' if status else 'passed',
                      'generation': 'passed'}, check=build['check'], timestamps=build['timestamps'],
                      completed=original['completed'])
        worker = dict(before=before, after=before, repository=record['repository'], report=report, status=status)
        (workspace / 'worker-result.json').write_text(json.dumps(worker))
        return record, path, workspace, worker

    def recover_stage(self, record):
        observer = admission.processes
        admission.processes = lambda: {}
        try:
            self.assertEqual(admission.recover_request(self.state, record['id'], record['boot']), 0)
        finally:
            admission.processes = observer

    def test_failed_stage_survives_cleanup_failure_and_recovery(self):
        record, path, workspace, worker = self.stage_request(1)
        clean = admission.clean_workspace
        def failed_cleanup(record):
            raise OSError('fixture cleanup failure')
        admission.clean_workspace = failed_cleanup
        try:
            with self.assertRaisesRegex(OSError, 'fixture cleanup failure'):
                admission.finish_request(self.state, path, record, 1)
        finally:
            admission.clean_workspace = clean
        record.update(state='recovery', cleanup='unconfirmed')
        admission.save(path, record)
        self.assertIsNotNone(admission.read_record(path))
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.recover_stage(record)
        self.assertFalse(workspace.exists())
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.origin()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'reuse')

    def test_failure_before_first_unit_accepts_the_empty_r_report(self):
        record, path, workspace, worker = self.stage_request(1)
        worker['report']['units'] = []
        (workspace / 'worker-result.json').write_text(json.dumps(worker))
        self.assertEqual(admission.finish_request(self.state, path, record, 1), 1)
        self.assertFalse(workspace.exists())
        retained = json.loads(path.with_name('observation.json').read_text())['payload']
        self.assertEqual(retained['outcome'], 1)
        self.assertEqual(retained['units'], {})

    def test_unobserved_failed_running_request_fences_an_older_success(self):
        record, path, workspace, worker = self.stage_request(1)
        self.assertIsNotNone(admission.read_record(path))
        self.assertNotIn('gate_outcome', record)
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.recover_stage(record)
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.origin()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'reuse')

    def test_unobserved_running_request_without_worker_result_is_uncertain(self):
        record, path, workspace, worker = self.stage_request(1)
        (workspace / 'worker-result.json').unlink()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.recover_stage(record)
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.origin()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'reuse')

    def test_missing_indexed_origin_journal_does_not_block_executed_cleanup(self):
        record, path, workspace, worker = self.stage_request(0)
        origin = next(self.state.glob('requests/*/evidence.json'))
        origin.with_name('record.json').unlink()
        self.assertEqual(admission.finish_request(self.state, path, record, 0), 0)
        self.assertFalse(workspace.exists())
        self.assertEqual(record['cleanup'], 'confirmed')
        self.assertTrue(record['observation_error'])
        self.assertFalse(path.with_name('evidence.json').exists())
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_pending_success_observation_is_not_an_execution_origin(self):
        record, path, workspace, worker = self.stage_request(0)
        for evidence in self.state.glob('requests/*/evidence.json'):
            evidence.unlink()
        (self.state / 'evidence-index.json').unlink()
        record['gate_outcome'] = 0
        admission.save(path, record)
        policy.observe_outcome(self.state, path, record, worker, admission.save)
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')

    def test_delayed_success_cleanup_cannot_hide_a_later_stage_failure(self):
        record, path, workspace, worker = self.stage_request(0)
        admission.retain_outcome(self.state, path, record, 0)
        sequence = record['observation_sequence']
        self.origin(status=1, mutate=lambda record, worker:
                    worker['report']['units'].update({'build-full': 'running'}))
        self.assertEqual(admission.finish_request(self.state, path, record, 0), 0)
        retained = json.loads(path.with_name('evidence.json').read_text())['payload']
        self.assertGreater(retained['sequence'], sequence)
        self.assertEqual(retained['units']['build-full']['observation_sequence'], sequence)
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.origin()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'reuse')

    def test_recovery_completes_only_the_interrupted_observation_index(self):
        record, path, workspace, worker = self.stage_request(1)
        original = admission.save
        def fail_index(path, value):
            if path.name == 'evidence-index.json':
                raise OSError('fixture index write failure')
            return original(path, value)
        admission.save = fail_index
        try:
            with self.assertRaisesRegex(OSError, 'fixture index write failure'):
                admission.finish_request(self.state, path, record, 1)
        finally:
            admission.save = original
        self.assertTrue(path.with_name('observation.json').exists())
        record.update(state='recovery', cleanup='unconfirmed')
        admission.save(path, record)
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.recover_stage(record)
        self.assertFalse(workspace.exists())
        self.assertEqual(self.plan()['units']['build-full']['action'], 'execute')
        self.origin()
        self.assertEqual(self.plan()['units']['build-full']['action'], 'reuse')


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
        (self.repo / policy.EXPECTATION).write_bytes(missing_generation_tools(current.encode()))
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
        self.assertIn('; cleanup: confirmed;', output)
        self.assertIn('observation unavailable for reuse', output)
        self.assertEqual(len(list(state.glob('requests/*/evidence.json'))), 0)

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

    def test_terminal_publication_failure_cannot_return_a_limited_pass(self):
        current = self.prepare_evidence()
        job, log = self.start()
        self.finish(job, log)
        (self.repo / policy.EXPECTATION).write_text(current)
        self.commit_all()
        adapter = '''
original_save = m.save
def fail_evidence(path, value):
    if path.name == 'evidence.json': raise OSError('fixture publication failure')
    return original_save(path, value)
m.save = fail_evidence
'''
        job, log = self.start(adapter=adapter)
        output = self.finish(job, log, status=1)
        self.assertIn('Reuse invalidated at terminal issuance', output)
        self.assertIn('REUSED build-full', output)
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
