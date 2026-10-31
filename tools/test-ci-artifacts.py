"""Exercise artifact verification through its command boundary on Git fixtures."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

SOURCE = Path(__file__).resolve().parent.parent


class Artifacts(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.repo = Path(self.temp.name) / 'source'
        self.git('clone', '--quiet', '--shared', str(SOURCE), str(self.repo), cwd=SOURCE)
        self.git('config', 'user.name', 'Artifact fixture')
        self.git('config', 'user.email', 'artifact@example.com')
        # Exercise current checker sources during the implementation loop too.
        for relative in ('tools/review-ready-admission.py', 'tools/verify-ci-artifacts.py',
                         'tools/ci-workflow-contracts.json',
                         '.github/scripts/verify-doc-references.R',
                         '.github/scripts/summarize-check-log.R'):
            shutil.copyfile(SOURCE / relative, self.repo / relative)
        for path in (SOURCE / '.github/workflows').glob('*.yaml'):
            shutil.copyfile(path, self.repo / '.github/workflows' / path.name)
        self.commit()
        self.base = self.git('rev-parse', 'HEAD').strip()

    def git(self, *args, cwd=None):
        return subprocess.check_output(['git', *args], cwd=cwd or self.repo,
                                       stderr=subprocess.STDOUT, text=True)

    def commit(self):
        self.git('add', '-A')
        self.git('commit', '--allow-empty', '-qm', 'artifact fixture')

    def check(self, status=0, fragment='workflow executable contracts'):
        self.commit()
        result = subprocess.run(['python3', 'tools/verify-ci-artifacts.py'], cwd=self.repo,
                                env=dict(os.environ, MARGINPLYR_REVIEW_BASE=self.base),
                                capture_output=True, text=True)
        output = result.stdout + result.stderr
        self.assertEqual(result.returncode, status, output)
        self.assertIn(fragment, output)

    def workflow(self, name='lint.yaml'):
        return self.repo / '.github/workflows' / name

    def test_comments_keep_the_executable_contract(self):
        path = self.workflow()
        path.write_text(path.read_text() + '\n# Artifact verification fixture\n')
        self.check()

    def test_workflow_name_cannot_collide_with_package_check_concurrency(self):
        path = self.workflow()
        path.write_text(path.read_text().replace('name: lint.yaml', 'name: R-CMD-check.yaml'))
        self.check(1, 'Workflow executable contract changed')

    def test_removed_wiring_conditions_environment_and_invalid_syntax_fail(self):
        path = self.workflow()
        original = path.read_text()
        changes = (
            (original.replace('run: Rscript .github/scripts/verify-context-budget.R',
                              'run: echo skipped'), 'Workflow executable contract changed'),
            (original.replace('  context-budget:\n',
                              "  context-budget:\n    if: github.event_name == 'push'\n"),
             'Workflow executable contract changed'),
            (original.replace('LINTR_ERROR_ON_LINT: true', 'LINTR_ERROR_ON_LINT: false'),
             'Workflow executable contract changed'),
            (original.replace('LINTR_ERROR_ON_LINT: true', 'LINTR_ERROR_ON_LINT: yes'),
             'Workflow executable contract changed'),
            (original + '\ninvalid: [\n', 'exit status 1'),
        )
        for contents, fragment in changes:
            with self.subTest(fragment=fragment, contents=contents[-80:]):
                path.write_text(contents)
                self.check(1, fragment)
        path.write_text(original)

    def test_unknown_workflow_and_script_are_unresolved_even_with_full(self):
        for name in ('.github/workflows/unknown.yaml', '.github/scripts/unknown.R',
                     'altdoc/pkgdown.yml'):
            with self.subTest(name=name):
                path = self.repo / name
                path.write_text('# no verified contract\n')
                self.check(1, 'CI validation unresolved')
                path.unlink()

    def test_changed_contract_cannot_silently_drop_a_workflow(self):
        path = self.repo / 'tools/ci-workflow-contracts.json'
        contract = json.loads(path.read_text())
        del contract['.github/workflows/lint.yaml']
        path.write_text(json.dumps(contract))
        self.check(1, 'Workflow executable contract changed')

    def test_missing_actionlint_fails_without_package_fallback(self):
        bin_path = Path(self.temp.name) / 'bin'
        bin_path.mkdir()
        for name in ('git', 'Rscript', 'python3'):
            (bin_path / name).symlink_to(shutil.which(name))
        previous = os.environ['PATH']
        try:
            os.environ['PATH'] = str(bin_path)
            self.check(1, 'install actionlint; package checking is no fallback')
        finally:
            os.environ['PATH'] = previous

    def test_reference_script_contract_accepts_existing_and_rejects_missing_paths(self):
        path = self.repo / 'design/artifact-reference-probe.md'
        for text, status, fragment in (
                ('See `design/architecture.md`.\n', 0, 'Verified:'),
                ('See `R/not-a-real-file.R`.\n', 1, 'not-a-real-file')):
            path.write_text(text)
            self.commit()
            result = subprocess.run(['Rscript', '.github/scripts/verify-doc-references.R'],
                                    cwd=self.repo, capture_output=True, text=True)
            self.assertEqual(result.returncode, status, result.stdout + result.stderr)
            self.assertIn(fragment, result.stdout + result.stderr)

    def test_ci_summary_runner_preserves_status_and_all_diagnostic_details(self):
        check = Path(self.temp.name) / 'check'
        rcheck = check / 'fixture.Rcheck'
        rcheck.mkdir(parents=True)
        summary = Path(self.temp.name) / 'summary.md'
        env = dict(os.environ, MARGINPLYR_CHECK_DIR=str(check),
                   MARGINPLYR_CHECK_LABEL='Fixture check', GITHUB_STEP_SUMMARY=str(summary))
        log = rcheck / '00check.log'
        log.write_text('* checking examples ... ERROR\nexample detail\n'
                       '* checking tests ... WARNING\nwarning detail\n'
                       '* checking metadata ... NOTE\nnote detail\n'
                       '* checking completed ... OK\nStatus: 1 ERROR, 1 WARNING, 1 NOTE\n')
        result = subprocess.run(['Rscript', '.github/scripts/summarize-check-log.R'],
                                cwd=self.repo, env=env, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        actual = summary.read_text()
        for part in ('## Fixture check', 'Status: 1 ERROR, 1 WARNING, 1 NOTE',
                     'example detail', 'warning detail', 'note detail'):
            self.assertIn(part, actual)
        self.assertNotIn('checking completed', actual)
        summary.unlink()
        log.write_text('* checking tests ... OK\n')
        result = subprocess.run(['Rscript', '.github/scripts/summarize-check-log.R'],
                                cwd=self.repo, env=env, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('No status line in the check log.', summary.read_text())
        log.unlink()
        summary.unlink()
        result = subprocess.run(['Rscript', '.github/scripts/summarize-check-log.R'],
                                cwd=self.repo, env=env, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('nothing to summarize', result.stderr)
        self.assertFalse(summary.exists())


if __name__ == '__main__':
    unittest.main()
