"""Verify supported CI artifacts, refusing changes without a local contract.

Workflow contracts preserve executable semantics. Comments and display names
can change independently; other changes require updating this protected checker
or its contract with Full evidence. A package check cannot supply that evidence.
"""
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys


def registry():
    spec = importlib.util.spec_from_file_location('admission', 'tools/review-ready-admission.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def run(*args):
    return subprocess.check_output(args, text=True, stderr=subprocess.STDOUT)


def workflows(paths):
    # R's YAML reader rejects duplicate keys and never evaluates YAML tags.
    code = ('p <- commandArgs(TRUE); '
            'tags <- c("bool#yes", "bool#no", "int", "int#hex", "int#oct", '
            '"int#base60", "float#fix", "float#exp", "float#base60", '
            '"float#inf", "float#neginf", "float#nan", "seq"); '
            'h <- setNames(rep(list(identity), length(tags)), tags); '
            'x <- setNames(lapply(p, yaml::yaml.load_file, eval.expr = FALSE, handlers = h), p); '
            'cat(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))')
    return json.loads(run('Rscript', '-e', code, *paths))


def executable_contract(workflow):
    workflow.pop('name', None)
    for job in workflow.get('jobs', {}).values():
        job.pop('name', None)
        for step in job.get('steps', []):
            step.pop('name', None)
    return workflow


def verify():
    policy = registry()
    base = os.environ.get('MARGINPLYR_REVIEW_BASE')
    if not base and os.environ.get('MARGINPLYR_REVIEW_REQUEST'):
        raise ValueError('CI validation unresolved: admission did not supply a pinned comparison base.')
    if not base:
        reference = 'main' if subprocess.run(
            ['git', 'show-ref', '--verify', '--quiet', 'refs/heads/main']
        ).returncode == 0 else 'refs/remotes/origin/main'
        base = run('git', 'merge-base', '--all', reference, 'HEAD').strip()
    if not re.fullmatch('[0-9a-f]{40}', base):
        raise ValueError('CI validation unresolved: a unique pinned comparison base is required.')
    changed = subprocess.check_output(
        ['git', 'diff', '--name-only', '-z', '--no-renames', base, 'HEAD', '--']
    ).decode('utf-8').split('\0')[:-1]
    for path in changed:
        if path.startswith(('.github/', 'altdoc/', '.codex/', '.claude/')) and not (
                policy.repository_artifact(path) or re.fullmatch(policy.CHECKING, path)):
            raise ValueError('CI validation unresolved: no artifact contract for ' + repr(path) +
                             '. Full package checking cannot validate it.')
    config = Path('.claude/settings.json')
    if config.exists():
        json.loads(config.read_text())
    paths = sorted(str(path) for path in Path('.github/workflows').glob('*')
                   if path.suffix in ('.yml', '.yaml'))
    expected_paths = sorted('.github/workflows/' + name for name in policy.WORKFLOWS)
    if paths != expected_paths:
        raise ValueError('CI validation unresolved: workflow inventory differs from supported contracts.')
    actionlint = shutil.which('actionlint')
    if not actionlint:
        raise ValueError('CI validation unresolved: install actionlint; package checking is no fallback.')
    print(run(actionlint, '-shellcheck=', '-pyflakes=', *paths), end='')
    print('Completed CI artifact verification: actionlint workflow syntax', flush=True)
    actual = {path: executable_contract(value) for path, value in workflows(paths).items()}
    expected = json.loads(Path('tools/ci-workflow-contracts.json').read_text())
    if actual != expected:
        different = sorted(path for path in set(actual) | set(expected)
                           if actual.get(path) != expected.get(path))
        raise ValueError('Workflow executable contract changed (conditions, environment, steps, '
                         'triggers and setup are protected): ' + ', '.join(different) +
                         '. Extend the contract and fixtures under Full; CI validation remains unresolved.')
    print('Completed CI artifact verification: workflow executable contracts', flush=True)
    print(run('Rscript', '.github/scripts/verify-verifier-invocation.R'), end='')
    print('Completed CI artifact verification: verifier-invocation wiring', flush=True)


if __name__ == '__main__':
    try:
        verify()
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        output = getattr(error, 'output', '')
        sys.exit(str(output) + '\n' + str(error))
