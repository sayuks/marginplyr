"""Verify workflow syntax, executable contracts and verifier invocation wiring.

This CI check reads the current sources without a change classifier, pinned
comparison base, installed launcher or package-check history. Intentional
workflow changes update the protected contract and its command fixtures.
"""
import json
from pathlib import Path
import shutil
import subprocess
import sys


def run(*args):
    return subprocess.check_output(args, text=True, stderr=subprocess.STDOUT)


def workflows(paths):
    # YAML 1.1 would collapse yes and true; keep scalar spellings and lists.
    code = ('p <- commandArgs(TRUE); '
            'tags <- c("bool#yes", "bool#no", "int", "int#hex", "int#oct", '
            '"int#base60", "float#fix", "float#exp", "float#base60", '
            '"float#inf", "float#neginf", "float#nan", "seq"); '
            'h <- setNames(rep(list(identity), length(tags)), tags); '
            'x <- setNames(lapply(p, yaml::yaml.load_file, eval.expr = FALSE, handlers = h), p); '
            'cat(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))')
    return json.loads(run('Rscript', '-e', code, *paths))


def verify():
    config = Path('.claude/settings.json')
    if config.exists():
        json.loads(config.read_text())
    paths = sorted(str(path) for path in Path('.github/workflows').glob('*')
                   if path.suffix in ('.yml', '.yaml'))
    expected = json.loads(Path('tools/ci-workflow-contracts.json').read_text())
    expected_paths = sorted(expected)
    if paths != expected_paths:
        raise ValueError('CI validation unresolved: workflow inventory differs from supported contracts.')
    actionlint = shutil.which('actionlint')
    if not actionlint:
        raise ValueError('CI validation unresolved: install actionlint; package checking is no fallback.')
    print(run(actionlint, '-shellcheck=', '-pyflakes=', *paths), end='')
    print('Completed CI artifact verification: actionlint workflow syntax', flush=True)
    actual = workflows(paths)
    if actual != expected:
        different = sorted(path for path in set(actual) | set(expected)
                           if actual.get(path) != expected.get(path))
        raise ValueError('Workflow executable contract changed (conditions, environment, steps, '
                         'triggers, names and setup are protected): ' + ', '.join(different) +
                         '. Update the contract and fixtures for an intentional workflow change.')
    print('Completed CI artifact verification: workflow executable contracts', flush=True)
    print(run('Rscript', '.github/scripts/verify-verifier-invocation.R'), end='')
    print('Completed CI artifact verification: verifier-invocation wiring', flush=True)


if __name__ == '__main__':
    try:
        verify()
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        output = getattr(error, 'output', '')
        sys.exit(str(output) + '\n' + str(error))
