#!/usr/bin/env python3
"""Verify this archive and reconstruct throwaway source trees without editing it."""
import argparse
import csv
import hashlib
import io
import json
from pathlib import Path
import shutil
import subprocess
import tarfile

ARCHIVE = Path(__file__).resolve().parent
OLD_ROOT = '/private/tmp/marginplyr-sqlite-design-20260925'
VARIANTS = ('A', 'B', 'B-direct', 'C', 'A661-stop')


def verify():
    entries = []
    for line in (ARCHIVE / 'SHA256SUMS').read_text().splitlines():
        digest, name = line.split('  ', 1)
        path = ARCHIVE / name
        if hashlib.sha256(path.read_bytes()).hexdigest() != digest:
            raise SystemExit('Checksum mismatch: ' + name)
        entries.append(name)
    actual = {str(p.relative_to(ARCHIVE)) for p in ARCHIVE.rglob('*')
              if p.is_file() and p.name != 'SHA256SUMS'}
    if actual != set(entries):
        raise SystemExit('Manifest inventory mismatch')
    print('Verified ' + str(len(entries)) + ' archive files')


def extract_base(data, target):
    target.mkdir(parents=True, exist_ok=True)
    with tarfile.open(fileobj=io.BytesIO(data), mode='r:') as archive:
        for member in archive.getmembers():
            path = Path(member.name)
            if path.is_absolute() or '..' in path.parts or member.issym() or member.islnk():
                raise SystemExit('Unsupported archive member: ' + member.name)
        archive.extractall(target)


def prepare(scratch, repo):
    if scratch.exists() and any(scratch.iterdir()):
        raise SystemExit('Scratch directory must be new or empty: ' + str(scratch))
    if ARCHIVE == scratch or ARCHIVE in scratch.parents or scratch in ARCHIVE.parents:
        raise SystemExit('Scratch directory must be outside this evidence archive')
    base = (ARCHIVE / 'BASE_COMMIT').read_text().strip()
    data = subprocess.check_output(['git', '-C', str(repo), 'archive', '--format=tar', base])
    extract_base(data, scratch)
    for name in VARIANTS:
        target = scratch / 'variants' / name
        extract_base(data, target)
        shutil.rmtree(target / 'R')
        shutil.copytree(ARCHIVE / 'final-sources' / name / 'R', target / 'R')
    shutil.copytree(ARCHIVE / 'review', scratch / 'original-review')
    adaptations = []
    # Scripts write their relative output paths into review/. Historical outputs
    # remain exclusively in original-review/ and are never used as replay results.
    for source in sorted((ARCHIVE / 'review').rglob('*')):
        relative = source.relative_to(ARCHIVE / 'review')
        target = scratch / 'review' / relative
        if source.is_dir():
            target.mkdir(parents=True, exist_ok=True)
        elif source.suffix in ('.R', '.py'):
            target.parent.mkdir(parents=True, exist_ok=True)
            text = source.read_text()
            replacements = text.count(OLD_ROOT)
            target.write_text(text.replace(OLD_ROOT, str(scratch)))
            if replacements:
                adaptations.append({'script': str(relative), 'replacements': replacements})
    for name in ['BASE_COMMIT'] + ['issue-' + str(n) + '.json' for n in range(661, 667)]:
        shutil.copy2(ARCHIVE / name, scratch / name)
    (scratch / 'replay-adaptations.json').write_text(json.dumps({
        'base': base, 'original_root': OLD_ROOT, 'replay_root': str(scratch),
        'changes': adaptations,
        'note': 'Literal root-path substitutions only, in executable script copies.'
    }, indent=2) + '\n')
    print('Prepared ' + str(scratch))


def cross_check(scratch, variant):
    package = scratch if variant == 'baseline' else scratch / 'variants' / variant
    output = scratch / 'review' / variant
    output.mkdir(parents=True, exist_ok=True)
    command = ['Rscript', str(scratch / 'review' / 'cross-check.R'), str(package), str(output)]
    with (output / 'cross-check.log').open('w') as log:
        subprocess.run(command, cwd=scratch, stdout=log, stderr=subprocess.STDOUT, check=True)
    with (output / 'cross-check.csv').open() as source:
        rows = list(csv.DictReader(source))
    passed = sum(row['pass'] == 'TRUE' for row in rows)
    print(variant + ': ' + str(passed) + '/' + str(len(rows)) + ' passing cases')
    # A baseline/C failure is evidence, but the explicitly selected run still
    # returns nonzero so a caller cannot mistake process completion for success.
    if passed != len(rows):
        raise SystemExit(1)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--verify-only', action='store_true')
    parser.add_argument('--scratch', type=Path)
    parser.add_argument('--repo', type=Path, default=ARCHIVE.parents[1])
    parser.add_argument('--run', choices=('baseline',) + VARIANTS[:-1])
    args = parser.parse_args()
    verify()
    if args.verify_only:
        return
    if args.scratch is None:
        parser.error('--scratch is required unless --verify-only is used')
    scratch = args.scratch.resolve()
    prepare(scratch, args.repo.resolve())
    if args.run:
        cross_check(scratch, args.run)


if __name__ == '__main__':
    main()
