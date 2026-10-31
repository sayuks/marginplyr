#!/usr/bin/env python3
"""Install the agent-neutral command for the current OS user."""
import os
import argparse
import hashlib
import importlib.machinery
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--verify', action='store_true', help='compare the installed command without writing')
    options = parser.parse_args()
    if sys.platform not in ('darwin', 'linux'):
        sys.exit('Installation requires macOS or Linux. No check was run.')
    import pwd
    home = Path(pwd.getpwuid(os.getuid()).pw_dir)
    destination = home / '.local/bin/marginplyr-review-ready'
    source = Path(__file__).with_name('review-ready-admission.py')
    admission = importlib.machinery.SourceFileLoader('admission_installer', str(source)).load_module()
    installed = admission.installed_source(source.parent.parent)
    if options.verify:
        if not destination.exists() or destination.read_bytes() != installed:
            sys.exit('Installed Review-ready command differs; refresh installation before checking.')
        print('Verified installed supervision protocol: 3; SHA-256: ' + hashlib.sha256(installed).hexdigest())
        return
    # Installation must not weaken an existing pending/unfinished request.
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=destination.parent, delete=False) as stream:
        temporary = Path(stream.name)
        stream.write(installed)
    try:
        temporary.chmod(0o755)
        os.replace(temporary, destination)
    finally:
        temporary.unlink(missing_ok=True)
    subprocess.run([str(destination), '--status'], check=True)
    print('Installed: ' + str(destination))
    print('Supervision protocol: 3; SHA-256: ' + hashlib.sha256(installed).hexdigest())
    if shutil.which('marginplyr-review-ready') != str(destination):
        print('Add ' + str(destination.parent) + ' to PATH in every execution environment,')
        print('or invoke the installed command by that absolute path. Refresh existing sessions.')


if __name__ == '__main__':
    main()
