#!/usr/bin/env python3
"""Install the agent-neutral command for the current OS user."""
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


def main():
    if sys.platform not in ('darwin', 'linux'):
        sys.exit('Installation requires macOS or Linux. No check was run.')
    import pwd
    home = Path(pwd.getpwuid(os.getuid()).pw_dir)
    destination = home / '.local/bin/marginplyr-review-ready'
    source = Path(__file__).with_name('review-ready-admission.py')
    # Installation must not weaken an existing pending/unfinished request.
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=destination.parent, delete=False) as stream:
        temporary = Path(stream.name)
        stream.write(source.read_bytes())
    try:
        temporary.chmod(0o755)
        os.replace(temporary, destination)
    finally:
        temporary.unlink(missing_ok=True)
    subprocess.run([str(destination), '--status'], check=True)
    print('Installed: ' + str(destination))
    if shutil.which('marginplyr-review-ready') != str(destination):
        print('Add ' + str(destination.parent) + ' to PATH in every execution environment,')
        print('or invoke the installed command by that absolute path. Refresh existing sessions.')


if __name__ == '__main__':
    main()
