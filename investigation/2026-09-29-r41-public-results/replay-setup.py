"""Prepare and optionally install the recorded R 4.1.3 source graph on macOS arm64."""

import argparse
import csv
import hashlib
import os
from pathlib import Path
import shutil
import subprocess

HERE = Path(__file__).resolve().parent
R_PKG_URL = "https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.1.3-arm64.pkg"
R_PKG_SHA = "d973134c1417afeb8c54a8bd0b53ddbc47719e0e30fd9c2122a71d13a57106c4"
OLD_HOME = "/Library/Frameworks/R.framework/Resources"


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def fetch(url, target, expected):
    if target.exists() and sha256(target) != expected:
        target.unlink()
    if not target.exists():
        subprocess.run(["curl", "-fL", "--retry", "3", "--output", str(target), url], check=True)
    if sha256(target) != expected:
        raise RuntimeError(f"SHA-256 mismatch: {target}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("root", type=Path, help="Disposable scratch directory")
    parser.add_argument("baseline_tarball", type=Path, help="Tarball built from b0a1fa6c77ae8a691c179f7bfd0d73da54325ae0")
    parser.add_argument("--manifest", type=Path, default=HERE / "source-manifest.csv", help="Source manifest; use a scratch copy for rebuilt tarballs")
    parser.add_argument("--install", action="store_true", help="Install all selected sources after preparation")
    args = parser.parse_args()
    root = args.root.resolve()
    root.mkdir(parents=True, exist_ok=True)
    rows = list(csv.DictReader(args.manifest.open()))
    expected_baseline = next(x["sha256"] for x in rows if x["package"] == "marginplyr")
    if sha256(args.baseline_tarball) != expected_baseline:
        raise RuntimeError("Baseline tarball differs from the recorded source identity")

    installer = root / "R-4.1.3-arm64.pkg"
    fetch(R_PKG_URL, installer, R_PKG_SHA)
    expanded = root / "pkg-expanded"
    if not expanded.exists():
        subprocess.run(["pkgutil", "--expand-full", str(installer), str(expanded)], check=True)
    home = expanded / "R-fw.pkg/Payload/R.framework/Versions/4.1-arm64/Resources"
    if not (home / "bin/exec/R").exists():
        raise RuntimeError("R 4.1.3 framework missing from expanded installer")
    for wrapper in (home / "bin/R", root / "R41"):
        if not wrapper.exists():
            shutil.copyfile(home / "bin/R", wrapper)
        text = wrapper.read_text().replace(OLD_HOME, str(home))
        wrapper.write_text(text)
        wrapper.chmod(0o755)
    (root / "Makevars-r41").write_text(f"LIBR = -L{home / 'lib'} -lR\n")

    sources = root / "sources"
    sources.mkdir(exist_ok=True)
    for row in rows:
        target = sources / f"{row['package']}_{row['version']}.tar.gz"
        if row["package"] == "marginplyr":
            if not target.exists() or sha256(target) != row["sha256"]:
                shutil.copyfile(args.baseline_tarball, target)
        else:
            fetch(row["source"], target, row["sha256"])
        if sha256(target) != row["sha256"]:
            raise RuntimeError(f"SHA-256 mismatch: {target}")
        print(f"verified {target.name} {row['sha256']}")

    if not args.install:
        return
    lib = root / "lib-r41"
    lib.mkdir(exist_ok=True)
    logs = root / "logs"
    logs.mkdir(exist_ok=True)
    env = dict(os.environ)
    env.update(
        DYLD_LIBRARY_PATH=str(home / "lib"), R_LIBS=str(lib),
        R_LIBS_USER=str(lib), R_LIBS_SITE=str(lib),
        R_PROFILE_USER="/dev/null", R_ENVIRON_USER="/dev/null",
        R_MAKEVARS_USER=str(root / "Makevars-r41"), MAKEFLAGS="-j4",
    )
    for row in rows:
        target = sources / f"{row['package']}_{row['version']}.tar.gz"
        command = [str(root / "R41"), "CMD", "INSTALL", f"--library={lib}", str(target)]
        with (logs / f"install-{row['package']}.log").open("w") as log:
            log.write(f"source_sha256={sha256(target)}\ncommand={' '.join(command)}\n")
            log.flush()
            subprocess.run(command, env=env, stdout=log, stderr=subprocess.STDOUT, check=True)
        print(f"installed {row['package']} {row['version']}")


if __name__ == "__main__":
    main()
