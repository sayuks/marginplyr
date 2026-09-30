# Signaling evidence

The original files from `evidence.rds` are stored directly here. `SHA256SUMS`
records their hashes. `archive.dput` retains the archive's date and repository
baseline; `environment.dput` retains the nested R environment metadata with its
classes and attributes. Read either with `dget()`.

The scripts and observations retain their original bytes, including source
whitespace and the script hashes in `manifest.json`. The
[conversion investigation](../rds-text-conversion-2026-09-30.md) records the
source snapshot and preservation checks.

To inspect integrity, run from this directory:

```sh
shasum -a 256 -c SHA256SUMS
```

To rerun with R, rlang, jsonlite and Python, copy the scripts to a disposable
directory so the historical observations are not overwritten:

```sh
replay=$(mktemp -d /private/tmp/condition-resignaling-XXXXXX)
cp probe.R run.py "$replay/"
(cd "$replay" && python3 run.py)
```

The supervisor sends no OS signal. Its new observations describe the local
environment; the [investigation note](../condition-resignaling-2026-09-30.md)
holds the scope and limits of the original measurements.
