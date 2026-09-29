# Replaying issue #744

The historical scratch root was `/private/tmp/marginplyr-744`. Use a new
scratch directory for a new run. The R 4.5.2 control library and baseline
tarball were created by the [earlier investigation](../2026-09-29-minimum-dependency-compatibility.md).
The source tarball was built from commit
`b0a1fa6c77ae8a691c179f7bfd0d73da54325ae0`; its recorded SHA-256 is in
`source-manifest.csv`. If the earlier artifact is gone, rebuild the source
package from that commit in a disposable checkout and record the new tarball
hash before using the scripts below.

For a rebuilt tarball, copy `source-manifest.csv` into the scratch directory,
replace only the `marginplyr` row's `sha256` with `shasum -a 256` of that new
tarball, then replace the `manifest=...` line below with
`manifest="$scratch/source-manifest.csv"`. Preserve the checked-in manifest as
the record of this run; the rebuilt artifact is a distinct replay.

These commands run from the repository root on macOS arm64. `replay-setup.py`
downloads and checksums the R 4.1.3 installer and all 30 selected source
archives, expands R only under the scratch directory, redirects both R wrapper
scripts to that framework, and creates a case-local `Makevars-r41`. With
`--install`, it installs every source in manifest order and writes a separate
log with the command and source hash for each package. It does not install R
or packages into normal libraries.

```sh
scratch=/private/tmp/marginplyr-744
baseline=/private/tmp/marginplyr-minver-kalaij/artifacts/marginplyr_0.1.0.tar.gz
artifact=investigation/2026-09-29-r41-public-results
manifest=${manifest:-"$artifact/source-manifest.csv"}
baseline_sha=$(python3 -c 'import csv,sys; print(next(row["sha256"] for row in csv.DictReader(open(sys.argv[1])) if row["package"] == "marginplyr"))' "$manifest")
python3 "$artifact/replay-setup.py" "$scratch" "$baseline" --manifest "$manifest"
```

Set the isolation variables before R 4.1 commands:

```sh
export DYLD_LIBRARY_PATH="$scratch/pkg-expanded/R-fw.pkg/Payload/R.framework/Versions/4.1-arm64/Resources/lib"
export R_LIBS="$scratch/lib-r41" R_LIBS_USER="$scratch/lib-r41" R_LIBS_SITE="$scratch/lib-r41"
export R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null
export R_MAKEVARS_USER="$scratch/Makevars-r41"
"$scratch/R41" --vanilla --slave -e 'print(R.version.string); print(.libPaths())'
"$scratch/R41" --vanilla --slave -f "$artifact/audit-graph.R" --args \
  source "$manifest" "$scratch/sources" "$scratch/source-graph-check.csv"
python3 "$artifact/replay-setup.py" "$scratch" "$baseline" --manifest "$manifest" --install
"$scratch/R41" --vanilla --slave -f "$artifact/audit-graph.R" --args \
  installed "$manifest" "$scratch/lib-r41" "$scratch/installed-graph-check.csv"
```

The original R 4.1.3 run used the same archives and installation order, then
audited the installed packages. A source-identity confirmation reran the
marginplyr installation after checking the tarball hash and recorded the
installed and loaded copy in `install-identity-r41.log`:

```sh
"$artifact/verify-install-source.sh" "$baseline" \
  "$baseline_sha" \
  b0a1fa6c77ae8a691c179f7bfd0d73da54325ae0 \
  "$scratch/R41" "$scratch/lib-r41" "$scratch/install-identity-r41.log"
```

For the R 4.5.2 control, use its separately isolated library and the
`r45-home/bin/R` wrapper from the earlier experiment. The explicit `env`
invocations below keep its R 4.5.2 library separate from the R 4.1.3
settings. Fresh-process source confirmation, probes, and comparison were:

```sh
"$scratch/R41" --vanilla --slave -f "$artifact/probe.R" --args \
  "$scratch/lib-r41" "$scratch/case-r41"
control_lib=/private/tmp/marginplyr-minver-kalaij/lib-r45near
control_r=/private/tmp/marginplyr-minver-kalaij/r45-home/bin/R
env -u DYLD_LIBRARY_PATH \
  R_MAKEVARS_USER=/private/tmp/marginplyr-minver-kalaij/Makevars-r45 \
  R_LIBS="$control_lib" R_LIBS_USER="$control_lib" R_LIBS_SITE="$control_lib" \
  "$artifact/verify-install-source.sh" "$baseline" \
  "$baseline_sha" \
  b0a1fa6c77ae8a691c179f7bfd0d73da54325ae0 \
  "$control_r" "$control_lib" "$scratch/install-identity-r45-control.log"
env -u DYLD_LIBRARY_PATH -u R_MAKEVARS_USER \
  R_LIBS="$control_lib" R_LIBS_USER="$control_lib" R_LIBS_SITE="$control_lib" \
  "$control_r" --vanilla --slave \
  -f "$artifact/probe.R" --args \
  "$control_lib" "$scratch/control"
Rscript --vanilla "$artifact/compare.R" \
  "$scratch/control/results.rds" "$scratch/case-r41/results.rds" \
  "$scratch/comparison.csv"
```

The R 4.1.3 `probe.R` process writes the R version, `.libPaths()`, loaded
namespace versions and paths, case results, and diagnostics. The R 4.5.2
process writes the same fields to a separate directory. The committed CSVs
and logs are copies of those outputs with line endings and trailing spaces
normalized for version control.

## Harness failure reproduction

Before `replay-setup.py` redirected the *internal* extracted
`Resources/bin/R` wrapper, the outer `R41` wrapper selected R 4.1.3 but
`R CMD INSTALL` invoked the internal wrapper, which pointed to the host's
`/Library/Frameworks/R.framework/Resources/include`. With only the outer
wrapper redirected, this command reproduced the first failure:

```sh
"$scratch/R41" CMD INSTALL --library="$scratch/lib-r41" \
  "$scratch/sources/cli_3.6.2.tar.gz"
```

The compiler then reported undeclared `Rf_findVar` and `STRING_PTR` from
`cli` sources. The saved [failure excerpt](harness-wrapper-failures.md)
records that diagnostic and the separate R 4.5.2 wrapper mistake found while
confirming source identity. Both resolved by using a case-local internal
wrapper; neither reached a marginplyr public call.
