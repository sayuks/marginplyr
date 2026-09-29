# Commands used for issue #744

The historical scratch root was `/private/tmp/marginplyr-744`. The R 4.5.2
control library and baseline source tarball came from the adjacent
`/private/tmp/marginplyr-minver-kalaij` experiment. These absolute paths are
observations, not installation requirements. The checked-in `source-manifest.csv`
fixes the 30 source versions, URLs, SHA-256 hashes, and installation order.

```sh
mkdir -p /private/tmp/marginplyr-744
curl -fL --retry 3 --output /private/tmp/marginplyr-744/R-4.1.3-arm64.pkg \
  https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.1.3-arm64.pkg
shasum -a 256 /private/tmp/marginplyr-744/R-4.1.3-arm64.pkg
pkgutil --expand-full /private/tmp/marginplyr-744/R-4.1.3-arm64.pkg \
  /private/tmp/marginplyr-744/pkg-expanded
```

The expanded framework's `Resources/bin/R` has a compiled-in wrapper path to
`/Library/Frameworks/R.framework/Resources`. The experiment copied it to
`/private/tmp/marginplyr-744/R41` and replaced that path in both copies with
`/private/tmp/marginplyr-744/pkg-expanded/R-fw.pkg/Payload/R.framework/Versions/4.1-arm64/Resources`.
It then wrote this disposable `Makevars-r41`:

```make
LIBR = -L/private/tmp/marginplyr-744/pkg-expanded/R-fw.pkg/Payload/R.framework/Versions/4.1-arm64/Resources/lib -lR
```

All R 4.1 commands used these environment variables (and fresh processes):

```sh
export DYLD_LIBRARY_PATH=/private/tmp/marginplyr-744/pkg-expanded/R-fw.pkg/Payload/R.framework/Versions/4.1-arm64/Resources/lib
export R_LIBS=/private/tmp/marginplyr-744/lib-r41
export R_LIBS_USER=/private/tmp/marginplyr-744/lib-r41
export R_LIBS_SITE=/private/tmp/marginplyr-744/lib-r41
export R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null
export R_MAKEVARS_USER=/private/tmp/marginplyr-744/Makevars-r41
/private/tmp/marginplyr-744/R41 --vanilla --slave -e 'print(R.version.string); print(.libPaths())'
```

Each CRAN URL in `source-manifest.csv` was downloaded into a separate `sources/`
file and its SHA-256 verified before installation. The marginplyr row used the
historical `marginplyr_0.1.0.tar.gz` from source commit
`b0a1fa6c77ae8a691c179f7bfd0d73da54325ae0`. For a new host without that
artifact, archive that commit into a disposable directory and build a fresh
source tarball; retain the commit identity and record the new tarball's hash.
The source `DESCRIPTION` fields were audited against R 4.1.3 and the selected
versions before installation. The recorded row-by-row verdict is
`source-graph-check.csv`. Installation followed `source-manifest.csv` order:

```sh
/private/tmp/marginplyr-744/R41 CMD INSTALL \
  --library=/private/tmp/marginplyr-744/lib-r41 \
  /private/tmp/marginplyr-744/sources/cli_3.6.2.tar.gz
# Repeat for each subsequent manifest row; the marginplyr row uses its baseline tarball.
```

The installed graph was audited again with the same constraint scanner used in
the [earlier experiment](../2026-09-29-minimum-dependency-compatibility.md):

```sh
HUNT_ROOT=/private/tmp/marginplyr-744 HUNT_MODE=r41 \
HUNT_LIB=/private/tmp/marginplyr-744/lib-r41 \
/private/tmp/marginplyr-744/R41 --vanilla --slave \
  -f /private/tmp/marginplyr-minver-kalaij/audit-deps.R
```

From the repository root, the public probes and comparison were:

```sh
/private/tmp/marginplyr-744/R41 --vanilla --slave \
  -f investigation/2026-09-29-r41-public-results/probe.R --args \
  /private/tmp/marginplyr-744/lib-r41 /private/tmp/marginplyr-744/case-r41
# In a separate shell with R_LIBS, R_LIBS_USER, and R_LIBS_SITE set to
# /private/tmp/marginplyr-minver-kalaij/lib-r45near:
/private/tmp/marginplyr-minver-kalaij/R45 --vanilla --slave \
  -f investigation/2026-09-29-r41-public-results/probe.R --args \
  /private/tmp/marginplyr-minver-kalaij/lib-r45near \
  /private/tmp/marginplyr-744/control
Rscript --vanilla investigation/2026-09-29-r41-public-results/compare.R \
  /private/tmp/marginplyr-744/control/results.rds \
  /private/tmp/marginplyr-744/case-r41/results.rds \
  /private/tmp/marginplyr-744/comparison.csv
```

The R 4.1 `DYLD_LIBRARY_PATH` is unset for the R 4.5.2 control. The probe
writes environment, loaded-namespace, and RDS result files in each output
directory. Its console output and the audit/install terminal summaries were
copied into this directory as logs.
