# Command record for issue #745

The historical scratch root was `/private/tmp/marginplyr-745-20260930`.
These commands describe the completed run; the prior investigation supplied
the isolated `lib-r45near`, `lib-r45`, and `lib-control` dependency libraries
and the case-local R 4.5.2 wrapper. Run them against a new scratch root for a
replay rather than reusing a live database or library.

```sh
set -eu
root=$(mktemp -d /private/tmp/marginplyr-745-replay-XXXXXX)
mkdir -p "$root/pgdata" "$root/socket" "$root/sources" \
  "$root/near-lib" "$root/current-r45-lib" "$root/current-lib" \
  "$root/stage" "$root/logs"
git archive 032690964f6c94a1430c3c094968309bd6ee17eb | tar -x -C "$root/stage"
R CMD build --no-build-vignettes --no-manual "$root/stage"
mv marginplyr_0.1.0.tar.gz "$root/sources/"
for spec in hms_1.1.4 timechange_0.4.0 lubridate_1.9.5 RPostgres_1.4.10; do
  package=${spec%%_*}
  curl -fsSL --retry 2 -o "$root/sources/$spec.tar.gz" \
    "https://cran.r-project.org/src/contrib/$spec.tar.gz" ||
    curl -fsSL --retry 2 -o "$root/sources/$spec.tar.gz" \
      "https://cran.r-project.org/src/contrib/Archive/$package/$spec.tar.gz"
done
shasum -a 256 "$root"/sources/*.tar.gz

initdb -D "$root/pgdata" -A trust -U marginprobe --no-instructions
pg_ctl -D "$root/pgdata" -l "$root/logs/postgres.log" \
  -o "-k $root/socket -p 55475 -c listen_addresses=''" start
createdb -h "$root/socket" -p 55475 -U marginprobe marginprobe
psql -h "$root/socket" -p 55475 -U marginprobe -d marginprobe \
  -Atqc 'select version()'
```

The four driver/transitive archives were fetched from CRAN's `src/contrib`
or versioned `Archive/<package>` paths. `source-archives.csv` records their
names and SHA-256 hashes. The following loop installs them and the marginplyr
tarball into each case overlay, audits its graph, and runs the public probe:

```sh
for case_name in near current-r45 current-r46; do
  case "$case_name" in
    near)
      rbin=/private/tmp/marginplyr-minver-kalaij/r45-home/bin/R
      base=/private/tmp/marginplyr-minver-kalaij/lib-r45near
      overlay="$root/near-lib"
      install_name=near
      probe_dir=near
      ;;
    current-r45)
      rbin=/private/tmp/marginplyr-minver-kalaij/r45-home/bin/R
      base=/private/tmp/marginplyr-minver-kalaij/lib-r45
      overlay="$root/current-r45-lib"
      install_name=current-r45
      probe_dir=current-r45
      ;;
    current-r46)
      rbin=/usr/local/bin/R
      base=/private/tmp/marginplyr-minver-kalaij/lib-control
      overlay="$root/current-lib"
      install_name=current
      probe_dir=current
      ;;
  esac
  mkdir -p "$overlay" "$root/probe-$probe_dir"
  export R_LIBS="$overlay:$base" R_LIBS_USER="$overlay:$base" R_LIBS_SITE="$overlay:$base"
  export R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null
  export PG_CONFIG=/opt/homebrew/bin/pg_config
  if [ "$case_name" = current-r46 ]; then
    unset R_MAKEVARS_USER
  else
    export R_MAKEVARS_USER=/private/tmp/marginplyr-minver-kalaij/Makevars-r45
  fi
  for spec in hms_1.1.4 timechange_0.4.0 lubridate_1.9.5 RPostgres_1.4.10 marginplyr_0.1.0; do
    "$rbin" CMD INSTALL --library="$overlay" "$root/sources/$spec.tar.gz" \
      > "$root/logs/install-$install_name-$spec.log" 2>&1
  done
  "$rbin" --vanilla --slave \
    -f investigation/2026-09-30-postgres-near-floor/audit-graph.R \
    --args "$root/probe-$probe_dir" \
    > "$root/logs/audit-$case_name.log" 2>&1
  "$rbin" --vanilla --slave \
    -f investigation/2026-09-30-postgres-near-floor/probe.R \
    --args "$case_name" "$root/probe-$probe_dir" "$root/socket" \
    > "$root/logs/probe-$case_name.log" 2>&1
done
```

The R 4.6.1 scratch overlay and probe directory used `current` while the
preserved artifact directory uses `current-r46`. Committed text logs have only
trailing whitespace and surplus final blank lines normalized. The comparisons
were:

```sh
Rscript --vanilla investigation/2026-09-30-postgres-near-floor/compare.R \
  "$root/probe-near" "$root/probe-current-r45" "$root/comparison-r45.csv"
Rscript --vanilla investigation/2026-09-30-postgres-near-floor/compare.R \
  "$root/probe-near" "$root/probe-current" "$root/comparison-r46.csv"
Rscript --vanilla -e 'devtools::test()' > "$root/logs/full-suite.log" 2>&1
pg_ctl -D "$root/pgdata" stop
```
