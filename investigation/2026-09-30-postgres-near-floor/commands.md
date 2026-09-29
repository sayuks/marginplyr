# Command record for issue #745

The historical scratch root was `/private/tmp/marginplyr-745-20260930`.
These commands describe the completed run; the prior investigation supplied
the isolated `lib-r45near`, `lib-r45`, and `lib-control` dependency libraries
and the case-local R 4.5.2 wrapper. Run them against a new scratch root for a
replay rather than reusing a live database or library.

```sh
root=/private/tmp/marginplyr-745-20260930
mkdir -p "$root"/{pgdata,socket,sources,near-lib,current-r45-lib,current-lib,stage,logs}
git archive 032690964f6c94a1430c3c094968309bd6ee17eb | tar -x -C "$root/stage"
R CMD build --no-build-vignettes --no-manual "$root/stage"
mv marginplyr_0.1.0.tar.gz "$root/sources/"
shasum -a 256 "$root/sources/marginplyr_0.1.0.tar.gz"

initdb -D "$root/pgdata" -A trust -U marginprobe --no-instructions
pg_ctl -D "$root/pgdata" -l "$root/logs/postgres.log" \
  -o "-k $root/socket -p 55475 -c listen_addresses=''" start
createdb -h "$root/socket" -p 55475 -U marginprobe marginprobe
psql -h "$root/socket" -p 55475 -U marginprobe -d marginprobe \
  -Atqc 'select version()'
```

The four driver/transitive archives were fetched from CRAN's `src/contrib`
or versioned `Archive/<package>` paths. `source-archives.csv` records their
names and SHA-256 hashes. For each of `near`, `current-r45`, and `current-r46`,
the following case command installed those archives followed by the one
marginplyr tarball. Values of `rbin`, `base`, and `overlay` were respectively:

| Case | `rbin` | `base` | `overlay` |
| --- | --- | --- | --- |
| near | `/private/tmp/marginplyr-minver-kalaij/r45-home/bin/R` | `/private/tmp/marginplyr-minver-kalaij/lib-r45near` | `$root/near-lib` |
| current-r45 | `/private/tmp/marginplyr-minver-kalaij/r45-home/bin/R` | `/private/tmp/marginplyr-minver-kalaij/lib-r45` | `$root/current-r45-lib` |
| current-r46 | `/usr/local/bin/R` | `/private/tmp/marginplyr-minver-kalaij/lib-control` | `$root/current-lib` |

```sh
export R_LIBS="$overlay:$base" R_LIBS_USER="$overlay:$base" R_LIBS_SITE="$overlay:$base"
export R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null
export PG_CONFIG=/opt/homebrew/bin/pg_config
# For the R 4.5.2 cases only:
export R_MAKEVARS_USER=/private/tmp/marginplyr-minver-kalaij/Makevars-r45
for spec in hms_1.1.4 timechange_0.4.0 lubridate_1.9.5 RPostgres_1.4.10 marginplyr_0.1.0; do
  "$rbin" CMD INSTALL --library="$overlay" "$root/sources/$spec.tar.gz" \
    > "$root/logs/install-$case_name-$spec.log" 2>&1
done

"$rbin" --vanilla --slave \
  -f investigation/2026-09-30-postgres-near-floor/audit-graph.R \
  --args "$root/probe-$case_name"
"$rbin" --vanilla --slave \
  -f investigation/2026-09-30-postgres-near-floor/probe.R \
  --args "$case_name" "$root/probe-$case_name" "$root/socket"
```

The R 4.6.1 scratch overlay and probe directory used the shorter name
`current` while the preserved artifact directory uses `current-r46`; its
`probe.R` case argument was `current-r46`. The case logs and saved environment
files preserve the exact paths. Committed text logs have only trailing
whitespace and surplus final blank lines normalized. The comparisons were:

```sh
Rscript --vanilla investigation/2026-09-30-postgres-near-floor/compare.R \
  "$root/probe-near" "$root/probe-current-r45" "$root/comparison-r45.csv"
Rscript --vanilla investigation/2026-09-30-postgres-near-floor/compare.R \
  "$root/probe-near" "$root/probe-current" "$root/comparison-r46.csv"
Rscript --vanilla -e 'devtools::test()' > "$root/logs/full-suite.log" 2>&1
pg_ctl -D "$root/pgdata" stop
```
