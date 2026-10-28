#!/bin/sh
set -eu
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 FRESH_OUTPUT_DIRECTORY" >&2
  exit 2
fi
out=$1
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
mkdir -p "$out"
if [ -e "$out/capture-started.txt" ] || [ -e "$out/sites-matrix.rds" ] || [ -e "$out/sites-failures.rds" ]; then
  echo 'Choose a fresh output directory' >&2
  exit 2
fi
date -u '+%Y-%m-%dT%H:%M:%SZ' > "$out/capture-started.txt"
cluster=$(mktemp -d /private/tmp/sql-audit-sites-pg.XXXXXX)
cleanup() {
  code=$?
  trap - EXIT HUP INT TERM
  if [ -f "$cluster/data/postmaster.pid" ]; then
    pg_ctl -D "$cluster/data" -m immediate -w stop >> "$out/stop.log" 2>&1 || true
  fi
  cp "$cluster/server.log" "$out/server.log" 2>/dev/null || true
  rm -rf "$cluster"
  exit "$code"
}
trap cleanup EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM
mkdir "$cluster/socket"
initdb -D "$cluster/data" -A trust --no-locale -E UTF8 -c dynamic_shared_memory_type=posix -c shared_memory_type=mmap > "$out/initdb.log" 2>&1
pg_ctl -D "$cluster/data" -l "$cluster/server.log" -o "-k $cluster/socket -p 56281 -c listen_addresses=" -w start > "$out/start.log" 2>&1
export PGUSER
PGUSER=$(id -un)
for script in ${SQL_AUDIT_SCRIPTS:-sites-matrix sites-failures}; do
  Rscript "$script_dir/$script.R" "$cluster/socket" 56281 "$out" > "$out/$script.log" 2>&1
done
date -u '+%Y-%m-%dT%H:%M:%SZ' > "$out/capture-completed.txt"
