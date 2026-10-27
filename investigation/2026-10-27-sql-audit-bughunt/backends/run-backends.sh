#!/bin/sh
# Run from the investigated marginplyr checkout. All server files are disposable.
set -eu
if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
  echo "Usage: $0 OUTPUT_DIRECTORY [POSTGRES_PORT]" >&2
  exit 2
fi
output=$1
port=${2:-56138}
case $port in ''|*[!0-9]*) echo "Port must be numeric" >&2; exit 2 ;; esac
if [ "$port" -lt 1 ] || [ "$port" -gt 65535 ]; then
  echo "Port must be in 1..65535" >&2
  exit 2
fi
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
if ! command -v initdb >/dev/null 2>&1 || ! command -v pg_ctl >/dev/null 2>&1; then
  if command -v pg_config >/dev/null 2>&1; then
    pg_bin=$(pg_config --bindir)
    PATH=$pg_bin:$PATH
    export PATH
  fi
fi
initdb_bin=$(command -v initdb) || exit 1
pg_ctl_bin=$(command -v pg_ctl) || exit 1
rscript_bin=$(command -v Rscript) || exit 1
# initdb owns this cluster as the OS user, independently of a caller's PGUSER.
PGUSER=$(id -un)
export PGUSER
mkdir -p "$output"
output=$(CDPATH= cd -- "$output" && pwd)
if [ -e "$output/capture-started.txt" ]; then
  echo "Choose a fresh output directory: $output already contains a capture" >&2
  exit 2
fi
date -u '+%Y-%m-%dT%H:%M:%SZ' > "$output/capture-started.txt"
cluster=$(mktemp -d /private/tmp/marginplyr-sql-audit-pg.XXXXXX)
cleanup() {
  code=$?
  trap - EXIT HUP INT TERM
  if [ -f "$cluster/data/postmaster.pid" ]; then
    "$pg_ctl_bin" -D "$cluster/data" -m immediate -w stop >> "$output/server-stop.log" 2>&1 || true
  fi
  if [ -f "$cluster/server.log" ]; then
    cp "$cluster/server.log" "$output/postgres-server.log"
  fi
  rm -rf "$cluster"
  exit "$code"
}
trap cleanup EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM
mkdir "$cluster/socket"
"$initdb_bin" -D "$cluster/data" -A trust --no-locale -E UTF8 \
  -c dynamic_shared_memory_type=posix -c shared_memory_type=mmap \
  > "$output/initdb.log" 2>&1
"$pg_ctl_bin" -D "$cluster/data" -l "$cluster/server.log" \
  -o "-k $cluster/socket -p $port -c listen_addresses=" -w start \
  > "$output/server-start.log" 2>&1
git rev-parse HEAD > "$output/git-head.txt"
"$initdb_bin" --version > "$output/postgres-version.txt"
"$rscript_bin" --version > "$output/rscript-version.txt" 2>&1
for script in internal-matrix label-special-names internal-live; do
  "$rscript_bin" "$script_dir/$script.R" "$cluster/socket" "$port" "$output" \
    > "$output/$script.log" 2>&1
  echo "$script complete"
done
"$pg_ctl_bin" -D "$cluster/data" -m fast -w stop >> "$output/server-stop.log" 2>&1
date -u '+%Y-%m-%dT%H:%M:%SZ' > "$output/capture-completed.txt"
echo "Captured results: $output"
