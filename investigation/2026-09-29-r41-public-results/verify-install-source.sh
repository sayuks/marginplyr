#!/bin/sh
# Record the exact source artifact passed to R CMD INSTALL and the fresh loaded copy.
set -eu
source_tar=$1
expected_sha=$2
source_commit=$3
r_bin=$4
case_lib=$5
out=$6
actual=$(shasum -a 256 "$source_tar" | awk '{print $1}')
[ "$actual" = "$expected_sha" ]
{
  printf 'source_commit=%s\nsource_tarball=%s\nsource_sha256=%s\n' "$source_commit" "$source_tar" "$actual"
  printf 'R=%s\ncase_lib=%s\n' "$r_bin" "$case_lib"
  "$r_bin" --vanilla --slave -e 'cat("R_version=", as.character(getRversion()), "\n", sep=""); cat("libPaths=", paste(.libPaths(), collapse="|"), "\n", sep="")'
  "$r_bin" CMD INSTALL --library="$case_lib" "$source_tar"
  "$r_bin" --vanilla --slave -e 'library(marginplyr); cat("installed_version=", as.character(packageVersion("marginplyr")), "\n", sep=""); cat("loaded_path=", normalizePath(getNamespaceInfo("marginplyr", "path")), "\n", sep=""); cat("built=", packageDescription("marginplyr")[["Built"]], "\n", sep="")'
} > "$out" 2>&1
