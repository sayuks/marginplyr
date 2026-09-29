# Wrapper-only failures in the disposable environments

These are diagnostic excerpts from the failed terminal attempts. Their full
per-package logs were overwritten by the successful retries. Neither attempt
reached a marginplyr public call.

## R 4.1.3: mixed R headers while building `cli` 3.6.2

The outer `R41` wrapper selected R 4.1.3, but the extracted framework's
internal `Resources/bin/R` still pointed to the host's
`/Library/Frameworks/R.framework/Resources`. The attempted command was:

```sh
/private/tmp/marginplyr-744/R41 CMD INSTALL \
  --library=/private/tmp/marginplyr-744/lib-r41 \
  /private/tmp/marginplyr-744/sources/cli_3.6.2.tar.gz
```

The compiler line contained
`-I"/Library/Frameworks/R.framework/Resources/include"`, followed by:

```text
cleancall.c:39:28: error: call to undeclared function 'Rf_findVar'
diff.c:88:15: error: call to undeclared function 'STRING_PTR'
ERROR: compilation failed for package ‘cli’
```

Redirecting the internal wrapper to the extracted R 4.1.3 framework, as
`replay-setup.py` does, let the unchanged source build and load.

## R 4.5.2: wrong internal wrapper during source confirmation

The first source-confirmation attempt called the older `R45` outer wrapper.
Its `R CMD INSTALL` recursion reached the system current-R wrapper (R 4.6),
which loaded R 4.5 packages and crashed during lazy loading:

```text
WARNING: ignoring environment value of R_HOME
sh: line 1: 25671 Segmentation fault: 11  R_TESTS= '/Library/Frameworks/R.framework/Resources/bin/R' ...
ERROR: lazy loading failed for package ‘marginplyr’
* restoring previous ‘/private/tmp/marginplyr-minver-kalaij/lib-r45near/marginplyr’
```

The R 4.5 case-local `r45-home/bin/R` wrapper then installed the same hashed
tarball and loaded it in a fresh process. The successful source identity log
is `install-identity-r45-control.log`. This was a harness-only failure; the
restored prior installation remained available, and the subsequent public
probe matched R 4.1.3 after the successful reinstall.

The initial output directory `r41` also conflicted with `R41` on the
case-insensitive scratch filesystem. Replacing it with `case-r41` fixed that
log-path failure; no public call result was written in the failed attempt.
