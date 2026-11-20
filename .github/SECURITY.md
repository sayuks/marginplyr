# Security policy

## Reporting a vulnerability

Please do **not** report security vulnerabilities through public GitHub
issues, discussions, or pull requests.

Report them privately through GitHub's
[private vulnerability reporting][report]: the **Report a vulnerability**
button on this repository's **Security** tab. If that option is unavailable,
email the maintainer at <sayuks.dev@gmail.com>.

Please include:

- what an attacker can do, and under what conditions;
- the marginplyr version or commit, R version, and the database backend and
  driver involved, if any;
- a minimal reproducible example, ideally with the SQL marginplyr generated
  (`dbplyr::show_query()`);
- any known mitigation or workaround.

## Supported versions

Fixes are made on the default branch and released in the next version. Please
confirm the issue against the latest commit on the default branch before
reporting.

## Scope

marginplyr builds SQL from the data a caller passes in, so the following are
in scope:

- column names, margin labels, or data values that change the structure of
  the SQL marginplyr generates (SQL injection);
- marginplyr reading a lazy table's data, or writing to a backend, when the
  call did not ask it to;
- weaknesses in this repository's GitHub Actions workflows or release process
  that could expose secrets or let untrusted code run with repository
  privileges.

The following are out of scope:

- the effects of R code or `dplyr::sql()` fragments the caller writes
  themselves: these are code the caller chose to run;
- vulnerabilities in dependencies such as dplyr, dbplyr, DBI, or a database
  driver, which should be reported to that project;
- incorrect results without a security impact, which are ordinary bugs and
  belong in the [issue tracker][issues].

## What to expect

The maintainer will acknowledge your report as soon as possible, confirm
whether it is in scope, and keep you informed of progress toward a fix.
Please keep the details confidential until a fixed version is released, or
for 90 days after your report, whichever comes first; the timeline can be
extended by agreement. Confirmed vulnerabilities are published as a GitHub
security advisory, with a CVE requested where appropriate. You will be
credited unless you prefer to remain anonymous.

[report]: https://github.com/sayuks/marginplyr/security/advisories/new
[issues]: https://github.com/sayuks/marginplyr/issues
