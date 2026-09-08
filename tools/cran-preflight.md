# Local CRAN preflight

Run the release gate from a clean repository root:

```sh
Rscript tools/cran-preflight.R
```

The command archives clean `HEAD` into an external disposable source tree,
builds exactly one source tarball, and retains that tarball and its SHA-256 with
the full evidence bundle. `--output <new-directory>` selects another external
bundle location; it changes neither checks nor pass criteria.

The command installs and edits nothing. Before running it, install every
`Suggests` entry at the version declared in `DESCRIPTION`, plus the packages in
`Config/Needs/preflight`, and install Quarto, Pandoc, and a TeX toolchain. The
installed `marginplyr` version must equal the candidate version because the
Quarto vignette build runs in a child R process. A missing prerequisite exits
with an installation hint rather than changing the library.

The invariant local gate performs, once each:

1. shipped spelling against the unpacked candidate;
2. the shared checktor baseline against the candidate tarball;
3. full `R CMD check --as-cran` through rcmdcheck, against that same tarball,
   with the manual, rebuilt vignettes, full Suggests, and incoming remote
   checks;
4. one bounded read-only URL diagnostic only when the check reports a URL
   problem; and
5. byte-for-byte comparison of the repository's before/after `git status`
   record.

Every ERROR and WARNING blocks. Every NOTE blocks unless the complete,
normalized NOTE matches the state-scoped policy in
`.github/scripts/cran-note-policy.R`; an allowed NOTE remains visible and must
have matching counts and its explanation marker in `cran-comments.md`.
checktor's baseline is an independent policy surface.

The evidence summary uses exit `0` for a passing candidate, `1` for a candidate
or release-policy failure, `2` for invocation, prerequisite, tooling, or
infrastructure failure, and `130` for interruption. Every nonzero result blocks
release. The printed external bundle path contains the tarball, hash manifest,
machine-readable results and step table, human summary, full check directory
and logs, checktor report, and the conditional URL diagnostic.

## Release sequence

`usethis::use_release_issue()` remains the separate human checklist. Record the
candidate SHA and then complete these stages in order:

1. Run this local preflight.
2. Push the same SHA and record all required CI results, including the routine
   matrix, documentation, lint, site, coverage, release/devel/oldrel tarball,
   depends-only, suite-coverage, library-isolation, and live backend evidence.
3. Complete the cited, non-mutating semantic CRAN review and disposition every
   finding.
4. Run one targeted current R-devel R-hub v2 platform, then upload the retained
   tarball to win-builder R-devel after verifying its SHA-256.
5. Complete `cran-comments.md`, version, submission, confirmation, publication,
   and follow-up as distinct human actions.

For an initial submission, the recorded `unpublished` state admits only the
narrow `New submission` NOTE and requires the unpublished installation route.
For an update, record current CRAN results, reverse-dependency evidence,
downstream communication where needed, and the update rationale; it gets no
weaker command mode or permanent rapid-resubmission NOTE allowance.

Evidence is valid only for its recorded commit. A later tracked change to
metadata, documentation, examples, dependencies or guards, executable sources,
or `cran-comments.md` restarts the applicable local, CI, semantic, and remote
stages. The command never dispatches those stages or performs a submission.
