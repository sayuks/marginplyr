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

## What follows this gate

Continue with the complete human-and-agent release sequence in
[`tools/cran-release.md`](cran-release.md). That playbook owns CI, semantic
review, remote checks, the release issue, CRAN submission, publication, and
post-publication work. This command performs none of them.

For a real release, `--output` must name a durable, repository-external
directory. Keep its retained tarball available through win-builder, CRAN
submission, and GitHub Release creation. The candidate-tree invariant above is
the status of the repository worktree; tools may change disposable external
build and check trees without changing that candidate.
