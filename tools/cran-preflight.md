# Local CRAN preflight

Run the release-readiness audit from a clean repository root, before freezing
the candidate SHA. There are two distinct uses of this command: a diagnostic
cache run and a formal preflight attempt for a release ledger.

```sh
Rscript tools/cran-preflight.R
```

Without `--output`, the command writes a diagnostic cache run below
`tools::R_user_dir("marginplyr", "cache")/cran-preflight`. It may create its
own unique cache directory there without separate approval. The run is useful
for diagnosing a candidate, but it is not release evidence and is never added
to the release ledger.

A **formal preflight attempt** is the stage-4 run made with an explicitly
approved durable `--output` directory. It archives clean `HEAD` into an
external disposable source tree, builds exactly one source tarball, and retains
that tarball and its SHA-256 with the full evidence bundle. Its public attempt
identifier is also the standard evidence-directory component, so its UTC
timestamp uses the path-safe basic form `YYYYMMDDTHHMMSSZ`:

```text
preflight-<full-candidate-sha>-<UTC timestamp>
```

For example, the standard durable location is:

```text
tools::R_user_dir("marginplyr", "data")/release-evidence/<version>/<attempt-id>
```

This is data, not a cache, and is neither a repository sibling nor a directory
an agent may select silently. Before creating a formal bundle, resolve and show
the absolute path and state that its purpose is to retain the private evidence
bundle for that one release attempt. Obtain explicit approval for that path. If
the standard location is not writable, the release operator may approve a
different durable, repository-external location. Do not add permanent Codex or
Claude Code write grants for either location, and do not make this command
interactive.

An explicit `--output <new-directory>` and its resolved target must not contain
`.Platform$path.sep`, because R CMD check adds its installed candidate library
to `R_LIBS`. The command rejects an unsafe path before candidate build or check
work with exit `2` (invocation unavailable); it never relocates the bundle.
A safe `--output` changes neither checks nor pass criteria. An exit 0 from a
formal attempt is evidence for the SHA the human approves and then freezes; a
candidate finding stops that path before freezing. The release operator owns the
local bundle. The command never uploads, shares, moves, or automatically deletes
it; a lost required bundle is invalid evidence and requires a new formal attempt,
even for the same candidate SHA. No shared raw evidence store or particular
backup product is required.

The command installs and edits nothing. Before running it, install every
`Suggests` entry at the version declared in `DESCRIPTION`, plus the packages in
`Config/Needs/preflight`, and install Quarto with its bundled Pandoc plus a TeX
toolchain. The installed `marginplyr` version must equal the candidate version because the
Quarto vignette build runs in a child R process. A missing prerequisite exits
with an installation hint rather than changing the library.

## Agent sandbox network policy

`.codex/config.toml` and `.claude/settings.json` give sandboxed commands the
same exact host allowlist used by this preflight. The repository verifier
derives that list from R's standard repositories and the current candidate
URLs, includes a candidate URL's required redirect host, rejects either client
drifting from the other, and rejects extra hosts. The preflight runs it before
loading the audit pipeline, and `lint.yaml` runs it before the preflight
contract fixtures. Each client grants write access to the platform-specific
default evidence-cache directory; neither replaces the home directory, filters
credentials from the subprocess environment, or grants a public-internet
wildcard.

Claude Code requires one user-owned setting because it deliberately ignores
strict allowlisting from repository settings. Merge this into
`~/.claude/settings.json` once, preserving any other settings:

```json
{
  "sandbox": {
    "network": {
      "strictAllowlist": true
    }
  }
}
```

Start a fresh session after changing agent settings. From the repository root,
run `codex --strict-config doctor --summary --no-color`; its Configuration row
must say that configuration loaded, and its sandbox row must report restricted
filesystem access with networking enabled. In Claude Code, run `/sandbox` and
inspect the Config tab: sandboxing and auto-allow must be enabled, unsandboxed
retry must be disabled, strict allowlisting must be enabled, and the resolved
domains must include the tracked project list. These checks confirm that the
fresh session loaded the project policy before the preflight is attempted.

An unlisted host is therefore denied rather than added or retried outside the
sandbox. A denial or genuine remote outage leaves the preflight unavailable
with exit `2`; it does not broaden either allowlist. These settings govern
local command subprocesses only. Codex hosted web search and Claude Code
in-process web tools keep their own access controls and are unaffected.

The invariant local audit performs, once each:

1. package-aware lintr against the unpacked candidate, after loading that
   candidate through pkgload's default package and test-helper environment;
2. shipped spelling against the unpacked candidate;
3. the shared checktor baseline against the candidate tarball;
4. full `R CMD check --as-cran` through rcmdcheck, against that same tarball,
   with the manual, rebuilt vignettes, full Suggests, and incoming remote
   checks;
5. a fresh base-only R child that resolves `marginplyr` through the check
   library and retains its expected path, `find.package()` path,
   `loadNamespace()` path, and match result;
6. one bounded read-only URL diagnostic only when the check reports a URL
   problem; and
7. byte-for-byte comparison of the repository's before/after `git status`
   record.

lintr runs with its cache disabled and reports each finding as a candidate-
relative file, line, column, linter, and message. Findings make the audit exit
`1`; a package-load or linter condition makes it exit `2`. Neither route skips
the normal human summary, machine result, step table, tool versions, or final
worktree evidence.

When spelling finds unknown words, it prints the words and their locations to
the command output so the release agent can repair their source or propose a
wordlist addition. The audit creates no separate spelling log or table.

Every ERROR and WARNING blocks. Every NOTE blocks unless the complete,
normalized NOTE matches the state-scoped policy in
`.github/scripts/cran-note-policy.R`; an allowed NOTE remains visible and must
have matching counts and its explanation marker in `cran-comments.md`.
checktor's baseline is an independent policy surface.

The evidence summary uses exit `0` for a passing candidate, `1` for a candidate
or release-policy failure, `2` for invocation, prerequisite, tooling, or
infrastructure failure, and `130` for interruption. A missing or mismatched
candidate-library identity is tooling unavailable with exit `2`, never a
candidate finding or a passing check. Every nonzero result blocks release. The
printed external bundle path contains the tarball, hash manifest, machine-readable
results and step table, human summary, full check directory and logs, checktor
report, candidate-library identity record, and the conditional URL diagnostic.
The canonical final evidence files are `results.dcf` for machine-readable
terminal evidence, `summary.md` for the human-readable summary, `steps.tsv` for
per-stage evidence, and `candidate-library-identity.dcf` for the completed
check's candidate-library identity.
Those raw files, the local absolute path, credentials, and private URLs remain
in the local bundle; only the non-secret summary prescribed by
[`tools/cran-release.md`](cran-release.md) is public.

## What follows this gate

Continue with the complete human-and-agent release sequence in
[`tools/cran-release.md`](cran-release.md). That playbook owns CI, semantic
review, remote checks, the release issue, CRAN submission, publication, and
post-publication work. This command performs none of them.

For a real release, use the approved formal-attempt procedure in
[`tools/cran-release.md`](cran-release.md), including its retention and cleanup
rules. Keep an approved passing bundle through submission, publication, and the
complete 72-hour monitoring window. The candidate-tree invariant above is the
status of the repository worktree; tools may change disposable external build
and check trees without changing that candidate.
