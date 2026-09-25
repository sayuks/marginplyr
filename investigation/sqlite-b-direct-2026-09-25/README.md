# SQLite B-direct design evidence

Investigated: 2026-09-25
Archived: 2026-09-26
Base: `6a5f611d41bd462f3c933edea1dd0b9861d6e892`

This bundle preserved the isolated design review and final prototype sources.
It was evidence for a design choice, not a production implementation or release
approval. The adopted contract belongs in the separately published specification
and ADR amendments. The archived reports describe what their authors established
on 2026-09-25; their earlier statement that no design had been adopted remains
part of that historical record.

## Index and provenance

| Artifact | Meaning |
| --- | --- |
| [Original review](review/README.md) | The complete Japanese report, unchanged byte for byte |
| [Contract history](review/contracts-history.md) | Public-contract and implementation history examined for the review |
| [B detail](review/B/report.md) | B-stage and B-direct findings, including destination safety |
| [Final B-direct source](final-sources/B-direct/R) | All 25 final R files; these are the source authority for reproducing this prototype |
| [Final B-direct patch](review/B/B-direct.patch) | Exact base-to-final-source patch, verified during archival |
| [Original source hashes](review/prototype-source-sha256.json) | Hashes recorded during the investigation |
| [Common matrix](review/cross-check.R) | The shared 98-case script; its archived outputs sit under each variant's review directory |
| [B-direct regressions](review/B-direct/regressions.log) | The original 64-test, 990-assertion run |
| [Independent checks](review/independent/README.md) | The #655 warning/audit regression and separate #664 reproduction |
| [Temporary #661 stop patch](review/A/661-disable-direct-compute.patch) | A throwaway containment experiment, not a shipped fix |
| [Archival verification](verification-2026-09-26/README.md) | New checks performed while making this archive; separate from the original evidence |
| [SHA256SUMS](SHA256SUMS) | Every file in this bundle except the manifest itself |

`review/` retained all 154 original files, including historical intermediate
scripts, patches, failing logs, corrected final runs, CSVs and RDS files. No
original report, log, or hash was rewritten. `BASE_COMMIT` and the six
`issue-661.json` through `issue-666.json` files were copied from the original
review root. These issue bodies are snapshots, not the maintained issue tracker.

`final-sources/` contains complete final `R/` directories for A, B (B-stage),
B-direct, C, and A661-stop. Other package files are reconstructed from the base
commit in this repository. No package file at this branch's root was changed.

The `.initial.py` scripts and `.initial.log` files in `review/B/` describe earlier
build attempts and are **not** instructions to reconstruct the final prototype.
Other historical attempts, such as the incomplete #661 identifier patch, remain
in their original locations and are identified by their original names and
reports. Use `final-sources/` or the verified B-direct patch for final-source
reproduction. Prototype argument validation and comments were not promoted to
production quality by archiving them.

## Verify and prepare an isolated replay

Requirements: Python 3, Git with the recorded base commit available, and an R
environment containing the prototype dependencies. The original versions are
recorded in the archived `session-info.txt` files. Preparation does not install
packages or fetch a commit. The helper refuses a nonempty scratch directory.

From a checkout containing this archive:

```sh
python3 investigation/sqlite-b-direct-2026-09-25/prepare-replay.py --verify-only
python3 investigation/sqlite-b-direct-2026-09-25/prepare-replay.py \
  --scratch /absolute/path/to/new-scratch --run B-direct
```

The helper verifies hashes, obtains the baseline with `git archive`, reconstructs
all five variants from the saved final R files, and runs the shared matrix against
B-direct. Use `--repo /path/to/repository` when the bundle was copied outside the
repository. Omit `--run` to prepare without running. The helper does not touch the
archive or the repository checkout. A replay needs its own new scratch directory.

The scratch directory contains:

- the baseline package at its root and independent packages under `variants/`;
- `original-review/`, an unmodified copy of the original review and outputs;
- `review/`, copied executable scripts and otherwise empty output directories;
- `replay-adaptations.json`, a list of literal absolute-root substitutions made
  only in the executable script copies.

The original scripts embedded their temporary review root in several places.
Preparation remaps that exact string to the new scratch root in `.R` and `.py`
script copies. Original reports, logs, saved sources and archival scripts keep
their original bytes. The helper does not run historical prototype-building
scripts. All fresh script output goes into the scratch `review/` tree, never
into `original-review/` or this archive.

After preparation, further original checks can be run from the scratch root:

```sh
Rscript review/cross-check.R . review/baseline
Rscript review/cross-check.R variants/A review/A
Rscript review/cross-check.R variants/B review/B
Rscript review/cross-check.R variants/C review/C
Rscript review/B-direct/run-regressions.R variants/B-direct
Rscript review/B/public-cases.R variants/B-direct
Rscript review/B/destination-safety.R variants/B-direct
Rscript review/A/safety.R variants/A661-stop
Rscript review/B-direct/commit-check.R
```

Consult each script for other entry points and argument conventions. The
baseline and C were expected to fail some matrix assertions; completing their
R process is not a passing test result. `prepare-replay.py --run` checks the CSV
as well as the R exit status and returns nonzero when any selected case fails.
The later archival verification reran only B-direct's common matrix. The other
commands above are preserved reproduction recipes, not claims of additional
archival reruns.

## Limits of the evidence

The original review's unverified items and residual risks remain in its
section 9. Archival replay did not run the full package gate, release matrix,
additional platforms or dependency versions, transaction-recovery faults,
concurrent writers, or performance benchmarks. Passing a prototype matrix does
not establish production readiness. Qualified-index limitations, destination
metadata cost, safe rejections, and the construction-time SQL audit boundary
remain material inputs to the adopted specification.
