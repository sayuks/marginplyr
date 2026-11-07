# Share lane execution record

Investigated: 2026-11-07

Source SHA: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`.
Disposable source came from `git archive HEAD`, extracted under lane scratch.
The submitting worktree was not edited. All listed research executions were
owned and collected by the share lane agent; their live sessions were not
transferred to the coordinator. All processes ended, and no owned child work
remained. The scripts spawned no explicit subprocesses.

| Execution | Retained identity | Terminal exit | Result |
|---|---|---:|---|
| Initial carrier probe | chunk 980915 | 0 | Adjacent exploratory observations |
| Initial name/attribute write probe | chunk 431e8c | 0 | Name-map boundary |
| Initial nested-container probe | chunk 48e8f7 | 0 | Nested-container boundary |
| Preliminary 96 comparisons | session 39872, terminal chunk 63f86c | 0 | 36 missing refusals, 12 repaired refusals, 48 legal calls |
| Final combined observation | session 53856, terminal chunk 0f0301 | 0 | 168 comparisons |
| Final combined control | session 23614, terminal chunk 4a5b0b | 0 | 108 comparisons |
| Combined desired contract | session 50484, terminal chunk 6b4303 | 1 | Intentional assertion failure |
| Family transfer desired contract | chunk d5b7ca | 1 | 72 missing-refusal / legal comparisons |
| Family rename desired contract | chunk 371291 | 1 | 24 missing-refusal / legal comparisons |
| Family nested desired contract | chunk 7d270a | 1 | 24 missing-refusal / legal comparisons |
| Safe renamed-member observation | chunk b75dd2 | 0 | 12 false refusals, 12 legal positional results |
| Safe renamed-member control | chunk 6ec2e8 | 0 | 12 legal positional results |
| Safe renamed-member contract | chunk 231781 | 1 | Intentional assertion failure |
| Final numeric oracle | session 16270, terminal chunk 567b3e | 0 | 144 collections, 9,540 ratio assertions |

The family-selection argument was added after the combined-mode runs; it did
not change the default combined selection. The final family contract runs
used the final script. The original numeric probe included one unused binding
assignment; it was removed before the separately recorded final numeric run.

`manifest.csv` records final content MD5s. These research outputs are dated
observations and not package-suite or Review-ready results. The coordinator's
fresh-main assessment, duplicate queries, independent replay, publication and
Review-ready records are separate evidence.
