# README.qmd, Floating Quarto, and the README Synchronization Gate

Investigated: 2026-09-14

## Question

Should marginplyr replace `README.Rmd` with `README.qmd`, use the CI suggested
by usethis, and stop pinning both Quarto and Pandoc?  Why does the repository
compare the generated README byte-for-byte, and is that comparison necessary?

## Conclusion

Migrate the README to `README.qmd`, use the latest released Quarto and its
bundled Pandoc, and remove the standalone Pandoc pin.  Keep the existing
pull-request synchronization gate: render the README, then fail if the tracked
`README.md` or its generated figures differ.  A new Quarto release may
therefore require a generated-file-only update even when the source did not
change.  That is the unavoidable cost of combining a floating renderer with a
committed generated file and a complete synchronization check.

Do not adopt the r-lib auto-commit example.  It is an R Markdown workflow, not
a Quarto workflow; it uses the standalone `setup-pandoc` action; and it changes
the repository after a push instead of proving in the pull request that source
and generated output arrive together.  No current usethis function creates a
Quarto README CI workflow.

Byte identity is not itself a user requirement.  It is the implementation of a
stronger and useful requirement: the Markdown a reader receives must be the
output of executing the tracked source against the working package.  A
semantic comparison could in principle ignore writer-only changes, but neither
usethis nor Quarto supplies one.  Removing the byte comparison without
replacing it would retain only “the source can render”, and would allow stale
example output, prose, links, badges, or an entirely stale `README.md` to pass.
For this repository, retaining the simple complete gate is preferable to
inventing and maintaining a partial semantic oracle.

## What usethis actually creates

The usethis reference says that `README.qmd` and `README.Rmd` must still be
rendered regularly and points to an example workflow in `r-lib/actions`.  It
does not say that `use_readme_qmd()` installs that workflow
([README reference](https://usethis.r-lib.org/reference/use_readme_rmd.html)).
The implementation confirms the narrower behavior:

- `use_readme_qmd()` writes `README.qmd`, adds it to `.Rbuildignore` for a
  package, and, in a Git repository, installs a local pre-commit hook
  ([implementation](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/R/readme.R#L88-L133)).
- The package template selects Quarto's `gfm` output and sets
  `default-image-extension: ""`; it also keeps the existing
  `man/figures/README-` figure path
  ([template](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/inst/templates/package-README-qmd)).
- The hook compares modification times and checks that source and output are
  staged together.  It does not render the source or compare its contents with
  the output
  ([hook template](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/inst/templates/readme-rmd-pre-commit.sh)).

The current exported workflow helper is the generic `use_github_action()`; a
`use_github_action_render()` export does not exist.  The generic helper copies
a named workflow from `r-lib/actions`, defaulting to that repository's latest
published release
([reference](https://usethis.r-lib.org/reference/use_github_action.html),
[implementation](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/R/github-actions.R#L82-L154),
[namespace](https://github.com/r-lib/usethis/blob/v3.2.2/NAMESPACE)).
`use_readme_qmd()` also does not convert an existing `README.Rmd`: it creates a
new skeleton, reports the existing file, and asks the caller to migrate and
delete it.  A repository-specific migration is therefore required here.

## The linked CI example is not a QMD synchronization check

The only README-like rendering example under the current `r-lib/actions`
`v2-branch` is `render-rmarkdown.yaml`.  It:

1. runs only after pushes that touch `**.Rmd`;
2. finds changed `.Rmd` files and calls `rmarkdown::render()` on them;
3. commits the corresponding `.md` files; and
4. pushes that new commit back to the branch.

It neither recognizes `.qmd` nor checks a committed output byte-for-byte
([workflow](https://github.com/r-lib/actions/blob/8efa4208c1e4a4837731d3c22011c7550205df2f/examples/render-rmarkdown.yaml)).
It also assumes an renv project (`r-version: renv` and `setup-renv`), while
marginplyr has no `renv.lock`.  Its `git commit` and `git push` failures are
both converted to successful shell output with `|| echo`, so a green run does
not prove that a generated update reached the branch.

The example does not meet the no-pin goal either.  It invokes
`r-lib/actions/setup-pandoc@v2` without an input, and the action's default on
the investigation date was the exact version 3.8.3
([action definition](https://github.com/r-lib/actions/blob/8efa4208c1e4a4837731d3c22011c7550205df2f/setup-pandoc/action.yml#L5-L20)).
The action reference `@v2` can move, but the Pandoc installed by the examined
version is not “whatever comes with Quarto”.

Adapting the example into a QMD auto-commit job would still be a poor fit for
this repository.  It would require `contents: write`, make source and output
land in separate commits, update `main` only after an unsynchronized commit had
already arrived, and conflict with protected-branch or fork-pull-request
permissions.  The release process deliberately keeps its source edit and
generated README in one reviewed pull request.  The existing documentation
workflow has read-only permissions and proves the proposed commit rather than
mutating it.

## What Quarto's actions do and do not do

`quarto-dev/quarto-actions/setup@v2` defaults its `version` input to `release`,
which resolves and downloads the latest stable Quarto when the job runs.  An
exact version is optional
([setup definition](https://github.com/quarto-dev/quarto-actions/blob/91dabb203fce7f899d10ef8e482d94b00f397cbc/setup/action.yml#L3-L8),
[download branch](https://github.com/quarto-dev/quarto-actions/blob/91dabb203fce7f899d10ef8e482d94b00f397cbc/setup/action.yml#L48-L66)).
That provides the desired floating toolchain when `version` is omitted.

Quarto defines `quarto pandoc` as running the version of Pandoc embedded in
Quarto.  Its installers bundle dependencies because Quarto's output is tailored
to them and version mismatches are unsupported
([CLI reference](https://quarto.org/docs/cli/#quarto-commands),
[dependency policy](https://quarto.org/docs/faq/index.html#development-and-maintenance)).
Consequently a QMD README rendered by Quarto needs no standalone
`setup-pandoc` step.

The companion `quarto-dev/quarto-actions/render@v2` action runs
`quarto render` for the supplied path.  It neither commits output nor compares
it with the checkout
([render action](https://github.com/quarto-dev/quarto-actions/blob/91dabb203fce7f899d10ef8e482d94b00f397cbc/render/action.yml#L1-L44)).
Using that action is optional here: invoking `quarto::quarto_render()` in the
existing R-oriented documentation job expresses the same render and leaves the
repository-specific diff check visible beside it.

`r-lib/actions/setup-r-dependencies` adds another detail.  When `.qmd` files
are present and Quarto is absent, it installs Quarto with a default
`quarto-version: release`; its Pandoc auto-install is independent and defaults
to standalone Pandoc 3.8.3 when rmarkdown requires Pandoc
([action inputs and checks](https://github.com/r-lib/actions/blob/8efa4208c1e4a4837731d3c22011c7550205df2f/setup-r-dependencies/action.yaml)).
A bundled-only documentation job should therefore set up latest Quarto
explicitly, disable standalone Pandoc auto-install, and remove rmarkdown from
the README-only extra dependencies.

## QMD does not make the generated bytes stable

Quarto's GFM output still uses Pandoc.  Quarto documents GFM as the Markdown
dialect for GitHub and exposes it as a Pandoc-backed output format
([format overview](https://quarto.org/docs/output-formats/all-formats/)).
Changing from R Markdown to Quarto therefore removes an independently managed
Pandoc; it does not remove Pandoc from the generation path.

The experiment recorded in
[`2026-09-14-pandoc-quarto-bundling.md`](2026-09-14-pandoc-quarto-bundling.md)
was extended on 2026-09-14 with a QMD rendering.  The same candidate
`README.qmd` was rendered by Quarto 1.10.18 while `QUARTO_PANDOC` selected
official Pandoc 3.8.3, 3.10, 3.10.1, and 3.11.  The 3.10 to 3.10.1 transition
produced the same difference already measured for R Markdown: the literal
reporting-levels fence changed from ```` ``` text ```` to a bare fence.  Thus
QMD migration does not cure the writer-only byte difference that motivated the
current pin.  Allowing Quarto to float also allows its bundled Pandoc and
Quarto's own filters to change.

This does not prevent the proposed policy.  It changes how an upstream writer
change is handled:

- with an exact renderer pin, a source-independent change is suppressed until
  the repository deliberately moves the pin;
- with latest Quarto, the synchronization job exposes it on the first run
  after the release, and the maintainer reviews and commits the newly generated
  bytes.

The latter satisfies “no Quarto or Pandoc version pin”, but it is deliberately
not byte-stable across time.

## Why this repository compares bytes

Commit
[`716c661`](https://github.com/sayuks/marginplyr/commit/716c66199fc40be611e79a6998f970e794635fd8)
added README regeneration and the comparison together.  The reason was not
that Markdown formatting has a byte-level public contract.  It was that
`README.md` was a generated file with no regeneration gate.

The current topology gives the generated half unusually wide reach:

- GitHub displays `README.md` at the repository root.
- The altdoc site uses it as the home-page source.
- `.Rbuildignore` excludes `README.Rmd`, while `README.md` is the half that can
  accompany an installed package.
- The README contains twelve reader-facing R chunks.  Their printed tables are part
  of what readers see, not merely evidence that the source parses.
- `README.Rmd` writes figures under `man/figures/README-`; the workflow renders
  before checking all of `man/`, so figure additions and changes are covered.
- The release helper edits the source, renders the output against a temporary
  installation of the edited working tree, and verifies that the generated
  output contains the post-publication installation claims.

These facts are recorded in the repository's
[`document.yaml`](../.github/workflows/document.yaml),
[`AGENTS.md`](../AGENTS.md), `.Rbuildignore`, and
[`tools/cran-release.R`](../tools/cran-release.R).  Commit
[`2e396de`](https://github.com/sayuks/marginplyr/commit/2e396dea45ea76571a88f0b1f0e71930384b71d5)
later clarified that a byte diff can mean either a stale generated file or a
different writer.

`git diff --exit-code README.md` is a complete generic oracle over that output.
It catches an unrendered prose edit, stale computed table, hand edit to the
generated file, dropped badge, changed link, writer rewrite, or any other
difference without maintaining a list of expected fragments.  The cost is that
it cannot distinguish a meaningful change from semantically equivalent
writer formatting.

## Is byte comparison necessary?

It is not necessary to establish that the source executes successfully.
Rendering to a disposable location and checking the exit status is sufficient
for that narrower property.  Existing tests would also continue to enforce
some selected semantics, including the recorded CRAN publication state and
the optional-dependency guard policy, over both README halves.

Those checks do not establish synchronization.  If the byte diff were simply
removed, all of the following could pass:

- `README.qmd` renders successfully, but committed `README.md` contains output
  from an older marginplyr implementation;
- source prose or a link changes, but the reader-facing file remains old;
- the generated README is edited directly and no longer derives from its
  source;
- a new successful chunk or figure never reaches the committed artifact.

A semantic synchronization gate could parse committed and newly rendered GFM
to Pandoc ASTs, normalize agreed writer-only differences, and compare the
result.  It would need explicit policy for code-block classes, raw HTML,
comments, wrapping, tables, links, figures, attributes, and every future
construct.  That policy would itself be another maintained generator contract.
No examined usethis, r-lib/actions, or Quarto facility provides it.  A marker
scan would be simpler but incomplete by construction.

Therefore byte comparison is not theoretically necessary, but some
synchronization comparison is necessary unless the project accepts stale
reader-facing documentation.  For marginplyr's executable, release-edited,
widely published README, byte comparison remains the smallest complete check.
The correct consequence of floating Quarto is to accept occasional reviewed
regeneration, not to discard the gate.

## Why migration is still worthwhile

marginplyr already uses `.qmd` for its vignettes, declares Quarto as a
`VignetteBuilder` and system requirement, and builds its site with Quarto.
Moving the README to QMD leaves one document engine and one bundled Pandoc path
instead of maintaining R Markdown plus a standalone Pandoc solely for the
README.

Current devtools supports this route.  `devtools::build_readme()` recognizes
`README.qmd`, installs the current package source into a temporary library,
and passes that library through `R_LIBS_USER` because Quarto starts a separate R
process for knitr
([implementation](https://github.com/r-lib/devtools/blob/v2.5.2/R/build-readme.R#L98-L167),
[reference](https://devtools.r-lib.org/reference/build_rmd.html)).
That preserves a marginplyr-specific requirement which a naive
`quarto render README.qmd` invocation can miss: examples must run against the
edited tree, not an older user-library installation.

The migration should use the usethis QMD front matter and syntax, but should
not run `use_readme_qmd()` as if it were an in-place converter.  The existing
source should be renamed and adapted deliberately.  The untracked local
pre-commit hook that usethis offers is optional and cannot replace CI.

## Required migration surface

An implementation needs to change these pieces together:

1. Rename `README.Rmd` to `README.qmd`; select `format: gfm` with
   `default-image-extension: ""`; translate chunk-header options to Quarto
   `#|` options; retain `fig.path = "man/figures/README-"`; and regenerate
   `README.md` with latest Quarto.
2. Replace the documentation job's standalone `setup-pandoc` step with
   `quarto-dev/quarto-actions/setup@v2` and no `version` input.  Prevent
   `setup-r-dependencies` from independently installing Pandoc.  Render the QMD
   after installing `local::.` and retain the diffs over `README.md`, `man/`,
   and `NAMESPACE`.
3. Change the failure text so it says that either source/output is stale or
   latest Quarto changed generated formatting.  The remedy is to inspect and
   commit the current render, not to reproduce an exact version.
4. Update `AGENTS.md`, `.Rbuildignore`, the README-source scans in
   `tests/testthat/test-documentation.R`, and the generated-file exclusion in
   `.github/scripts/verify-doc-references.R` from `.Rmd` to `.qmd`.
5. Update `tools/cran-release.R`, `tools/cran-release.md`, and
   `.github/scripts/verify-cran-release.R` to edit and fixture `README.qmd`.
   Remove the Pandoc 3.10.1 assertion.  Preserve the temporary working-tree
   installation and expose its library to Quarto's child R process, following
   the mechanism in `devtools::build_readme()`.
6. Update the preflight verifier's prohibited README-render spelling and any
   generated-file notices.  Run the repository's package-affecting review
   check after implementation, as required by `design/agents/local-checks.md`.

The Quarto action's generic `render` wrapper and the r-lib auto-commit example
add no missing guarantee.  The appropriate CI is a small adaptation of the
repository's existing `document.yaml`, because that workflow already owns the
source/output synchronization contract and the ordering needed for generated
figures.

## Decision

Adopt `README.qmd`, floating latest-release Quarto, and Quarto's bundled
Pandoc.  Do not pin either tool.  Do not adopt an auto-commit workflow.  Keep
the byte comparison as a pull-request gate and document upstream writer churn
as one of its expected failure modes.

Revisit semantic comparison only if upstream-only README churn becomes frequent
enough to impose a demonstrated maintenance cost.  Until then it would replace
one visible occasional regeneration with a larger permanent normalization
surface and a weaker, harder-to-explain correctness argument.
