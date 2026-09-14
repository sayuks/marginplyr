# Pandoc Pinning and Quarto's Bundled Pandoc

Investigated: 2026-09-14
Revised: 2026-09-14 — investigation/2026-09-14-readme-qmd-ci.md

## Question

Can marginplyr stop installing and pinning Pandoc separately, and instead use
the Pandoc bundled with Quarto?

## Conclusion

Yes, but not by deleting `setup-pandoc` alone.

An exact Quarto release selects a fixed official installer, and that installer
contains the Pandoc version against which Quarto was built.  Pinning the Quarto
release can therefore replace a separate Pandoc pin for processes that Quarto
itself runs.  Quarto explicitly recommends keeping its bundled dependencies
together because its output is tailored to those versions, and its CLI defines
`quarto pandoc` as the embedded Pandoc
([Quarto FAQ](https://quarto.org/docs/faq/index.html#development-and-maintenance),
[Quarto CLI reference](https://quarto.org/docs/cli/#quarto-commands)).

Two additional conditions apply to marginplyr:

1. Quarto must be pinned to an exact release.  The default `release` input to
   `quarto-dev/quarto-actions/setup@v2` resolves the latest stable release at
   workflow runtime, so neither Quarto nor its Pandoc is fixed
   ([setup action inputs](https://github.com/quarto-dev/quarto-actions/blob/main/setup/README.md#inputs-available),
   [action implementation](https://github.com/quarto-dev/quarto-actions/blob/main/setup/action.yml#L4-L7)).
2. Direct `rmarkdown::render()` calls must be made to use the bundled binary.
   Installing Quarto does not put its internal Pandoc in any of the locations
   that `rmarkdown::find_pandoc()` promises to search.

For Quarto-driven `.qmd` rendering, a separate Pandoc installation is
unnecessary.  For the README byte-comparison gate, either the existing exact
standalone Pandoc pin must remain, or the job must pin Quarto exactly and route
`rmarkdown::render()` explicitly to that Quarto release's Pandoc.

## Why the repository pins Pandoc

Commit `716c66199fc40be611e79a6998f970e794635fd8` introduced both README
regeneration and the Pandoc 3.10.1 pin on 2026-08-14.  The commit and the
comments in `.github/workflows/document.yaml` give one reason: the workflow
regenerates `README.md` with `rmarkdown::render("README.Rmd")` and compares it
byte-for-byte with the committed file.  A writer-only change therefore fails
the job even when `README.Rmd` and the executed R code have not changed.

The claimed difference was reproduced on 2026-09-14 by rendering the same
working-tree `README.Rmd` in isolated output directories with official Pandoc
binaries.  Pandoc 3.8.3 and 3.10 emitted:

````text
``` text
(region, store)  store detail
```
````

Pandoc 3.10.1 and 3.11 emitted the same fence without the `text` info string.
The 3.10.1 and 3.11 results matched the committed `README.md` byte-for-byte.
The 3.8.3 versus 3.10.1 outputs differed only on that fence line; the measured
SHA-256 values for those two full files were respectively:

```text
95d48d1cae96441e0c30920cbe52044f95852c98b0eacc737bbb35670e3fa30d
1c5b0603d7d3fce028b4487f17fe337bf8fe4ebc7cb5b386c6ba6a2bd14972cb
```

Pandoc's Markdown output was not byte-stable across the measured versions.
Its 3.10.1 release notes themselves record Markdown writer changes, including
a change to how the code language class is identified
([Pandoc 3.10.1 release notes](https://github.com/jgm/pandoc/releases/tag/3.10.1)).
The repository's pin is consequently solving a real reproducibility problem,
not merely supplying a missing executable.

## What Quarto fixes, and what the action leaves floating

Quarto's installers bundle required dependencies to avoid mismatches.  Quarto
says those dependencies are ideally updated once per stable release, while
reserving the right to change that cadence
([Quarto FAQ](https://quarto.org/docs/faq/index.html#development-and-maintenance)).
The packaging configuration records the Pandoc version used for a release.  In
particular:

- Quarto 1.9.38 records `PANDOC=3.8.3`
  ([1.9.38 configuration](https://github.com/quarto-dev/quarto-cli/blob/v1.9.38/configuration#L10-L16)).
- Quarto 1.10.18 records `PANDOC=3.10`
  ([1.10.18 configuration](https://github.com/quarto-dev/quarto-cli/blob/v1.10.18/configuration#L10-L16));
  the 1.10 release metadata also records the update to Pandoc 3.10
  ([1.10.18 release metadata](https://quarto.org/docs/download/_download.json)).

This demonstrates both sides of the answer.  An exact Quarto version fixes its
official bundled Pandoc, but allowing the Quarto version to move also allows
Pandoc to move.

The setup action accepts an exact version such as `version: 1.4.515`.  When the
input is omitted, it installs the latest released Quarto; its implementation
maps the `release` value through Quarto's live download metadata and downloads
that version's installer, while an exact value downloads the installer under
that exact release tag
([setup documentation](https://github.com/quarto-dev/quarto-actions/blob/main/setup/README.md#inputs-available),
[download implementation](https://github.com/quarto-dev/quarto-actions/blob/main/setup/action.yml#L52-L65)).
On the investigation date, that live metadata named Quarto 1.10.18
([download metadata](https://quarto.org/docs/download/_download.json)).

The action reference `@v2` is itself a moving major-version tag.  The Quarto
actions repository states that a major tag follows the latest minor or patch
release of the action; a full action release tag pins the action implementation
([release management](https://github.com/quarto-dev/quarto-actions/blob/main/README.md#release-management)).
That is separate from the `version:` input, which selects the Quarto installer.

`r-lib/actions/setup-pandoc` takes a different path: it downloads the requested
Pandoc version and adds that standalone binary to `PATH`.  Its `pandoc-version`
input accepts an exact version, `latest`, or `nightly`; 3.8.3 was its default on
the investigation date
([setup-pandoc documentation](https://github.com/r-lib/actions/tree/v2-branch/setup-pandoc#readme),
[setup-pandoc action definition](https://github.com/r-lib/actions/blob/v2-branch/setup-pandoc/action.yml#L7-L20)).
It does not configure or replace Quarto's internal binary.

## Why direct rmarkdown rendering does not automatically use Quarto

`rmarkdown::find_pandoc()` searches `RSTUDIO_PANDOC`, `PATH`, and
`~/opt/pandoc/`, then chooses the highest version it finds.  Its documented
search does not include the Quarto installation and does not mention
`QUARTO_PANDOC`
([rmarkdown reference](https://pkgs.rstudio.com/rmarkdown/reference/find_pandoc.html)).
The rmarkdown source read on the investigation date implements that exact list
([`find_pandoc()` source](https://github.com/rstudio/rmarkdown/blob/main/R/pandoc.R#L674-L724)).

`QUARTO_PANDOC` belongs to Quarto's own binary selection.  Quarto's source
checks `QUARTO_<BINARY>` and otherwise returns its packaged tools path; its
render pipeline invokes the path returned by `pandocBinaryPath()`
([Quarto resource lookup](https://github.com/quarto-dev/quarto-cli/blob/v1.10.18/src/core/resources.ts#L31-L80),
[Quarto Pandoc invocation](https://github.com/quarto-dev/quarto-cli/blob/v1.10.18/src/command/render/pandoc.ts#L374-L375)).
Setting `QUARTO_PANDOC` therefore changes Quarto's choice, but does not make a
separate `Rscript -e 'rmarkdown::render(...)'` discover it.

This distinction was also measured on 2026-09-14.  With `PATH` containing only
Quarto's `bin` directory, `rmarkdown::find_pandoc(cache = FALSE)` returned
version 0 and `dir = NULL`.  Passing the bundled tools directory through
`find_pandoc(dir = ...)` found the binary.  The latter is stronger than merely
setting `RSTUDIO_PANDOC`, because rmarkdown otherwise chooses the highest
version among all searched locations.

The distinction maps directly to this repository:

- `.github/workflows/altdoc.yaml` installs Quarto and renders the site without
  a separate `setup-pandoc`; Quarto drives that render.
- `.github/workflows/document.yaml` directly calls
  `rmarkdown::render("README.Rmd")`; Quarto does not drive that render.
- `.github/workflows/R-CMD-check.yaml` and three release-matrix jobs install
  both standalone Pandoc and Quarto, although the package vignettes use the
  `quarto::html` engine.  No package code, test, or vignette directly calls
  `rmarkdown::render()`; the only direct calls found on 2026-09-14 were the
  README workflow and the release tooling.

`setup-r-dependencies` also has independent auto-install behavior: it installs
Pandoc when rmarkdown is in the resolved package dependencies and no `pandoc`
is on `PATH`, and installs the latest Quarto when `.qmd` files are present and
no `quarto` is on `PATH`.  Its inputs permit both behaviors to be controlled
([action definition](https://github.com/r-lib/actions/blob/v2-branch/setup-r-dependencies/action.yaml)).
Consequently, deleting the explicit `setup-pandoc` step without disabling or
satisfying that auto-install path does not establish a bundled-only toolchain.

There is a related release-tooling seam.  `tools/cran-release.md` asks the
operator to confirm `quarto pandoc --version`, while `tools/cran-release.R`
checks `rmarkdown::pandoc_version()` before the README render.  Those commands
do not inherently identify the same executable.  In the local installation
measured on 2026-09-14, the architecture-specific `pandoc` under Quarto 1.10.18
was a symlink to a separately installed 3.10.1, while the packaged
`pandoc-3.10.0` reported 3.10; `quarto check` warned that 3.10.1 did not
strictly match the expected 3.10.0.  This local mutation is not evidence about
the official installer, but it demonstrates why an assertion must verify the
intended path as well as a version string.  Quarto's documented
`QUARTO_PANDOC` override and source-level binary lookup permit this distinction
([Quarto resource lookup](https://github.com/quarto-dev/quarto-cli/blob/v1.10.18/src/core/resources.ts#L31-L80)).

## Practical options

### Keep the standalone Pandoc pin

This is the smallest change and preserves the committed README exactly.  It
continues to install Pandoc separately from Quarto, but makes the byte-level
renderer contract explicit.

### Use only Quarto's bundled Pandoc

This is sound if the implementation does all of the following together:

1. install an exact Quarto release rather than `release`;
2. disable the standalone Pandoc auto-install in `setup-r-dependencies`;
3. make the README render process call `rmarkdown::find_pandoc(dir =
   <that Quarto installation's architecture-specific tools directory>)`
   before `rmarkdown::render()`;
4. assert both `quarto --version` and `quarto pandoc --version` in the job;
5. regenerate and commit `README.md` whenever the exact Quarto release moves.

Using official Quarto 1.10.18 for this option would use Pandoc 3.10, not the
repository-pinned 3.10.1.  The measurement above shows that this changes the
reporting-levels fence to ```` ``` text ```` and therefore requires a matching
README regeneration.  Release tooling that requires
`rmarkdown::pandoc_version() == "3.10.1"` would need to assert the selected
Quarto bundle instead.

The second option removes a redundant independently managed binary and follows
Quarto's recommended bundled-dependency model.  It does not remove version
pinning as a reproducibility requirement: it transfers the renderer pin from
Pandoc to Quarto and adds an explicit bridge for the one render that bypasses
Quarto.

## Revisions (2026-09-14)

The follow-up QMD experiment in
`investigation/2026-09-14-readme-qmd-ci.md` established a different policy
choice: migrate the README to QMD, float the latest stable Quarto with its
bundled Pandoc, retain the exact generated-file gate, and record renderer
versions when it fails. That accepts occasional reviewed renderer-only updates
instead of requiring historical byte reproducibility. The measurements and
bundling findings above remain evidence for why such drift is expected.
