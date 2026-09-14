# README generation in representative tidyverse repositories

Investigated: 2026-09-14

## Question

How did representative repositories maintained in the tidyverse organization
manage an executable README, and what did that practice imply for
`marginplyr`'s choice among an exact renderer pin, the latest stable Quarto,
and no generated-file synchronization check?

The survey covered `dplyr`, `tidyr`, `readr`, `purrr`, `ggplot2`, `tibble`, and
the `tidyverse` package. `usethis`, `devtools`, and `pkgdown` were also read
because they own the current package-development convention and the tools that
implement it. Every repository claim below refers to the named default-branch
commit, not to an assumed permanent state.

## Summary

The seven tidyverse repositories did not provide a precedent for making an
executable README reproducible in CI. At the surveyed commits, all seven kept
`README.Rmd` and generated `README.md`, build-ignored the source, and had no
repository-visible workflow step that rendered the README or failed on a
README diff. Six used the standard r-lib Actions workflows; those installed
Pandoc for package checks and pkgdown, but did not rebuild the README. Tibble's
custom workflow also installed Pandoc indirectly and committed some generated
artifacts, but its pre-commit mutation steps did not render the README.

That fleet represented the older convention, not the direction of the current
tidyverse tooling. At the surveyed `usethis` commit,
`create_tidy_package()` called `use_readme_qmd()`, and the QMD template rendered
to GFM. The change was introduced on 2026-03-05. The upkeep and release
checklists still treated `devtools::build_readme()` as a human-run task rather
than a repository-visible CI invariant.

The best fit for `marginplyr` was therefore not to copy the surveyed fleet's
manual-only synchronization. It was to adopt the current QMD direction, use
the latest stable Quarto with its bundled Pandoc, and retain the existing
read-only regeneration-and-diff gate. This gave contributors one renderer and
one command, avoided separately choosing a Pandoc version, and made renderer
drift an explicit reviewed repository change rather than an unnoticed stale
README or a bot push.

## Survey method

The default branches were cloned at the following commits and searched,
including dot-directories, for executable README sources and for
`README.Rmd`, `README.qmd`, `build_readme`, `rmarkdown::render`,
`setup-pandoc`, `setup-quarto`, renderer version inputs, README diffs, and
pre-commit references in every workflow. The GitHub commits API was also used
to find the latest change to each README source and output. The inspection
covered repository-visible mechanisms only: a maintainer's untracked local Git
hook could not be observed and would not be inherited by a fresh clone.

| Repository | Surveyed commit | README source and generated output | Repository-visible README synchronization | Documentation tool setup |
|---|---|---|---|---|
| dplyr | [`d5e94e7`](https://github.com/tidyverse/dplyr/tree/d5e94e7fa8fd4a5f79c1a707d1842216bb4c691f) | [`README.Rmd` used `github_document` and named `README.md` as generated](https://github.com/tidyverse/dplyr/blob/d5e94e7fa8fd4a5f79c1a707d1842216bb4c691f/README.Rmd#L1-L12); both files existed and [the source was build-ignored](https://github.com/tidyverse/dplyr/blob/d5e94e7fa8fd4a5f79c1a707d1842216bb4c691f/.Rbuildignore#L6) | No render or README-diff step was found in the [workflow directory](https://github.com/tidyverse/dplyr/tree/d5e94e7fa8fd4a5f79c1a707d1842216bb4c691f/.github/workflows) | `setup-pandoc@v2` appeared in [R CMD check](https://github.com/tidyverse/dplyr/blob/d5e94e7fa8fd4a5f79c1a707d1842216bb4c691f/.github/workflows/R-CMD-check.yaml#L43-L59) and [pkgdown](https://github.com/tidyverse/dplyr/blob/d5e94e7fa8fd4a5f79c1a707d1842216bb4c691f/.github/workflows/pkgdown.yaml#L25-L41), without a version input; no Quarto setup was found |
| tidyr | [`26f83e8`](https://github.com/tidyverse/tidyr/tree/26f83e89a690b6cf31a260489b828df0ff43ebb2) | [`README.Rmd` + `README.md`](https://github.com/tidyverse/tidyr/blob/26f83e89a690b6cf31a260489b828df0ff43ebb2/README.Rmd#L1-L12); [source build-ignore](https://github.com/tidyverse/tidyr/blob/26f83e89a690b6cf31a260489b828df0ff43ebb2/.Rbuildignore#L12-L13) | No render or README-diff step in the [workflow directory](https://github.com/tidyverse/tidyr/tree/26f83e89a690b6cf31a260489b828df0ff43ebb2/.github/workflows) | Unconfigured `setup-pandoc@v2` in [R CMD check](https://github.com/tidyverse/tidyr/blob/26f83e89a690b6cf31a260489b828df0ff43ebb2/.github/workflows/R-CMD-check.yaml#L44-L50), the development check, and pkgdown; no Quarto setup |
| readr | [`238ea87`](https://github.com/tidyverse/readr/tree/238ea873fdc1a34b3638f01493bd8df9f770ac62) | [`README.Rmd` + `README.md`](https://github.com/tidyverse/readr/blob/238ea873fdc1a34b3638f01493bd8df9f770ac62/README.Rmd#L1-L12); [source build-ignore](https://github.com/tidyverse/readr/blob/238ea873fdc1a34b3638f01493bd8df9f770ac62/.Rbuildignore#L16-L17) | No render or README-diff step in the [workflow directory](https://github.com/tidyverse/readr/tree/238ea873fdc1a34b3638f01493bd8df9f770ac62/.github/workflows) | Unconfigured `setup-pandoc@v2` in [R CMD check](https://github.com/tidyverse/readr/blob/238ea873fdc1a34b3638f01493bd8df9f770ac62/.github/workflows/R-CMD-check.yaml#L43-L49) and pkgdown; no Quarto setup |
| purrr | [`481e829`](https://github.com/tidyverse/purrr/tree/481e829f297fd4315b386518215157f361475ad0) | [`README.Rmd` + `README.md`](https://github.com/tidyverse/purrr/blob/481e829f297fd4315b386518215157f361475ad0/README.Rmd#L1-L12); [source build-ignore](https://github.com/tidyverse/purrr/blob/481e829f297fd4315b386518215157f361475ad0/.Rbuildignore#L10-L11) | No render or README-diff step in the [workflow directory](https://github.com/tidyverse/purrr/tree/481e829f297fd4315b386518215157f361475ad0/.github/workflows) | Unconfigured `setup-pandoc@v2` in [R CMD check](https://github.com/tidyverse/purrr/blob/481e829f297fd4315b386518215157f361475ad0/.github/workflows/R-CMD-check.yaml#L44-L50) and pkgdown; no Quarto setup |
| ggplot2 | [`0ac300f`](https://github.com/tidyverse/ggplot2/tree/0ac300f2aeaf259c84e9205c53b3d1189452043a) | [`README.Rmd` + `README.md`](https://github.com/tidyverse/ggplot2/blob/0ac300f2aeaf259c84e9205c53b3d1189452043a/README.Rmd#L1-L13); [source build-ignore](https://github.com/tidyverse/ggplot2/blob/0ac300f2aeaf259c84e9205c53b3d1189452043a/.Rbuildignore#L21-L22) | No render or README-diff step in the [workflow directory](https://github.com/tidyverse/ggplot2/tree/0ac300f2aeaf259c84e9205c53b3d1189452043a/.github/workflows) | Unconfigured `setup-pandoc@v2` in [R CMD check](https://github.com/tidyverse/ggplot2/blob/0ac300f2aeaf259c84e9205c53b3d1189452043a/.github/workflows/R-CMD-check.yaml#L50-L57) and pkgdown; no explicit Quarto setup |
| tibble | [`6ea0165`](https://github.com/tidyverse/tibble/tree/6ea01654c46b20b581bda36a66dee8065ccac308) | [`README.Rmd` + `README.md`](https://github.com/tidyverse/tibble/blob/6ea01654c46b20b581bda36a66dee8065ccac308/README.Rmd#L1-L14); [source build-ignore](https://github.com/tidyverse/tibble/blob/6ea01654c46b20b581bda36a66dee8065ccac308/.Rbuildignore#L10-L12) | Its smoke job styled code, updated snapshots, and roxygenized before a [generic generated-change commit](https://github.com/tidyverse/tibble/blob/6ea01654c46b20b581bda36a66dee8065ccac308/.github/workflows/R-CMD-check.yaml#L160-L191), but contained no README render | Environment setup was delegated to [`cynkratemplate` at the floating `main` ref](https://github.com/tidyverse/tibble/blob/6ea01654c46b20b581bda36a66dee8065ccac308/.github/workflows/R-CMD-check.yaml#L108-L121); the referenced install action [called unconfigured `setup-pandoc@v2`](https://github.com/cynkra/cynkratemplate/blob/de9caa21061d9d51a434442f50f1d31c5ad76ff9/.github/actions/install/action.yml#L96-L122). No Quarto setup was found |
| tidyverse | [`0231aaf`](https://github.com/tidyverse/tidyverse/tree/0231aafbc56914ee5371dd6c7b60677f168d7154) | [`README.Rmd` + `README.md`](https://github.com/tidyverse/tidyverse/blob/0231aafbc56914ee5371dd6c7b60677f168d7154/README.Rmd#L1-L12); [source build-ignore](https://github.com/tidyverse/tidyverse/blob/0231aafbc56914ee5371dd6c7b60677f168d7154/.Rbuildignore#L5-L6) | No render or README-diff step in the [workflow directory](https://github.com/tidyverse/tidyverse/tree/0231aafbc56914ee5371dd6c7b60677f168d7154/.github/workflows) | Unconfigured `setup-pandoc@v2` in [R CMD check](https://github.com/tidyverse/tidyverse/blob/0231aafbc56914ee5371dd6c7b60677f168d7154/.github/workflows/R-CMD-check.yaml#L44-L50) and pkgdown; no Quarto setup |

`r-lib/usethis` and `r-lib/pkgdown` also retained `README.Rmd` at the surveyed
commits ([usethis](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/README.Rmd#L1-L14),
[pkgdown](https://github.com/r-lib/pkgdown/blob/f69b62a7e74b58b42170284b6f3f56c674e7a3f8/README.Rmd#L1-L13)),
while `r-lib/devtools` kept only a hand-written
[`README.md`](https://github.com/r-lib/devtools/blob/8fcec5a0664001c79d215c9161edf60f356bafb0/README.md#L1-L12).
Neither executable-README repository had a repository-visible README render or
diff workflow. This reinforced that the sampled installed base lagged the new
template; it did not make Rmd the current recommendation.

The absence of an explicit Quarto setup in ggplot2 did not mean that its
vignettes avoided Quarto. Its `DESCRIPTION` selected Quarto as the
[`VignetteBuilder`](https://github.com/tidyverse/ggplot2/blob/0ac300f2aeaf259c84e9205c53b3d1189452043a/DESCRIPTION#L66-L79),
and `setup-r-dependencies` auto-installed the latest released Quarto when it
found `.qmd` files
([action inputs](https://github.com/r-lib/actions/blob/8efa4208c1e4a4837731d3c22011c7550205df2f/setup-r-dependencies/action.yaml#L49-L63)).
That implicit setup served package documentation, not a README synchronization
check.

## What actually kept the sampled READMEs current

The primary tidyverse guidance said that an executable README had to be
rendered regularly, recommended `devtools::build_readme()`, installed a local
pre-commit hook, and included the same command in release/upkeep checklists:

- The current `use_readme_qmd()`/`use_readme_rmd()` implementation explicitly
  said to render regularly and described the local hook
  ([source](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/R/readme.R#L8-L28)).
  The hook only acted when a README source was staged, compared mtimes, required
  source and output to be staged together, and could be bypassed with
  `--no-verify`
  ([template](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/inst/templates/readme-rmd-pre-commit.sh#L1-L14)).
- The current tidy upkeep checklist ended with
  `devtools::build_readme()`
  ([source](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/R/upkeep.R#L312-L327)),
  and the release checklist included it before package checks
  ([source](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/R/release.R#L117-L130)).
- `pkgdown` did not close the synchronization gap. Its own documentation said
  that it consumed `README.md` for the home page and deliberately did not knit
  `README.Rmd`; generation of the corresponding Markdown was the package
  author's responsibility
  ([`build_home()` documentation](https://pkgdown.r-lib.org/reference/build_home.html#home-page)).

The commit history was consistent with a maintainer-run command. At the
surveyed commits, the latest source/output update was the same commit in dplyr
([`4b85b96`](https://github.com/tidyverse/dplyr/commit/4b85b96099e5b147bf5c924121b5106cf93c93b8)),
tidyr ([`de737f3`](https://github.com/tidyverse/tidyr/commit/de737f38c47b6751d248d1e8f0e73626717e8272)),
purrr ([`efca82a`](https://github.com/tidyverse/purrr/commit/efca82a7fe1e3a68b2b29a7d745165d43f70169e)),
ggplot2 ([`00c2667`](https://github.com/tidyverse/ggplot2/commit/00c2667b0579b684f9826efeb2034487383adfd2)),
tibble ([`7fa5311`](https://github.com/tidyverse/tibble/commit/7fa53116e55bb5123bdbc5e93ff609ab10298f93)),
and tidyverse ([`b6bcbcd`](https://github.com/tidyverse/tidyverse/commit/b6bcbcd3432665ba943ec29e2ef188fdfda6b669)).
Readr provided especially clear evidence of periodic regeneration: its
`README.Rmd` last changed in the 2025 upkeep, then a separate 2026
[`build_readme()` commit](https://github.com/tidyverse/readr/commit/89d57bf24be1fc4801d16d35c2861b065daffde9)
updated only `README.md` to reflect new dependency versions. This practice
could work well with attentive maintainers, but it did not prove on every pull
request that the output matched the source and installed package.

## Rmd in the fleet versus QMD in the current standard

At `eefc9fd`, `create_tidy_package()` created a QMD README
([source](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/R/tidyverse.R#L61-L73)).
That switch was introduced by the 2026-03-05
[`use_readme_qmd()` commit](https://github.com/r-lib/usethis/commit/54ed979a9555cc012cc39e1308177a972f81d4c4).
The template selected Quarto GFM and kept generated `README.md`
([template](https://github.com/r-lib/usethis/blob/eefc9fd26c44b6c3eca7672dbedfb5b2ec284c03/inst/templates/package-README-qmd#L1-L15)).
Consequently, the absence of QMD in the seven long-lived repositories was an
installed-base observation, not evidence against migrating a package that was
already using Quarto elsewhere.

Current `devtools::build_readme()` also made the contributor command the same
for Rmd and QMD. It selected `README.qmd` first, installed the working source
into a temporary library, and rendered in a clean process
([contract and dispatch](https://github.com/r-lib/devtools/blob/8fcec5a0664001c79d215c9161edf60f356bafb0/R/build-readme.R#L85-L132));
for QMD it passed the temporary library to the Quarto subprocess and called
`quarto::quarto_render()`
([implementation](https://github.com/r-lib/devtools/blob/8fcec5a0664001c79d215c9161edf60f356bafb0/R/build-readme.R#L134-L162)).
This matched `marginplyr`'s requirement that README examples execute against
the working tree rather than an older user-library installation.

## Pandoc and Quarto version policy in the evidence

It would be inaccurate to summarize the sampled tidyverse workflows as
"always use latest Pandoc." They omitted the `pandoc-version` input, but at the
surveyed r-lib Actions `v2` commit that input defaulted to the exact version
`3.8.3`
([action definition](https://github.com/r-lib/actions/blob/8efa4208c1e4a4837731d3c22011c7550205df2f/setup-pandoc/action.yml#L6-L20)).
Because `v2` was a moving major tag, that was an implicit centrally maintained
default rather than a repository-owned immutable pin. More importantly, those
workflow installations served R CMD check and pkgdown; none of the six
standard workflows used them to verify `README.md`.

Quarto's policy supported treating Quarto plus its bundled Pandoc as one
compatibility unit: its installers bundled the required dependencies because
Quarto output was tailored to them
([Quarto FAQ](https://quarto.org/docs/faq/index.html#development-and-maintenance)),
and `quarto pandoc` explicitly ran the embedded Pandoc
([CLI reference](https://quarto.org/docs/cli/)). A standalone Pandoc setup or
pin next to Quarto therefore introduced a second ownership path without making
QMD output more reproducible.

At the surveyed `quarto-actions/setup@v2` commit, omitting `version` selected
`release`, and the action resolved that to the latest stable release at run
time
([action definition](https://github.com/quarto-dev/quarto-actions/blob/8a96df13519ee81fd526f2dfca5962811136661b/setup/action.yml#L1-L8),
[resolution](https://github.com/quarto-dev/quarto-actions/blob/8a96df13519ee81fd526f2dfca5962811136661b/setup/action.yml#L46-L67)).
That was the concrete mechanism for a "keep using current Quarto" policy.

## Options for `marginplyr`

| Policy | Maintainability | Historical byte reproducibility | Contributor experience | CI/upstream behavior | Security and review |
|---|---|---|---|---|---|
| Exact Quarto pin, bundled Pandoc, byte gate | Requires periodic explicit upgrades and regeneration; one renderer to manage | Strong for README generation while the R/package dependency environment also remains available | Contributors must match the pinned Quarto or rely on CI | Quiet between planned upgrades; drift appears only when the pin moves | Read-only gate can remain; upgrades are reviewed |
| **Latest stable Quarto, bundled Pandoc, byte gate** | **No version bump administration; one occasional reviewed README update when output changes** | **Does not recreate old bytes indefinitely, but every commit still proves source/output agreement under the CI renderer used then** | **One current Quarto install and `devtools::build_readme()`; a mismatch includes an actionable regenerated diff** | **An upstream stable release can make an otherwise unrelated PR red; logging both versions identifies why, and the repair is explicit** | **Read-only job; no bot write credential; generated changes stay in the PR review path** |
| Latest stable Quarto, no synchronization gate (closest to the sampled fleet) | Least CI machinery, but correctness depends on memory, local hooks, and release/upkeep rituals | None between manual renders | Fewer CI failures, but a fresh clone has no hook and package-code-only changes are not detected | Upstream drift and stale executed output can remain invisible | Read-only, but no automated provenance for the committed README |
| Latest stable Quarto, bot renders and commits | Bot repairs output automatically | The resulting commit records whatever renderer ran, without making old environments recoverable | Contributors need not regenerate, but receive follow-up commits and possible branch conflicts | Renderer releases can create unsolicited repository churn | Requires `contents: write`; generated changes can bypass the contributor's original review cycle |

The byte gate was not intrinsically the only possible comparison. A semantic
Markdown/AST comparison could ignore selected writer-only differences, but it
would create a second specification of which generated changes were safe to
discard. That comparator would itself need maintenance and could hide a real
reader-visible change. For a tracked generated artifact, exact `git diff` was
the simpler complete synchronization predicate.

## Recommendation

Adopt **latest stable Quarto + its bundled Pandoc + an exact generated README
diff gate**:

1. Migrate `README.Rmd` to `README.qmd` using the current usethis GFM shape.
   This followed the current tidyverse package template and consolidated the
   repository on the renderer it already used for vignettes and the site.
2. In the README documentation job, set up Quarto with `version: release` or
   its equivalent default. Remove standalone Pandoc setup and all repository
   checks for an exact Pandoc or Quarto number.
3. Render with one documented contributor command,
   `devtools::build_readme()`, after installing the dependencies it needs.
   Keep CI's regeneration followed by `git diff --exit-code README.md` so the
   committed GitHub, CRAN, and site entry point is reviewed and synchronized.
4. Print `quarto --version` and `quarto pandoc --version` in the job. These are
   observations for diagnosing a drift failure, not version requirements.
5. Keep the synchronization job read-only and do not auto-commit. GitHub's
   security guidance recommends least-privilege tokens and notes that a full
   action commit SHA is the only immutable action reference
   ([secure-use guidance](https://docs.github.com/en/actions/reference/security/secure-use)).
   Action-code pinning with automated updates can be considered separately
   from leaving the Quarto `version: release` input floating.
6. Document the expected failure mode: after a stable Quarto release changes
   GFM output, regenerate once, review the Markdown diff, and commit it. Such a
   failure is the latest-renderer policy detecting a required repository
   update, not an inexplicable flaky test.

This recommendation deliberately differed from the surveyed repositories in
one respect: it kept a stronger source/output invariant. That was justified by
`marginplyr`'s larger executable README, its existing use of README output in
multiple published surfaces, and its existing release automation. The
tidyverse fleet showed that manual regeneration was workable; the current
usethis standard showed that QMD was the forward path. Neither provided a
reason to discard a working read-only correctness gate.
