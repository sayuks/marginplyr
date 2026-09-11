# Can attachment detect unused Suggests?

Investigated: 2026-09-11

This investigation asked whether the `attachment` package can replace the
manual audit for unused entries in `DESCRIPTION`'s `Suggests` field. It read
the CRAN 1.1.0 source, the upstream source at commit
[`8e73618c`](https://github.com/ThinkR-open/attachment/tree/8e73618c051c394d5dbe094129eb1065821dbf3d),
and R Core's dependency rules, then ran a small package fixture against the
installed CRAN release.

## Conclusion

`attachment::att_amend_desc()` can expose an unused Suggest as a removal, but
it is not an authoritative unused-Suggests checker. It reconstructs Imports
and Suggests from the package references its scanners recognise, reports the
diff, and writes the reconstructed fields back to `DESCRIPTION`. A declared
Suggest that the scan does not rediscover is therefore removed whether it is
genuinely unused or merely outside the scanner's model. The CRAN 1.1.0 source
builds the replacement table with an outer join only on the newly inferred
packages, then calls `desc$del_deps()` and `desc$set_deps()`
([CRAN 1.1.0 source tarball](https://cran.r-project.org/src/contrib/attachment_1.1.0.tar.gz),
`R/att_to_description.R`, lines 347--457; the same reconstruction is visible
in the [upstream source](https://github.com/ThinkR-open/attachment/blob/8e73618c051c394d5dbe094129eb1065821dbf3d/R/att_to_description.R#L314-L419)).

No exported read-only unused-dependency checker was present in the 1.1.0
reference index. The operation that produces removals is the mutating
`att_amend_desc()` / `att_to_desc_from_is()` path
([official reference index](https://thinkr-open.github.io/attachment/reference/index.html),
[official `att_amend_desc()` reference](https://thinkr-open.github.io/attachment/reference/att_amend_desc.html)).
It could be run on a temporary copy and its diff treated as an advisory signal,
but accepting that diff still requires a semantic dependency audit.

## What the package scanner models

With its defaults, `att_amend_desc()` assigns dependencies found in
`NAMESPACE` and `R/` to Imports. It assigns dependencies found in roxygen
examples, `vignettes/`, and `tests/` to Suggests, then adds anything named in
`extra.suggests`. These locations default to `R`, `vignettes`, and `tests`
([CRAN 1.1.0 source tarball](https://cran.r-project.org/src/contrib/attachment_1.1.0.tar.gz),
`R/att_to_description.R`, lines 192--241; [inspectable upstream copy](https://github.com/ThinkR-open/attachment/blob/8e73618c051c394d5dbe094129eb1065821dbf3d/R/att_to_description.R#L187-L236)).
The location determines the dependency field; the scanner does not decide
whether a reference in `R/` is optional at runtime.

The R-script scanner walks the syntax tree and recognises `::`, `:::`, and a
fixed set of calls: `library()`, `require()`, `requireNamespace()`,
`loadNamespace()`, `use()`, and `getFromNamespace()`. It intentionally does not
treat `packageVersion()`, `getNamespace()`, `asNamespace()`, or
`attachNamespace()` as dependency introducers
([CRAN 1.1.0 scanner source](https://github.com/ThinkR-open/attachment/blob/8e73618c051c394d5dbe094129eb1065821dbf3d/R/att_from_rscripts.R#L35-L165)).
It has no symbol-to-package resolution for an ordinary bare call. A bare call
in package code is covered only when the package also appears in an `import()`
or `importFrom()` directive, because the separate NAMESPACE scanner extracts
those directives
([NAMESPACE scanner](https://github.com/ThinkR-open/attachment/blob/8e73618c051c394d5dbe094129eb1065821dbf3d/R/att_from_namespace.R#L26-L58)).
There is no corresponding attribution source for a bare call in a test or
vignette unless some scanned file also names the package explicitly.

Control flow does not change classification. The AST walker descends into an
`if` body, so `if (guard) arrow::schema()` is detected, but its occurrence in
`R/` still places `arrow` in Imports. The upstream edge-case fixture records
both this traversal and limitations such as unresolved dynamic package names
and absent scope analysis
([official edge-case fixture](https://github.com/ThinkR-open/attachment/blob/8e73618c051c394d5dbe094129eb1065821dbf3d/dev/manual_detection_edge_cases.R#L250-L375)).
This conflicts with using the result to infer optionality: R Core explicitly
allows packages used conditionally in function bodies, examples, tests, or
vignettes to be Suggests, and recommends a `requireNamespace()` guard with a
qualified call
([Writing R Extensions, “Suggested packages”](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Suggested-packages)).

Tests are recursively passed to the R-script scanner. R Markdown and Quarto
vignettes are first converted to R with `knitr::purl()`, their YAML output
package is appended, and that R is scanned. When the scanned path contains
`vignettes`, attachment 1.1.0 also adds `knitr` unconditionally and adds
`rmarkdown` for `.Rmd` files or `quarto` for `.qmd` files
([vignette scanner](https://github.com/ThinkR-open/attachment/blob/8e73618c051c394d5dbe094129eb1065821dbf3d/R/att_from_rmds.R#L25-L147)).
Thus the earlier failure mode in which an ordinary vignette lost `knitr` no
longer applies to CRAN 1.1.0. The scanner still does not read the
`VignetteBuilder` field, so the hard-coded engine inference cannot establish
arbitrary builders or a builder's additional direct requirements. R Core
requires a non-Sweave engine provider to appear in both `VignetteBuilder` and
one of Depends, Imports, or Suggests, and notes that all packages needed by the
engine must be declared directly
([Writing R Extensions, package metadata](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file),
[Writing R Extensions, non-Sweave vignettes](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Non_002dSweave-vignettes)).

Roxygen examples receive another specialised pass. The 1.1.0 implementation
scans only the immediate `.R` files in `R/`, extracts `@examples` and
`@examplesIf` bodies, and deliberately discards the `@examplesIf` condition
itself before using the R-script scanner
([example scanner](https://github.com/ThinkR-open/attachment/blob/8e73618c051c394d5dbe094129eb1065821dbf3d/R/add_from_examples.R#L16-L99)).

## Reproduction

The fixture used R 4.6.1 and the installed CRAN `attachment` 1.1.0. Its initial
metadata declared Imports `glue` and `stats`, and Suggests `covr`, `knitr`,
`rmarkdown`, `testthat`, `tidyr`, and `withr`. Its sources contained:

- `glue::glue()` in `R/`;
- guarded `requireNamespace("DBI")` plus `DBI::ANSI()` in `R/`;
- a bare `setNames()` backed by `importFrom(stats,setNames)`;
- `library(testthat)` in `tests/testthat.R`, plus bare `local_tempdir()` from
  withr and bare testthat expectations in a test;
- `tidyr::nest()` in an R Markdown vignette using `knitr::rmarkdown`.

The individual scanners returned:

```text
R          glue, DBI
NAMESPACE  stats
tests      testthat
vignettes  knitr, rmarkdown, tidyr
```

Running `att_amend_desc(document = FALSE, must.exist = FALSE,
check_if_suggests_is_installed = FALSE, use.config = FALSE)` then reported:

```text
[-] 2 package(s) removed: covr, withr.
[+] 1 package(s) added: DBI.
```

The result correctly removed the deliberately unused `covr`, but also removed
the genuinely used `withr` because its test used a bare name. It classified
guarded `DBI` as an Import because it appeared in `R/`, despite the code's
optional-use shape. It retained `knitr` and `rmarkdown` through the 1.1.0
vignette special case.

A second run on a temporary copy of the marginplyr tree demonstrated the same
limits against the package under consideration. It removed the declared
Suggests `quartabs` and `tidyr`, moved the conditional backend packages
`arrow`, `data.table`, and `DBI` to Imports, and inferred `pkg` and `tools` as
new Suggests. The direct vignette scan also emitted evaluation errors for
chunk-option expressions such as `has_tidyr` and `has_duckdb` before returning
its partial package list. This was an observation of the tree on the
investigation date, not a maintained description of its dependencies.

## Relationship to R CMD check

R Core states that all packages needed for checking must be declared and that
Imports should not contain packages that are neither imported through
`NAMESPACE` nor accessed through `::` or `:::`; `R CMD check` checks that
Imports rule. The manual gives no equivalent promise that `R CMD check`
reports every unused Suggest
([Writing R Extensions, “Package Dependencies”](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies)).
It also describes its dependency-use check as non-exhaustive
([Writing R Extensions, checking package subdirectories](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Checking-package-subdirectories)).

The two mechanisms therefore answer different incomplete questions.
`R CMD check` is the package-boundary check; attachment reconstructs metadata
from recognisable static references. Using attachment as an automatic clean
gate would turn missed references into deletions and would promote conditional
dependencies found in `R/`. For marginplyr, its output can at most supplement
the existing cross-directory grep and human classification audit; it cannot
replace that audit or prove a Suggest is semantically optional.
