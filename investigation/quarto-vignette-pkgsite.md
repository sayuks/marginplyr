# Quarto Vignettes and pkgsite 0.1.0

Research date: 2026-07-25

## Conclusion

`vignettes/get_started.qmd` can be used directly as the source of an R package
vignette. The same file can also be used as an article on the altdoc Quarto
site. Therefore, **there is no need to migrate to pkgsite solely to establish a
single source of truth**.

The recommended approach is:

1. Keep altdoc.
2. Add Quarto vignette metadata to `vignettes/get_started.qmd`.
3. Add `VignetteBuilder: quarto` to `DESCRIPTION`. Because `quarto` is already
   listed in `Suggests`, no new dependency is needed.
4. Continue rendering the same `.qmd` as a normal Quarto website page for the
   website.
5. Let the CRAN vignette use the lightweight HTML produced by `quarto::html`.
   The `quartabs` tabset will work on the website, but interactive tabs may be
   disabled in the CRAN vignette's minimal HTML. Inspect the generated HTML.
   The content should remain readable as headings and prose if tabs are
   unavailable.

## 1. Can a `.qmd` file be a package vignette?

Yes. Version 1.4 of the Quarto R package registered a vignette engine for
`.qmd` files. Its official changelog explicitly records support for using
`quarto` as a vignette builder and `.qmd` files as vignettes.

- [quarto R package changelog](https://quarto-dev.github.io/quarto-r/news/index.html#quarto-14)

### DESCRIPTION

The required configuration is:

```text
Suggests:
    quarto (>= 1.4)
VignetteBuilder:
    quarto
```

The official Quarto HTML vignette example also specifies
`VignetteBuilder: quarto`.

- [Quarto HTML Vignettes](https://quarto-dev.github.io/quarto-r/articles/hello.html#quarto-as-vignette-builder)
- [Official vignette source](https://github.com/quarto-dev/quarto-r/blob/main/vignettes/hello.qmd)
- [Official quarto package DESCRIPTION](https://github.com/quarto-dev/quarto-r/blob/main/DESCRIPTION)

Writing R Extensions requires the package that provides a non-Sweave engine to
be listed in both `VignetteBuilder` and one of `Suggests`, `Imports`, or
`Depends`.

- [Writing R Extensions: VignetteBuilder](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file)
- [Writing R Extensions: Non-Sweave vignettes](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Non_002dSweave-vignettes)

marginplyr already lists `quarto` in `Suggests`, so only
`VignetteBuilder: quarto` is required. It is also reasonable to declare the
external tool with `SystemRequirements: Quarto`. Writing R Extensions asks
packages to declare external dependencies in `SystemRequirements`, and the
quarto package itself declares `SystemRequirements: Quarto command line tool`.

- [Writing R Extensions: SystemRequirements](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file)
- [quarto package DESCRIPTION](https://github.com/quarto-dev/quarto-r/blob/main/DESCRIPTION)

### YAML for `get_started.qmd`

To create an HTML vignette, add the following metadata to the existing YAML:

```yaml
vignette: >
  %\VignetteIndexEntry{Get started}
  %\VignetteEngine{quarto::html}
  %\VignetteEncoding{UTF-8}
```

`quarto::html` is the recommended engine for CRAN. It produces standalone HTML
and enforces a lightweight format without Bootstrap.

- [Quarto HTML Vignettes](https://quarto-dev.github.io/quarto-r/articles/hello.html#html-vignette-engines)
- [Custom Quarto Formats for Vignettes](https://quarto-dev.github.io/quarto-r/articles/advanced-vignettes.html#recommendations)

During an R package build or check, sources in `vignettes/` are processed, and
the generated HTML and its source are placed in `inst/doc` in the package
tarball.

- [Writing R Extensions: Writing package vignettes](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-package-vignettes)

## 2. Can one `.qmd` serve as both a package vignette and a website article?

Yes. Vignette metadata tells R which engine to use during a vignette build.
When the same file is rendered with ordinary `quarto render`, Quarto produces
normal HTML instead. The official Quarto documentation explicitly states that
the lightweight vignette format applies only when building the vignette;
ordinary `quarto render` uses the normal HTML format.

- [Quarto HTML Vignettes: interactive rendering limitation](https://quarto-dev.github.io/quarto-r/articles/hello.html)

The same source can therefore produce both outputs:

```text
vignettes/get_started.qmd
├─ R CMD build/check ── quarto::html ── CRAN/installed vignette
└─ altdoc/Quarto ───── normal HTML ──── GitHub Pages article
```

The current altdoc configuration already links
`vignettes/get_started.qmd` directly from its navbar and sidebar. The altdoc
Quarto backend also imports the package's `vignettes/` directory into its build
directory before rendering.

- [altdoc source: import_vignettes.R](https://github.com/etiennebacher/altdoc/blob/main/R/import_vignettes.R)
- [altdoc official documentation](https://altdoc.etiennebacher.com/)

The authoring source is therefore already singular. An internal altdoc copy in
a build directory is not a second, independently edited source.

### CRAN minimal HTML and `quartabs`

The `quarto::html` engine enforces:

```yaml
format:
  html:
    theme: none
    minimal: true
    embed-resources: true
```

- [Quarto HTML Vignettes](https://quarto-dev.github.io/quarto-r/articles/hello.html#html-vignette-engines)
- [quarto R source: utils-vignettes.R](https://github.com/quarto-dev/quarto-r/blob/main/R/utils-vignettes.R)

The official Quarto HTML documentation says that `minimal: true` disables the
built-in Bootstrap theme, anchors, popups, tabsets, code-copy controls, and
responsive figures.

- [Quarto HTML Basics: Minimal HTML](https://quarto.org/docs/output-formats/html-basics.html#minimal-html)
- [Quarto HTML Basics: Tabsets](https://quarto.org/docs/output-formats/html-basics.html#tabsets)

`quartabs::render_tabset()` emits Quarto tabset markup, so it can appear as
tabs on the normal website. The CRAN vignette, however, is subject to the
minimal HTML restrictions. Non-Bootstrap tabset behavior has also changed
between Quarto releases. After `R CMD build`, inspect
`inst/doc/get_started.html` and verify that:

- `Total` appears as the first tab or first heading;
- content for every region remains present and readable;
- all content remains reachable without JavaScript;
- the vignette HTML is not excessively large.

The `quarto::format` engine can retain a full Quarto format in the CRAN
vignette, but the official Quarto guidance does not recommend heavy,
Bootstrap-based formats for CRAN vignettes. It is better to make the CRAN
version readable when statically expanded than to switch to `quarto::format`
only to preserve tabs.

- [Custom Quarto Formats for Vignettes](https://quarto-dev.github.io/quarto-r/articles/advanced-vignettes.html)

## 3. Can pkgsite 0.1.0 replace altdoc?

### It can, but pkgsite is not what enables the single-source design

The central purpose of pkgsite 0.1.0 is to convert `man/*.Rd` files into
individual `reference/*.qmd` files and a reference index.
`write_reference()` is its main entry point. Registering a package vignette
engine and collecting vignettes are not pkgsite responsibilities.

- [pkgsite 0.1.0 README](https://github.com/edgararuiz/pkgsite/blob/v0.1.0/README.md)
- [pkgsite write_reference implementation](https://github.com/edgararuiz/pkgsite/blob/v0.1.0/R/write-reference.R)
- [CRAN pkgsite 0.1.0](https://cran.r-project.org/package=pkgsite)

Even after adopting pkgsite, `get_started.qmd` would become a vignette through
the same `VignetteBuilder: quarto` field and `quarto::html` metadata. On the
website side, `vignettes/get_started.qmd` would need to be added manually to
`_quarto.yml` as a normal Quarto website page. Single-source operation is
therefore a result of Quarto and the repository layout, not a pkgsite-specific
feature.

### Advantages of pkgsite

- The repository itself can be managed directly as a Quarto project.
- Quarto `freeze` is straightforward to use directly.
- Reference-page `.qmd` templates and ordering are highly configurable.
- R and Python reference documentation can be combined in one Quarto site.

The official pkgsite README gives these as intended use cases.

- [pkgsite README: When should I use pkgsite?](https://github.com/edgararuiz/pkgsite/blob/v0.1.0/README.md#when-should-i-use-pkgsite)
- [pkgsite README: Customize the pages](https://github.com/edgararuiz/pkgsite/blob/v0.1.0/README.md#customize-the-pages)

### Constraints of pkgsite 0.1.0

- Version 0.1.0 is an initial release focused on reference conversion.
- Users assemble `_quarto.yml`, the navbar and sidebar, README and NEWS
  wrappers, reference generation, downlit auto-linking, and GitHub Actions for
  rendering and deployment.
- pkgsite itself says that mature pkgdown will suit most projects that do not
  specifically require Quarto.
- pkgsite does not provide the same integrated orchestration as altdoc's
  `setup_docs()`, `render_docs()`, and deployment setup.

- [pkgsite README](https://github.com/edgararuiz/pkgsite/blob/v0.1.0/README.md)
- [pkgsite: Creating the Quarto website](https://github.com/edgararuiz/pkgsite/blob/v0.1.0/articles/quarto-website.qmd)
- [pkgsite: Publishing with GitHub Actions](https://github.com/edgararuiz/pkgsite/blob/v0.1.0/articles/github-actions.qmd)
- [pkgsite NEWS](https://github.com/edgararuiz/pkgsite/blob/v0.1.0/NEWS.md)

marginplyr already has working altdoc Actions and GitHub Pages deployment.
Migration to pkgsite is worth considering separately if the project wants to
eliminate the staged build and manage a root Quarto project directly, or make
`freeze` central to its documentation architecture. It is not required to
turn the current `.qmd` into a vignette.

## Minimal implementation

```diff
 Suggests:
-    quarto,
+    quarto (>= 1.4),
 ...
+VignetteBuilder:
+    quarto
+SystemRequirements: Quarto command line tool
```

```diff
 ---
 title: Get started
+vignette: >
+  %\VignetteIndexEntry{Get started}
+  %\VignetteEngine{quarto::html}
+  %\VignetteEncoding{UTF-8}
 format:
   html:
     code-tools: true
     toc: true
     toc-expand: true
     number-sections: true
 ---
```

After implementation, verify at least `R CMD build`,
`R CMD check --as-cran` on the tarball, `tools::buildVignettes()`,
`vignette("get_started", package = "marginplyr")` after installation, and an
altdoc render.
