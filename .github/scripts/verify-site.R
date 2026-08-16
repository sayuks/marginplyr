# Verifies the built site. What it checks is derived from what the package
# ships rather than written out here.
#
# The list of required pages used to be hand-written, so `recipes.qmd` shipped
# as the fifth vignette with nothing asserting that its page existed, held
# anything, or was free of leaked paths: a silent render failure of it left
# this job green (#114). That is the defect `generate-backend-matrix.R` was
# introduced to remove from the `backend` matrix, and the fix is the same one --
# derive, do not enumerate. Every `.qmd` under `vignettes/` and every
# non-internal `.Rd` under `man/` names a page, and every page named that way
# gets the baseline: it exists, it rendered to completion, it carries no
# build-machine path, and it holds none of the strings no page may hold. Adding
# a vignette or an exported function therefore needs no edit here to be covered
# by it.
#
# `markers` is what stays hand-written, and it sits on top of that baseline
# rather than deciding coverage. Naming a page there adds prose the page must
# contain -- prose a chunk had to run to produce, so it fails when a render
# succeeds while executing nothing. A page absent from `markers` still gets
# everything above. A `markers` key naming no derived page is an error, so
# renaming a vignette cannot quietly drop its markers instead of moving them.

source(".github/scripts/ci-helpers.R")

read_page <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# `\keyword{internal}` is the rule altdoc applies when deciding that a topic
# documents internals and gets no page of its own. Stating the rule rather than
# naming the topic it currently excludes keeps a second internal topic from
# demanding a page that will never be rendered.
documents_internals <- function(path) {
  any(grepl(
    "\\keyword{internal}",
    readLines(path, warn = FALSE),
    fixed = TRUE
  ))
}

# The page a source renders to. Extension swap only: `README` is the one source
# altdoc renames on the way out, and its page is named directly below.
#
# The branch is not decoration. `file.path()` is vectorized, so a zero-length
# component makes the whole call `character(0)` -- passing `NULL` through it
# would drop every page in the set rather than putting it at the top level, and
# a page set that lost a whole directory this way would still pass everything
# below it.
page_for <- function(sources, subdirectory = NULL) {
  html <- paste0(tools::file_path_sans_ext(basename(sources)), ".html")
  if (is.null(subdirectory)) {
    file.path("docs", html)
  } else {
    file.path("docs", subdirectory, html)
  }
}

vignette_sources <- sort(Sys.glob("vignettes/*.qmd"))
reference_sources <- sort(Sys.glob("man/*.Rd"))
reference_sources <- reference_sources[
  !vapply(reference_sources, documents_internals, logical(1))
]

# altdoc also renders a page for repository files that are not this package's
# own documents, and `altdoc/quarto_website.yml` names them: it writes each
# `file: $ALTDOC_*` slot as a page when the repository has the file behind it,
# and drops the slot when it does not. So the placeholder alone cannot say
# whether a page is required, and this table supplies the missing half by
# pairing each with its file.
#
# The table is a list, which is what this script exists to remove, so the
# assertion below is what keeps it from behaving like one: a `file:` slot the
# site config declares and this table does not name stops the script instead of
# escaping it. Adding `CHANGELOG.md` to the repository therefore cannot ship an
# unverified page -- the way adding `recipes.qmd` once did.
imported_sources <- c(
  ALTDOC_NEWS = "NEWS.md",
  ALTDOC_CHANGELOG = "CHANGELOG.md",
  ALTDOC_LICENSE = "LICENSE.md",
  ALTDOC_LICENCE = "LICENCE.md",
  ALTDOC_CODE_OF_CONDUCT = "CODE_OF_CONDUCT.md",
  ALTDOC_CITATION = "inst/CITATION"
)

site_config <- readLines("altdoc/quarto_website.yml", warn = FALSE)
declared <- sub(
  ".*\\$(ALTDOC_[A-Z_]+).*",
  "\\1",
  grep("^\\s*file:\\s*\\$ALTDOC_", site_config, value = TRUE)
)
unnamed <- setdiff(declared, names(imported_sources))
if (length(unnamed) > 0L) {
  stop(
    call. = FALSE,
    "altdoc/quarto_website.yml renders pages this script cannot name: ",
    paste(unnamed, collapse = ", "),
    ". Add each to `imported_sources` with the repository file behind it."
  )
}

imported_sources <- imported_sources[declared]
imported_sources <- imported_sources[file.exists(imported_sources)]

# Asserting the derivation before concluding anything from it. A glob that
# matched nothing -- a moved directory, a run from somewhere other than the
# repository root -- would otherwise produce an empty page set, and every
# assertion below would pass by having nothing to check.
if (length(vignette_sources) == 0L || length(reference_sources) == 0L) {
  stop(
    call. = FALSE,
    "Derived no vignette or reference sources. Expected `vignettes/*.qmd` and ",
    "`man/*.Rd` relative to the working directory; run this from the ",
    "repository root."
  )
}

pages <- c(
  "docs/index.html",
  page_for(vignette_sources, "vignettes"),
  page_for(reference_sources, "man"),
  page_for(imported_sources)
)

# One page per source, plus the home page. The count is asserted because losing
# a whole set is silent otherwise: everything below iterates over `pages`, so a
# set that arrived empty is a set that passes.
expected <- 1L + length(vignette_sources) + length(reference_sources) +
  length(imported_sources)
if (length(pages) != expected) {
  stop(
    call. = FALSE,
    sprintf(
      "Derived %d page(s) from %d source(s); every source names one page.",
      length(pages),
      expected
    )
  )
}

# The search index holds every page's text, so a string that leaked into a page
# leaked into it too, and it is served next to them. It is not a page: it gets
# the two text scans below and neither the completeness check nor a marker.
scanned <- c(pages, "docs/search.json")

required <- c(scanned, "docs/.nojekyll")
missing <- required[!file.exists(required)]
if (length(missing) > 0L) {
  stop(call. = FALSE, "Missing site output: ", paste(missing, collapse = ", "))
}

# A page that reaches `</html>` is a page quarto finished writing. A render
# killed partway leaves a file that exists and is not empty, which the
# existence check alone accepts.
assert_complete <- function(text, page) {
  if (!grepl("</html>", text, fixed = TRUE)) {
    stop(call. = FALSE, "Truncated or unrendered page: ", page)
  }
}

# One scanner for both things a file must not contain, reporting the text it
# matched rather than the pattern that matched it -- for a regex, the pattern
# names the shape of a leak and the match names the leak.
assert_no_match <- function(text, patterns, path, what, fixed) {
  found <- vapply(
    patterns,
    function(pattern) {
      found <- if (fixed) {
        regmatches(text, regexpr(pattern, text, fixed = TRUE))
      } else {
        regmatches(text, regexpr(pattern, text, perl = TRUE))
      }
      if (length(found) == 0L) NA_character_ else found
    },
    character(1)
  )
  found <- unique(found[!is.na(found)])
  if (length(found) > 0L) {
    stop(
      call. = FALSE,
      what,
      " in ",
      path,
      ": ",
      paste(found, collapse = ", ")
    )
  }
}

# Absolute paths belonging to the machine that rendered the page. #99 baked
# `/Users/<user>/.duckdb` into two shipped vignettes and nothing here noticed,
# because the only path check named `Rtmp` and ran against one article.
#
# The patterns cover the shapes the three platforms write these paths in,
# including shapes the machine running this script does not use, since a page
# can be rendered on one platform and checked on another.
leak_patterns <- c(
  # R's per-session temporary directory, under any platform's temporary root.
  "Rtmp",
  # macOS's temporary root, which appears without `Rtmp` in some messages.
  "/var/folders/",
  # A home directory: the segment after `/Users` or `/home` is a user name.
  "/(Users|home)/[^/[:space:]\"'<>]+/",
  "[A-Za-z]:\\\\Users\\\\"
)

# This machine's expanded home directory, so a home in a shape the patterns
# above do not describe is still caught where it would leak. Guarded because a
# home of `/` -- or an unexpanded `~`, which is what `normalizePath()` returns
# when it cannot expand one -- would match most of the site.
home <- normalizePath("~", mustWork = FALSE)
if (nchar(home) > 1L && home != "~") {
  leak_patterns <- c(leak_patterns, sprintf("\\Q%s\\E", home))
}

# Strings no page may hold, whatever produced it.
forbidden <- c(
  # Installation documentation must not test for a package with a scan of the
  # installed library.
  "installed.packages",
  # A retired public name. Anywhere on the site it reads as an instruction to
  # call a function that no longer exists.
  "union_all_with_margins"
)

assert_markers <- function(text, markers, page) {
  absent <- markers[!vapply(
    markers,
    grepl,
    logical(1),
    x = text,
    fixed = TRUE
  )]
  if (length(absent) > 0L) {
    stop(
      call. = FALSE,
      "Missing rendered markers in ",
      page,
      ": ",
      paste(absent, collapse = ", ")
    )
  }
}

markers <- list(
  "docs/index.html" = c(
    'install.packages</span>(<span class="st">"pak"</span>)',
    "pak",
    "pkg_install",
    "SQL-style grouping sets",
    "A total is not the same thing as",
    "Compare each summary with the whole",
    "Choose the tool for the job",
    "grouping_identity.html",
    "completing_keys.html",
    "database_backends.html"
  ),
  "docs/vignettes/get_started.html" = c(
    "panel-tabset",
    "Optional Quarto tabsets with quartabs",
    "Union versus Cartesian product",
    "The manual version",
    "When marginplyr fits",
    "Intentional differences from",
    "grouping_bit",
    "expand_with_margins",
    "For several measures, use two ordered",
    "Compare a summary with the whole",
    "original pre-margin",
    "funion(funion(",
    "Total",
    # The guide's `must_error` chunks, by the diagnostic each one has to
    # produce. Every one is marked with the class form, so these markers also
    # stand for the assertions: prose survives a chunk that stopped running,
    # and a chunk that started failing for some other reason halts the render
    # rather than reaching this scan. None of them needs an optional Suggest,
    # so all of them hold wherever the page is built.
    "Error in `summarize_with_margins()`",
    "Error in `nest_with_margins()`",
    "does not support `cur_group_id()`",
    "Duplicate grouping sets were produced at positions",
    "Can't supply `.by` when `.data` is grouped",
    "Grouped input created with `.drop = FALSE` is not supported",
    "do not define one unambiguous parent",
    "Add an empty `grouping_set()` to the `grouping_sets()` specification",
    "`.duplicates` must be one of \"error\", \"drop\""
  ),
  "docs/vignettes/recipes.html" = c(
    "Put each subtotal with the rows it summarizes",
    "Compare each row with the right total",
    "Keep only one grouping set",
    "Name the grouping set on every row",
    "The same join against a lazy table",
    "Compute what the summary pass cannot express",
    "share_of_total",
    "expand_with_margins",
    # The guide's `must_error` chunks, by the diagnostic each one has to
    # produce. Prose alone would still be there if every chunk stopped running;
    # a rendered error is proof the call was made and was refused. Only the
    # three that need no database are listed, so the markers hold wherever the
    # page is built.
    "Error in `filter()`",
    "Error in `left_join()`",
    "Error in `purrr::map()`"
  ),
  "docs/vignettes/grouping_identity.html" = c(
    "Four related values",
    "Why a rollup skips Grouping identifier 2",
    "A cube includes every mask",
    "Occurrence does not promise row order",
    "Three kinds of apparent missingness",
    "Parent lookup is structural"
  ),
  "docs/vignettes/completing_keys.html" = c(
    "Why completion is a separate operation",
    "tidyr::complete",
    "fact_id",
    "Union an explicit scaffold",
    "copy_inline",
    "copy_to",
    "never copies, collects, uploads, downloads"
  ),
  "docs/vignettes/database_backends.html" = c(
    "One report, two execution locations",
    "GROUP BY GROUPING SETS",
    "Execute in DuckDB",
    "Portable fallback SQL",
    "UNION ALL",
    "show_query",
    "collect",
    "Parent shares use a staged lazy query",
    "share_of_total",
    "Completion stays in the lazy input pipeline",
    "Only DuckDB and SQLite are exercised as live SQL databases",
    "DuckDB covers native SQL end to end",
    "dozen-plus fallback dialects",
    "Arrow and dtplyr are tested for the lazy",
    # The one `must_error` chunk on this page that needs no optional Suggest:
    # nesting a SQL table, refused through the dbplyr simulator. The guide's
    # DuckDB, dtplyr, and Arrow refusals are behind availability guards, so
    # they render nothing where their package is absent and can carry no
    # marker — the same reason only three of `recipes.qmd`'s are listed above.
    "Error in `nest_with_margins()`",
    "which can be nested: data.frame, dtplyr_step"
  ),
  "docs/man/summarize_with_margins.html" = c(
    "summarise_with_margins",
    "Relationship to dplyr summaries",
    "Display labels and grouping identity",
    "Contextual shares",
    "Backend extension design",
    "Database backend coverage",
    "Microsoft SQL Server",
    "NA is already a factor level",
    "cannot select any column named in the complete grouping plan",
    "cur_group_id"
  ),
  "docs/man/inspect_grouping.html" = c(
    "Formats and ordinary tibble behavior",
    "Positron",
    "31 variable dimensions",
    "separate from a SQL execution plan"
  ),
  "docs/man/share_of_parent.html" = c(
    "Direct shares",
    "Eligible source summaries",
    "Column-wise shares",
    "Rejected forms and supported rewrites",
    "Lazy execution boundaries",
    # The page documents both denominators, and states that neither completes
    # keys. Losing either would leave one helper without a contract.
    "share_of_total",
    "synthesizes or completes keys",
    "revenue_quantile",
    "Missing fixed and included keys",
    "where(is.numeric)",
    ".unpack = TRUE",
    "runtime-only incompatibility",
    # What a general dbplyr backend does instead of reading the caller's data
    # (ADR 0020). This page is the canonical reference for both helpers'
    # source contract, so it is the page that has to carry the count, and the
    # count is the part a later edit is most likely to drift back to "one".
    "at most two queries"
  ),
  "docs/man/nest_with_margins.html" = c(
    "Relationship to tidyr and dplyr",
    "original, pre-margin values",
    "Nesting does not support",
    "No input column name is reserved"
  )
)

unknown <- setdiff(names(markers), pages)
if (length(unknown) > 0L) {
  stop(
    call. = FALSE,
    "`markers` names pages the site does not produce: ",
    paste(unknown, collapse = ", "),
    ". Move each entry to the page that replaced it."
  )
}

texts <- vapply(scanned, read_page, character(1))

for (path in scanned) {
  text <- texts[[path]]
  assert_no_match(text, leak_patterns, path, "Build-machine path", FALSE)
  assert_no_match(text, forbidden, path, "Forbidden string", TRUE)
}

for (page in pages) {
  text <- texts[[page]]
  assert_complete(text, page)
  if (!is.null(markers[[page]])) {
    assert_markers(text, markers[[page]], page)
  }
}

# The one assertion about a page's structure rather than its text: the first
# tabset must open on the Total tab, which no marker can express.
article <- texts[["docs/vignettes/get_started.html"]]
first_tabset <- strsplit(
  article,
  '<div class="panel-tabset">',
  fixed = TRUE
)[[1]][2]
if (
  is.na(first_tabset) ||
    !grepl(
      '<a class="nav-link active"[^>]*>Total</a>',
      first_tabset
    )
) {
  stop(call. = FALSE, "Total tab is not first")
}

# What the workflow file no longer shows, for the same reason
# `generate-backend-matrix.R` writes its matrix here: a reader of
# `altdoc.yaml` now sees one script invocation where the required pages used to
# be spelled out, so the run page is where the derived set becomes visible.
write_step_summary(c(
  "## Verified site output",
  "",
  sprintf(
    paste0(
      "%d page(s) derived from %d vignette source(s), %d help topic(s), and ",
      "%d imported file(s), plus the search index."
    ),
    length(pages),
    length(vignette_sources),
    length(reference_sources),
    length(imported_sources)
  ),
  "",
  sprintf(
    "- `%s`%s",
    scanned,
    ifelse(scanned %in% names(markers), " — with markers", "")
  )
))
