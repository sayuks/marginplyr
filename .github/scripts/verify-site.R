required <- c(
  "docs/index.html",
  "docs/vignettes/get_started.html",
  "docs/vignettes/grouping_identity.html",
  "docs/vignettes/completing_keys.html",
  "docs/vignettes/database_backends.html",
  "docs/man/expand_with_margins.html",
  "docs/man/grouping_bit.html",
  "docs/man/grouping_set.html",
  "docs/man/inspect_grouping.html",
  "docs/man/nest_by_with_margins.html",
  "docs/man/nest_with_margins.html",
  "docs/man/retail_sales.html",
  "docs/man/share_of_parent.html",
  "docs/man/summarize_with_margins.html",
  "docs/.nojekyll"
)
missing <- required[!file.exists(required)]
if (length(missing) > 0L) {
  stop("Missing site output: ", paste(missing, collapse = ", "))
}

read_page <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

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
      "Missing rendered markers in ",
      page,
      ": ",
      paste(absent, collapse = ", ")
    )
  }
}

assert_page_markers <- function(path, markers, page) {
  assert_markers(read_page(path), markers, page)
}

home <- read_page("docs/index.html")
assert_markers(
  home,
  c(
    'install.packages</span>(<span class="st">"pak"</span>)',
    "pak",
    "pkg_install",
    "SQL-style grouping sets",
    "A total is not the same thing as",
    "Choose the tool for the job",
    "grouping_identity.html",
    "completing_keys.html",
    "database_backends.html"
  ),
  "README"
)

article <- read_page("docs/vignettes/get_started.html")
assert_markers(
  article,
  c(
    "panel-tabset",
    "Optional Quarto tabsets with quartabs",
    "Union versus Cartesian product",
    "The manual version",
    "When marginplyr fits",
    "Intentional differences from",
    "grouping_bit",
    "expand_with_margins",
    "For several measures, use two ordered",
    "original pre-margin",
    "funion(funion(",
    "Total"
  ),
  "Get started"
)

assert_page_markers(
  "docs/vignettes/grouping_identity.html",
  c(
    "Four related values",
    "Why a rollup skips Grouping identifier 2",
    "A cube includes every mask",
    "Occurrence does not promise row order",
    "Three kinds of apparent missingness",
    "Parent lookup is structural"
  ),
  "Grouping identity"
)

assert_page_markers(
  "docs/vignettes/completing_keys.html",
  c(
    "Why completion is a separate operation",
    "tidyr::complete",
    "fact_id",
    "Union an explicit scaffold",
    "copy_inline",
    "copy_to",
    "never copies, collects, uploads, downloads"
  ),
  "Complete absent keys before margins"
)

database_article <- read_page("docs/vignettes/database_backends.html")
assert_markers(
  database_article,
  c(
    "One report, two execution locations",
    "GROUP BY GROUPING SETS",
    "Execute in DuckDB",
    "Portable fallback SQL",
    "UNION ALL",
    "show_query",
    "collect",
    "Parent shares use a staged lazy query",
    "Completion stays in the lazy input pipeline",
    "Live native execution tested",
    "Native SQL generation tested",
    "Fallback SQL generation tested",
    "Microsoft SQL Server"
  ),
  "Database and lazy backends"
)
if (
  grepl("installed.packages", home, fixed = TRUE) ||
    grepl("installed.packages", article, fixed = TRUE) ||
    grepl("installed.packages", database_article, fixed = TRUE)
) {
  stop("Rendered installation documentation contains a package-presence check")
}

summary_reference <- read_page("docs/man/summarize_with_margins.html")
assert_markers(
  summary_reference,
  c(
    "summarise_with_margins",
    "Relationship to dplyr summaries",
    "Display labels and grouping identity",
    "Parent shares",
    "Empty inputs follow this Parent-share contract",
    "Backend extension design",
    "NA is already a factor level",
    "cannot select any column named in the complete grouping plan",
    "cur_group_id"
  ),
  "summarize_with_margins reference"
)

assert_page_markers(
  "docs/man/inspect_grouping.html",
  c(
    "Formats and ordinary tibble behavior",
    "Positron",
    "31 variable dimensions",
    "separate from a SQL execution plan"
  ),
  "inspect_grouping reference"
)

assert_page_markers(
  "docs/man/share_of_parent.html",
  c(
    "Direct Parent shares",
    "Eligible source summaries",
    "Column-wise Parent shares",
    "Rejected forms and supported rewrites",
    "Lazy execution boundaries",
    "revenue_quantile",
    "Missing fixed and included keys",
    "where(is.numeric)",
    ".unpack = TRUE",
    "runtime-only incompatibilities"
  ),
  "share_of_parent reference"
)

assert_page_markers(
  "docs/man/nest_with_margins.html",
  c(
    "Relationship to tidyr and dplyr",
    "original, pre-margin values",
    "Nesting does not support",
    "No input column name is reserved"
  ),
  "nest_with_margins reference"
)

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
  stop("Total tab is not first")
}
if (grepl("Rtmp", article, fixed = TRUE)) {
  stop("Rendered site contains a local temporary path")
}
if (grepl("union_all_with_margins", article, fixed = TRUE)) {
  stop("Rendered site contains the retired public function name")
}
