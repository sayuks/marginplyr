required <- c(
  "docs/index.html",
  "docs/vignettes/get_started.html",
  "docs/man/expand_with_margins.html",
  "docs/man/grouping_bit.html",
  "docs/man/grouping_set.html",
  "docs/man/nest_by_with_margins.html",
  "docs/man/nest_with_margins.html",
  "docs/man/retail_sales.html",
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

article <- read_page("docs/vignettes/get_started.html")
assert_markers(
  article,
  c(
    "panel-tabset",
    "Optional Quarto tabsets with quartabs",
    "Union versus Cartesian product",
    "Intentional differences from",
    "GROUP BY GROUPING SETS",
    "grouping_bit",
    "expand_with_margins",
    "Microsoft SQL Server",
    "original pre-margin",
    "setorder(funion",
    "Total"
  ),
  "Get started"
)

summary_reference <- read_page("docs/man/summarize_with_margins.html")
assert_markers(
  summary_reference,
  c(
    "summarise_with_margins",
    "Relationship to dplyr summaries",
    "Display labels and grouping identity",
    "Backend extension design",
    "cannot select any column named in the complete grouping plan",
    "cur_group_id"
  ),
  "summarize_with_margins reference"
)

nest_reference <- read_page("docs/man/nest_with_margins.html")
assert_markers(
  nest_reference,
  c(
    "Relationship to tidyr and dplyr",
    "original, pre-margin values",
    "indistinguishable outer keys",
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
