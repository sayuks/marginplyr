Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_SOURCE", unset = getwd()), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(1109)
outdir <- tempfile("marginplyr-numeric-oracle-")
dir.create(outdir, showWarnings = FALSE)
# Input generation does not inspect a Grouping plan or either share helper.
inputs <- lapply(0:35, function(i) {
  n <- c(0L, 1L, 2L, 5L, 8L, 12L)[i %% 6L + 1L]
  x <- tibble(p = sample(c("P", "Q", NA_character_), n, TRUE),
              a = sample(c("a", "b", NA_character_), n, TRUE),
              b = sample(c("u", "v", NA_character_), n, TRUE),
              v = sample(c(-2, 0, 1, 3, NA_real_, NaN), n, TRUE))
  if (i %% 3L == 1L) x$a <- factor(x$a, levels = c("b", "a", "unused"))
  if (i %% 3L == 2L) x$b <- as.Date(match(x$b, c("u", "v")), origin = "2020-01-01")
  x
})
families <- list(
  list(spec = rollup(a, b), sets = list(c("a", "b"), "a", character())),
  list(spec = cube(a, b), sets = list(c("a", "b"), "a", "b", character())),
  list(spec = grouping_sets(grouping_set(b), grouping_set(a), grouping_set()),
       sets = list("b", "a", character())),
  list(spec = grouping_spec(rollup(a), rollup(b)),
       sets = list(c("a", "b"), "a", "b", character())),
  list(spec = grouping_sets(grouping_set(), grouping_set(a), grouping_set()),
       sets = list(character(), "a", character()), dup = "keep"),
  list(spec = rollup(grouping_set(a,b)), sets = list(c("a","b"), character()))
)
canonical <- function(x) {
  x <- as.data.frame(x)
  for (nm in names(x)) {
    if (is.factor(x[[nm]])) x[[nm]] <- as.character(x[[nm]])
  }
  if (nrow(x)) x <- x[do.call(order, c(unname(x[intersect(c("id","p","a","b"),names(x))]), na.last = TRUE)), , drop=FALSE]
  rownames(x) <- NULL
  x
}
# Expectation enumerates grains explicitly and derives each denominator from
# the raw input with ordinary dplyr, never from marginplyr internals.
reference <- function(data, sets, fixed) {
  dimensions <- unique(unlist(sets))
  root <- data |> group_by(across(all_of(fixed))) |>
    summarise(v = sum(v, na.rm = TRUE), n = n(), .groups="drop")
  branches <- lapply(seq_along(sets), function(i) {
    keys <- c(fixed, sets[[i]])
    branch <- data |> group_by(across(all_of(keys))) |>
      summarise(v = sum(v, na.rm=TRUE), n=n(), .groups="drop")
    if (!length(sets[[i]]) && !length(fixed)) branch <- root
    absent <- setdiff(dimensions, sets[[i]])
    for (nm in absent) branch[[nm]] <- vctrs::vec_init(data[[nm]], nrow(branch))
    branch$id <- as.integer(i)
    if (length(fixed)) {
      den <- root |> rename(den_v=v, den_n=n)
      branch <- left_join(branch, den, by=fixed, na_matches="na")
    } else {
      branch$den_v <- root$v
      branch$den_n <- root$n
    }
    branch$sv <- if (!length(sets[[i]])) 1 else ifelse(is.na(branch$v) | is.na(branch$den_v) | branch$den_v==0, NA_real_, branch$v/branch$den_v)
    branch$sn <- if (!length(sets[[i]])) 1 else ifelse(is.na(branch$n) | is.na(branch$den_n) | branch$den_n==0, NA_real_, branch$n/branch$den_n)
    branch |> select(all_of(c(fixed, dimensions)), id,v,n,sv,sn)
  })
  bind_rows(branches)
}
failures <- list(); checks <- 0L
for (i in seq_along(inputs)) for (j in seq_along(families)) for (fixed in list(character(), "p")) {
  data <- inputs[[i]]; fam <- families[[j]]
  expected <- canonical(reference(data,fam$sets,fixed))
  for (kind in c("local", "dtplyr")) for (sort in c("none","first","last")) {
    input <- if (kind=="local") data else dtplyr::lazy_dt(data)
    actual <- tryCatch(summarize_with_margins(input,
      v = sum(v,na.rm=TRUE), n = n(), sv = share_of_total(v), sn=share_of_total(n),
      .by=all_of(fixed), .grouping=!!fam$spec, .id="id", .margin_label=NULL,
      .duplicates=if (is.null(fam$dup)) "error" else fam$dup, .sort=sort) |> collect() |> canonical(), error=identity)
    checks <- checks+1L
    equal <- !inherits(actual,"error") && isTRUE(all.equal(actual,expected,check.attributes=TRUE))
    if (!equal) {
      key <- paste(i,j,paste(fixed,collapse=""),kind,sort,sep="/")
      failures[[key]] <- list(data=data,sets=fam$sets,actual=actual,expected=expected,
                             diff=if(inherits(actual,"error")) conditionMessage(actual) else all.equal(actual,expected))
    }
  }
}
saveRDS(failures, file.path(outdir,"oracle-failures.rds"))
cat("comparisons",checks,"failures",length(failures),"\n")
if(length(failures)) print(lapply(head(failures,12), `[[`, "diff"))
capture.output(sessionInfo(), file=file.path(outdir,"environment.txt"))
stopifnot(length(failures) == 0L)
