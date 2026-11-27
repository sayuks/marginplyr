# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A later across() selection reading a package constant's lazy-load promise names no output in a fresh session (#1809).
# Candidate 3: a later across() whose selection reads an attached package's
# lazy-load promise (here datasets' `state.abb`) names no output until
# something in the session has forced that promise, so a share of its output is
# refused on the first call of a session and accepted afterwards.
# Each case runs in its own fresh process: the script re-invokes itself.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))

mode <- commandArgs(TRUE)
if (length(mode) == 0L) {
  self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
  cat("Expected: every case accepted with s = 0.5 0.5 1, which is AL_t over its Grand total\n",
      "(AL = 1, 2, 3 by row: group a 1 + 2 = 3, group b 3, total 6), as the forced case is.\n",
      "#1793's decision: 'Read a lazy-load promise as its value: when the binding is lazy and\n",
      "it lives in a namespace, a package: environment or an imports environment, the planner\n",
      "may force it.'\n\n", sep = "")
  for (m in c("df-later", "df-later-forced", "df-first", "datatable-later", "duckdb-later",
              "native")) {
    out <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(self), m),
                   stdout = TRUE, stderr = TRUE)
    out <- grep("^OMP", out, value = TRUE, invert = TRUE)
    cat(sprintf("[%s]\n", m)); cat(paste0("   ", out), sep = "\n"); cat("\n")
  }
  quit(save = "no", status = 0)
}

abb <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
         "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
         "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT",
         "VA","WA","WV","WI","WY")
d <- data.frame(g = c("a", "a", "b"), matrix(seq_len(3 * 50), nrow = 3))
names(d)[-1] <- abb
lazy <- function() rlang::env_binding_are_lazy(as.environment("package:datasets"), "state.abb")
cat("state.abb is an unforced lazy-load promise before the call:", lazy(), "\n")
result <- function(expr) tryCatch({
  r <- collect(expr)
  paste("accepted, s =", paste(round(r$s, 4), collapse = " "))
}, error = function(e) paste0("refused <", class(e)[[1L]], ">: ", gsub("\n", " | ", conditionMessage(e))))

if (mode == "native") {
  r <- summarise(d, n = n(), across(all_of(state.abb), sum, .names = "{.col}_t"), .by = g)
  cat("native dplyr::summarise(): columns", paste(names(r)[1:4], collapse = ", "), "...; AL_t =",
      paste(r$AL_t, collapse = ", "), "\n")
  quit(save = "no", status = 0)
}
if (mode == "df-later-forced") invisible(state.abb)
input <- switch(mode,
  "datatable-later" = data.table::as.data.table(d),
  "duckdb-later" = {
    con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
    DBI::dbWriteTable(con, "d", d)
    tbl(con, "d")
  },
  d
)
if (mode == "df-first") {
  cat(result(summarize_with_margins(
    input, across(all_of(state.abb), sum, .names = "{.col}_t"), n = n(),
    s = share_of_total(AL_t), .grouping = rollup(g)
  )), "\n")
} else {
  cat(result(summarize_with_margins(
    input, n = n(), across(all_of(state.abb), sum, .names = "{.col}_t"),
    s = share_of_total(AL_t), .grouping = rollup(g)
  )), "\n")
}
cat("state.abb still unforced after the call:", lazy(), "\n")
if (mode == "df-later") {
  invisible(state.abb)
  cat("after invisible(state.abb), the same call: ")
  cat(result(summarize_with_margins(
    input, n = n(), across(all_of(state.abb), sum, .names = "{.col}_t"),
    s = share_of_total(AL_t), .grouping = rollup(g)
  )), "\n")
}
