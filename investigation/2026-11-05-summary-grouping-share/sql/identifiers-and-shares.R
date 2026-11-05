suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(".", quiet = TRUE)
run <- function(con, backend) {
  on.exit(DBI::dbDisconnect(con))
  input <- tibble(p = c("A", "A", "A", NA, NA, "B"), g = c("z", NA, "a", "z", "a", "a"),
                  h = c("n", "n", NA, "m", "n", "n"), v = c(1, -1, 2, 5, 1, 0))
  call <- function(src, p_name, g_name, h_name, v_name, id, mode, label, sort) {
    summarize_with_margins(
      src, value = sum(.data[[v_name]], na.rm = TRUE),
      parent = share_of_parent(value), total = share_of_total(value),
      gb = grouping_bit(!!rlang::sym(g_name)), hb = grouping_bit(!!rlang::sym(h_name)),
      .by = all_of(p_name), .grouping = rollup(all_of(g_name), all_of(h_name)),
      .id = id, .duplicates = mode, .margin_label = label, .sort = sort,
      .check_share_source = FALSE
    )
  }
  canonical <- function(x, sort) {
    x <- as.data.frame(x)
    names(x) <- seq_along(x)
    x[] <- lapply(x, function(v) if (is.numeric(v)) as.double(v) else v)
    if (sort == "none") x <- x[do.call(order, x[c(1:4, 8:9)]), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  cases <- list(c("p","g","h","v","set"), c("TRUE","FALSE",".env","v",".data"),
                c(".env",".data","TRUE","FALSE","Group"), c("space p","select","a`b","value z","Case"),
                c("p","g","h","v","TRUE"), c("p","g","h","v",".env"))
  ok <- 0L
  for (i in seq_along(cases)) {
    nms <- cases[[i]]
    renamed <- stats::setNames(input, nms[seq_len(4L)])
    source <- copy_to(con, renamed, paste0("data_", i), temporary = TRUE, overwrite = TRUE)
    for (mode in c("error", "keep")) for (label in list(NULL, "Total")) {
      for (sort in c("none", "first", "last")) {
        label_text <- if (is.null(label)) "NULL" else label
        want <- call(renamed, nms[1], nms[2], nms[3], nms[4], nms[5], mode, label, sort)
        ans <- tryCatch(collect(call(source, nms[1], nms[2], nms[3], nms[4], nms[5], mode, label, sort)),
                        error = identity)
        if (inherits(ans, "error")) {
          cat(backend, i, mode, label_text, sort, "ERROR", gsub("\n", " ", conditionMessage(ans)), "\n")
        } else if (!isTRUE(all.equal(canonical(ans, sort), canonical(want, sort)))) {
          cat(backend, i, mode, label_text, sort, "DIFF", "\n")
          print(list(actual = ans, expected = want))
        } else {
          ok <- ok + 1L
        }
      }
    }
  }
  cat(backend, ok, "matched\n")
  stopifnot(ok == 72L)
}
run(DBI::dbConnect(RSQLite::SQLite()), "SQLite")
run(DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)), "DuckDB")
socket <- Sys.getenv("MP_HUNT_SOCKET")
if (nzchar(socket)) {
  con <- DBI::dbConnect(RPostgres::Postgres(), host = socket,
                        port = 55689L, dbname = "postgres")
  run(con, "PostgreSQL")
}
