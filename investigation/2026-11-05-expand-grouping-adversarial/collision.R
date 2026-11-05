suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
nms <- c(".N", ".I", ".SD", ".BY", ".GRP", ".NGRP", ".EACHI", ".", ".data", ".env",
         "...", "..1", "..01", "..0", "a b", "NA", "TRUE", "if", "T", "F", "g")
for (nm in nms) for (dt in c(FALSE, TRUE)) for (collides in c(FALSE, TRUE)) {
  d <- data.frame(k = 1:2)
  d[[nm]] <- c("a", if (collides) "Total" else "b")
  x <- if (dt) dtplyr::lazy_dt(d) else d
  res <- tryCatch({
    out <- expand_with_margins(x, .grouping = rollup(dplyr::all_of(nm)),
                                .check_margin_label = TRUE, .id = "id")
    if (dt) out <- dplyr::collect(out)
    if (identical(out[[nm]], c(d[[nm]], "Total", "Total"))) "good" else "wrong values"
  }, error = function(e) {
    if (inherits(e, "marginplyr_error") && grepl("already", conditionMessage(e))) "collision"
    else paste0(class(e)[[1]], " ", gsub("\n", " ", conditionMessage(e)))
  })
  expected <- if (collides) "collision" else "good"
  if (res != expected) cat(sprintf("%s dt=%s collides=%s: %s\n", nm, dt, collides, res))
}
