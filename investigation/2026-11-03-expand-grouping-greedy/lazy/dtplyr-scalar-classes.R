Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
library(dplyr)
classes <- list(dist = dist(matrix(1:3)), logLik = logLik(lm(c(1, 3, 2, 5) ~ c(1, 2, 3, 4))))
for (type in names(classes)) {
 cat("TYPE", type, "\n")
 g <- classes[[type]]
 src <- tibble(g = g, rid = seq_len(vctrs::vec_size(g)))
 print(src)
 dt <- data.table::as.data.table(src)
 print(lapply(dt, class))
 lazy <- dtplyr::lazy_dt(dt, immutable = TRUE)
 native <- tryCatch(collect(lazy), error = identity)
 print(lapply(native, class))
 control <- tryCatch(union_all(lazy, mutate(lazy, g = vctrs::vec_rep(vctrs::vec_init(g, 1L), length(g)))) |> collect(), error = identity)
 print(control)
 actual <- tryCatch(expand_with_margins(lazy, .grouping = rollup(g), .margin_label = NULL, .id = "set") |> collect(), error = identity)
 print(actual)
}
