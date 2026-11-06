Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet=TRUE)
d <- tibble::tibble(a="x", value=1L)
for (n in c(50L,100L,200L,400L,800L,1600L)) {
  s <- rlang::inject(rollup(!!!rep(list(rlang::quo(a)),n)))
  time <- system.time({
    p <- inspect_grouping(d,.grouping=s,.duplicates="drop",.format="list")
  })
  stopifnot(identical(p$included,list("a",character())))
  cat(n,"units",time[["elapsed"]],"seconds",nrow(p),"retained sets\n")
}
