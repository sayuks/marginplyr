expect_share_dependencies <- function(
  data, reads, reverse_message = "depends on earlier summary alias") {
  env <- parent.frame()
  for (kind in c("parent", "total")) {
    for (read in reads) {
      read <- rlang::as_quosure(read, env)
      share <- rlang::call2(paste0("share_of_", kind), quote(source))
      expect_error(
        summarize_with_margins(
          data, source = sum(.data$x), prior = !!share, observed = !!read,
          .grouping = rollup("g")
        ),
        "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
      )
      share <- rlang::call2(paste0("share_of_", kind), quote(observed))
      expect_error(
        summarize_with_margins(
          data, prior = sum(.data$x), observed = !!read, ratio = !!share,
          .grouping = rollup("g")
        ),
        reverse_message, class = "marginplyr_error"
      )
    }
  }
}
