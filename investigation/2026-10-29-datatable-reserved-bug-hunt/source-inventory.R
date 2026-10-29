# Recompute the installed-version inventory without evaluating caller data.
out <- "investigation/2026-10-29-datatable-reserved-bug-hunt"
rd <- tools::Rd_db("data.table")[["special-symbols.Rd"]]
aliases <- vapply(Filter(function(node) identical(attr(node, "Rd_tag"), "\\alias"), rd), function(node) paste(unlist(node), collapse = ""), character(1))
official <- aliases[startsWith(aliases, ".")]
stopifnot(setequal(official, c(".SD", ".BY", ".N", ".I", ".GRP", ".NGRP", ".EACHI", ".NATURAL", ".ROW")))
ns <- asNamespace("data.table")
functions <- Filter(function(name) is.function(get(name, ns)), ls(ns, all.names = TRUE))
pattern <- "reserved|special meaning|may not be called|lockBinding|SDenv|assign\\("
hits <- lapply(functions, function(name) {
  body <- deparse(get(name, ns), width.cutoff = 120L)
  selected <- grep(pattern, body, value = TRUE)
  if (!length(selected)) return(NULL)
  data.frame(function_name = name, expression = trimws(selected))
})
hits <- do.call(rbind, hits)
write.csv(hits, file.path(out, "namespace-scan.csv"), row.names = FALSE)
cat("Official aliases:", paste(official, collapse = ", "), "\n")
cat("Namespace functions scanned:", length(functions), "\n")
cat("Matching functions:", paste(unique(hits$function_name), collapse = ", "), "\n")
cat("Explicit reservation/locking matches:\n")
print(hits[grepl("reserved|lockBinding", hits$expression), ], row.names = FALSE)
cat("Versions:\n")
for (pkg in c("data.table", "dtplyr", "dplyr", "rlang", "tibble", "vctrs", "pkgload")) cat(pkg, as.character(packageVersion(pkg)), "\n")
cat(R.version.string, "\n")
print(sessionInfo())
