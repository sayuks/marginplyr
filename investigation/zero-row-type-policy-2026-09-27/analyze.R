root <- commandArgs(trailingOnly = TRUE)[[1L]]
base <- readRDS(file.path(root, "base.rds"))
for (variant in c("relaxed", "extended")) {
  alt <- readRDS(file.path(root, paste0(variant, ".rds")))
  differences <- list()
  for (key in names(base)) {
    x <- base[[key]]
    y <- alt[[key]]
    if (!identical(x, y)) {
      type_x <- vapply(x, typeof, character(1))
      type_y <- vapply(y, typeof, character(1))
      changed <- names(type_x)[type_x != type_y]
      differences[[key]] <- data.frame(
        key = key, rows = nrow(x),
        names_equal = identical(names(x), names(y)),
        rows_equal = nrow(x) == nrow(y),
        data_equal = identical(as.data.frame(x), as.data.frame(y)),
        class_equal = identical(class(x), class(y)),
        changed = paste(changed, collapse = ","),
        before = paste(type_x[changed], collapse = ","),
        after = paste(type_y[changed], collapse = ",")
      )
    }
  }
  differences <- do.call(rbind, differences)
  write.csv(differences, file.path(root, paste0(variant, "-differences.csv")),
    row.names = FALSE)
  cat(variant, "\n")
  cat("nonempty observations", sum(vapply(base, nrow, integer(1)) > 0L), "\n")
  print(table(differences$rows > 0L))
  print(unique(differences[c("rows", "changed", "before", "after")]))
  print(subset(differences, rows > 0L))
  cat("public names and row counts stable:",
    all(differences$names_equal & differences$rows_equal), "\n")
  cat("nonempty data/type differences:",
    sum(differences$rows > 0L & !differences$data_equal), "\n")
  cat("nonempty class differences:",
    sum(differences$rows > 0L & !differences$class_equal), "\n")
}
