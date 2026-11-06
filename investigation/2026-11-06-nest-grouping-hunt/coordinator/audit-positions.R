args <- commandArgs(TRUE)
p <- if (length(args)) args[[1]] else "."
r <- read.csv(file.path(p, "mixed-algebra.csv"))
cases <- readRDS(file.path(p, "mixed-trees.rds"))
valid <- function(groups, fam) {
  all(vapply(groups, function(ix) length(ix) > 1L && !anyNA(ix) && all(ix %in% seq_along(fam)) && length(unique(fam[ix])) == 1L, logical(1)))
}
stopifnot(valid(list(c(1L, 2L)), list("a", "a", "b")),
          !valid(list(c(1L, 3L)), list("a", "a", "b")),
          !valid(list(c(1L, 4L)), list("a", "a", "b")))
checks <- lapply(which(r$policy == "error" & r$observation != "plan"), function(i) {
  line <- strsplit(r$observation[[i]], "\n", fixed=TRUE)[[1]][[2]]
  line <- gsub("[^0-9,; ]", "", line)
  groups <- lapply(strsplit(line, ";", fixed=TRUE)[[1]], function(s) as.integer(trimws(strsplit(s, ",", fixed=TRUE)[[1]])))
  fam <- cases[[r$case[[i]]]]$family
  dims <- unique(unlist(fam, use.names=FALSE))
  fam <- lapply(fam, function(s) dims[dims %in% s])
  data.frame(case=r$case[[i]], parsed=!anyNA(unlist(groups)), valid=valid(groups, fam), positions=line)
})
ans <- do.call(rbind, checks)
write.csv(ans, file.path(p,"position-audit.csv"), row.names=FALSE)
cat("Diagnostic reports parsed:", nrow(ans), "Valid:", sum(ans$valid), "Synthetic false-positive controls: 2 rejected\n")
print(ans[!ans$valid,], row.names=FALSE)
stopifnot(all(ans$parsed), all(ans$valid))
