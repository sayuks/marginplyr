args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
recursive <- read.csv(file.path(args[[1L]], "recursive-keys.csv"))
extension <- read.csv(file.path(args[[1L]], "extension-keys.csv"))
stopifnot(nrow(recursive) == 2880L, all(recursive$status == "match"), all(recursive$immutable))
stopifnot(nrow(extension) == 2916L, all(extension$immutable))
counts <- table(extension$status)
stopifnot(counts[["match"]] == 1953L, counts[["required_refusal"]] == 207L,
  counts[["ordinary_error"]] == 270L, counts[["fixture_exclusion"]] == 486L,
  identical(sort(names(counts)), sort(c("match", "required_refusal", "ordinary_error", "fixture_exclusion"))))
refusals <- extension[extension$status == "required_refusal", ]
stopifnot(all(grepl("cannot label packed|cannot label matrix or array", refusals$detail)))
cat("Validated 2,880 recursive matches and all 2,916 extension classifications.\n")
