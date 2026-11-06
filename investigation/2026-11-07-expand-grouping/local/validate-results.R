args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
labels <- read.csv(file.path(args[[1L]], "named-labels.csv"), stringsAsFactors = FALSE)
packed <- read.csv(file.path(args[[1L]], "packed-inner-names.csv"), stringsAsFactors = FALSE)
stopifnot(nrow(labels) == 12672L,
          sum(labels$status == "match") == 12384L,
          sum(labels$status == "required_refusal") == 288L,
          all(labels$status %in% c("match", "required_refusal")),
          nrow(packed) == 1296L,
          all(packed$status == "match"))
refusals <- labels$detail[labels$status == "required_refusal"]
stopifnot(length(unique(refusals)) == 1L,
          all(grepl("NA_character_.*already a factor level", refusals)),
          all(grepl("typed-missing Margin label while preserving the NA level", refusals, fixed = TRUE)))
cat("Validated 12,672 named-label cases and 1,296 packed-inner-name cases.\n")
cat("All 288 refusals had the declared factor NA-level diagnostic and NULL remedy.\n")
cat(unique(refusals), "\n")
