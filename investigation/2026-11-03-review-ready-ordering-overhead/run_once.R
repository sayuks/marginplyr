# Fresh-process entry: source definitions, perform one pipeline, record its result.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 4L)
source(file.path(args[[1L]], "cost_pipeline.R"))
result <- cost_pipeline(args[[2L]], file.path(args[[3L]], "r-output"))
saveRDS(result, args[[4L]], compress = FALSE)
