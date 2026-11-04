# Dated experiment. Run from the worktree root with an external output directory.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
output <- normalizePath(args[[1L]], mustWork = TRUE)
root <- normalizePath(".")
sha <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
archive <- file.path(output, "source.tar")
stopifnot(system2("git", c("archive", "--format=tar",
                         paste0("--output=", shQuote(archive)), sha)) == 0L)
source_root <- file.path(output, "source")
dir.create(source_root)
untar(archive, exdir = source_root)
worker <- file.path(output, "worker.R")
writeLines(c(
  "args <- commandArgs(trailingOnly = TRUE)",
  "setwd(args[[1L]])",
  "load_time <- system.time(pkgload::load_all('.', quiet = TRUE))",
  "lint_time <- system.time(result <- lintr::lint_package(cache = FALSE))",
  "stopifnot(length(result) == 0L)",
  "saveRDS(list(load = load_time, lint = lint_time, findings = length(result)), args[[2L]])"
), worker)
schedule <- c("baseline", "reduced", "reduced",
              "baseline", "baseline", "reduced")
rows <- vector("list", length(schedule))
for (i in seq_along(schedule)) {
  mode <- schedule[[i]]
  configuration <- file.path(source_root, ".lintr")
  unlink(configuration)
  if (mode != "baseline") {
    stopifnot(file.copy(file.path(root, "investigation",
                                 "lintr-jarl-overlap-2026-11-04",
                                 paste0(mode, ".lintr")), configuration))
  }
  result_path <- file.path(output, paste0("result-", i, ".rds"))
  log_path <- file.path(output, paste0("run-", i, ".log"))
  command_time <- system.time({
    status <- system2(
      file.path(R.home("bin"), "Rscript"),
      shQuote(c(worker, source_root, result_path)),
      stdout = log_path, stderr = log_path,
      env = "LINTR_ERROR_ON_LINT=true"
    )
  })
  if (status != 0L) {
    cat(readLines(log_path), sep = "\n")
    stop("Benchmark child failed.")
  }
  result <- readRDS(result_path)
  rows[[i]] <- data.frame(
    iteration = i, variant = mode, sha = sha,
    command_elapsed = command_time[["elapsed"]],
    load_elapsed = result$load[["elapsed"]],
    lint_elapsed = result$lint[["elapsed"]],
    lint_cpu = sum(result$lint[c("user.self", "sys.self")]),
    findings = result$findings
  )
  print(rows[[i]])
  write.csv(do.call(rbind, rows[seq_len(i)]),
            file.path(output, "timings.csv"), row.names = FALSE)
}
writeLines(c(R.version.string,
             paste("lintr", packageVersion("lintr")),
             paste("pkgload", packageVersion("pkgload")),
             system2("jarl", "--version", stdout = TRUE),
             paste("SHA", sha)), file.path(output, "environment.txt"))
