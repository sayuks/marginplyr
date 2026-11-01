# Actual generation/site rendering with the repository runner's private roots.
# This exercises real tools and installed source, not the process stand-in.
args <- commandArgs(trailingOnly = TRUE)
candidate <- if (length(args)) normalizePath(args[[1L]], mustWork = TRUE) else getwd()
entry <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
fixture_root <- dirname(dirname(normalizePath(entry, mustWork = TRUE)))
source(file.path(candidate, "tools/review-ready-core.R"))
source(file.path(candidate, "tools/review-ready-registry.R"))
source(file.path(candidate, "tools/review-ready-evidence.R"))
source(file.path(fixture_root, "tools/review-ready-check-lib.R"))

run_renderer_contract <- function() {
  fixture <- tempfile("runner-renderer-", tmpdir = Sys.getenv("TMPDIR", tempdir()))
  directory <- rr_mkdir(file.path(fixture, ".local/state/marginplyr/review-ready/requests", rr_id()))
  on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
  env <- rr_render_env(directory)
  env[["MARGINPLYR_REVIEW_STAGE_REPORT"]] <- ""
  env[["MARGINPLYR_REVIEW_BASE"]] <- ""
  root <- candidate
  sha <- rr_git(root, "rev-parse", "HEAD")
  checkout <- file.path(rr_mkdir(file.path(directory, "work")), "source")
  rr_run("git", c("clone", "--quiet", "--shared", "--no-checkout", root, checkout))
  rr_git(checkout, "checkout", "--quiet", "--detach", sha)
  for (path in c("tools/review-ready-generated.R", "tools/verify-site-artifacts.R",
      ".github/scripts/verify-site.R", ".github/scripts/ci-helpers.R")) {
    if (!file.copy(file.path(fixture_root, path), file.path(checkout, path), overwrite = TRUE)) rr_stop("Accepted renderer helper source unavailable.")
  }
  renderer <- rr_renderer()
  paths <- review_ready_render_paths()
  deno <- rr_run(unname(paths[["Deno"]]), c("info", "--json"), root, env)
  info <- jsonlite::fromJSON(deno$stdout)
  if (!startsWith(info$denoDir, directory)) rr_stop("Deno resolved a shared cache: ", info$denoDir)
  probe <- rr_run(file.path(R.home("bin"), "Rscript"), c("-e",
    'cat(jsonlite::toJSON(list(tmp=tempdir(), cache=tools::R_user_dir("marginplyr","cache"), libraries=as.list(.libPaths())),auto_unbox=TRUE))'), root, env)
  observed <- jsonlite::fromJSON(probe$stdout)
  if (!startsWith(observed$tmp, directory) || !startsWith(observed$cache, directory) ||
      !identical(unname(observed$libraries), unname(.libPaths()))) rr_stop("R renderer storage/library isolation failed.")
  rr_say("Real renderer contract: commit ", sha, "; Quarto ", renderer$version,
    "; JS SHA-256 ", rr_file_identity(renderer$js)$digest)
  for (path in c("tools/review-ready-generated.R", "tools/verify-site-artifacts.R")) {
    result <- processx::run(file.path(R.home("bin"), "Rscript"), path, wd = checkout, env = env,
      echo = TRUE, error_on_status = FALSE, cleanup_tree = TRUE)
    if (result$status != 0L) rr_stop("Private renderer contract failed: ", path)
  }
  expected <- if (Sys.info()[["sysname"]] == "Darwin") file.path(env[["HOME"]], "Library/Caches/quarto") else
    file.path(env[["XDG_CACHE_HOME"]], "quarto")
  if (!dir.exists(expected)) rr_stop("Quarto did not use the registered private app/cache directory.")
  rr_say("Real README/help/figures and site renderer isolation passed; all owned writers collected.")
}
run_renderer_contract()
