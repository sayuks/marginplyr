# Checks a pinned committed source tree through the repository command.

# Resolves the account home independently of caller HOME and cache overrides.
review_ready_account_home <- function() {
  path.expand(paste0("~", Sys.info()[["user"]]))
}

# Takes process-library identity handles while the owned parent is still alive.
# Inspection errors remain errors; only a process that has exited has no children.
review_ready_children <- function(process) {
  tryCatch(ps::ps_children(ps::ps_handle(process$get_pid()), recursive = TRUE),
    no_such_process = function(cnd) list())
}

# Stops owned descendants and confirms exit before capacity may be released.
review_ready_stop_process <- function(process, children = list()) {
  suspendInterrupts({
    reported <- NULL
    repeat {
      stopped <- tryCatch({
        current <- review_ready_children(process)
        for (child in current) children[[as.character(ps::ps_pid(child))]] <- child
        if (process$is_alive()) process$kill_tree()
        process$wait()
        if (!length(children)) TRUE else {
          ps::ps_kill(children)
          all(ps::ps_wait(children, timeout = 1000L))
        }
      }, error = function(cnd) cnd)
      if (isTRUE(stopped)) break
      if (inherits(stopped, "error") && !identical(reported, conditionMessage(stopped))) {
        reported <- conditionMessage(stopped)
        cat("Cleanup cannot yet confirm owned-process exit; capacity remains held: ",
          reported, "\n", sep = "")
      }
      Sys.sleep(0.2)
    }
  })
  invisible(NULL)
}

# Streams one external command and retains its process handle through cleanup.
review_ready_run_system <- function(command, args = character(), directory,
                                    env = character(), capture = FALSE) {
  process <- processx::process$new(command, args, wd = directory,
    env = { current <- Sys.getenv(); current[names(env)] <- env; current }, stdout = "|", stderr = "|",
    cleanup_tree = TRUE, supervise = TRUE)
  children <- list()
  on.exit(review_ready_stop_process(process, children), add = TRUE)
  output <- character()
  errors <- character()
  repeat {
    current <- review_ready_children(process)
    for (child in current) children[[as.character(ps::ps_pid(child))]] <- child
    process$poll_io(200L)
    lines <- process$read_output_lines()
    diagnostic <- process$read_error_lines()
    if (capture) errors <- c(errors, diagnostic) else if (length(diagnostic)) cat(paste0(diagnostic, "\n"), sep = "")
    if (capture) output <- c(output, lines) else if (length(lines)) cat(paste0(lines, "\n"), sep = "")
    if (!process$is_alive()) break
  }
  process$wait()
  lines <- process$read_all_output_lines()
  if (capture) output <- c(output, lines) else if (length(lines)) cat(paste0(lines, "\n"), sep = "")
  diagnostic <- process$read_all_error_lines()
  if (capture) errors <- c(errors, diagnostic) else if (length(diagnostic)) cat(paste0(diagnostic, "\n"), sep = "")
  if (process$get_exit_status() != 0L) output <- c(output, errors)
  list(status = process$get_exit_status(), output = output)
}

# Both scopes require an explicit human-readable AI judgment.
parse_review_ready_args <- function(args) {
  if (length(args) != 4L || !identical(args[c(1L, 3L)], c("--scope", "--reason")) ||
      !args[[2L]] %in% c("source", "full") || !nzchar(trimws(args[[4L]]))) {
    stop("Usage: Rscript tools/review-ready.R --scope source|full --reason TEXT", call. = FALSE)
  }
  list(scope = args[[2L]], reason = args[[4L]])
}

# Returns the stdout from a successful Git query.
review_ready_git_output <- function(repository_root, args) {
  result <- review_ready_run_system(
    "git",
    args,
    directory = repository_root,
    capture = TRUE
  )
  if (result$status != 0L) {
    stop(paste(result$output, collapse = "\n"), call. = FALSE)
  }
  result$output
}

# Pins a clean committed revision in the repository containing the command.
review_ready_identity <- function(
  repository_root,
  git_output = review_ready_git_output
) {
  expected_root <- normalizePath(repository_root, winslash = "/", mustWork = TRUE)
  actual_root <- normalizePath(
    git_output(repository_root, c("rev-parse", "--show-toplevel"))[[1L]],
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(actual_root, expected_root)) {
    stop("Run the review-ready check from its own repository.", call. = FALSE)
  }
  status <- git_output(
    repository_root,
    c("status", "--porcelain=v1", "--untracked-files=all")
  )
  if (length(status) > 0L && any(nzchar(status))) {
    stop(
      paste0(
        "The review-ready check requires a clean committed HEAD.\n",
        paste(status[nzchar(status)], collapse = "\n")
      ),
      call. = FALSE
    )
  }
  sha <- git_output(repository_root, c("rev-parse", "HEAD"))[[1L]]
  if (!grepl("^[0-9a-f]{40}$", sha)) {
    stop("git did not return a 40-character commit SHA.", call. = FALSE)
  }
  list(root = expected_root, sha = sha)
}

# Requires the declared local review tools before starting checks.
review_ready_prerequisites <- function(
  description = read.dcf("DESCRIPTION"),
  package_available = function(package) {
    requireNamespace(package, quietly = TRUE)
  },
  find_command = Sys.which,
  r_bin = R.home("bin")
) {
  field <- "Config/Needs/review"
  if (!(field %in% colnames(description))) {
    stop("DESCRIPTION is missing `", field, "`.", call. = FALSE)
  }
  packages <- dependency_requirements(description[[1L, field]])$package
  available <- vapply(packages, package_available, logical(1))
  commands <- c(
    jarl = unname(find_command("jarl")),
    R = file.path(r_bin, "R")
  )
  missing <- c(packages[!available], names(commands)[!nzchar(commands)])
  if (length(missing) > 0L) {
    stop(
      "Missing review-ready prerequisites: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  list(
    r = unname(commands[["R"]]),
    rscript = file.path(r_bin, "Rscript"),
    jarl = unname(commands[["jarl"]])
  )
}

# Finds test package references missing from dependency declarations.
review_ready_test_package_candidates <- function(
  source_root,
  scanner = function(description, files) {
    helper <- get(".check_packages_used_helper", envir = asNamespace("tools"))
    helper(description, files)
  }
) {
  description <- read.dcf(file.path(source_root, "DESCRIPTION"))[1L, ]
  files <- list.files(
    file.path(source_root, "tests"),
    pattern = "[.](Rin|[rR])$",
    recursive = TRUE,
    full.names = TRUE
  )
  usage <- scanner(description, files)
  unique(as.character(unlist(
    usage[c("others", "imports", "data")],
    use.names = FALSE
  )))
}

# Refuses undeclared test dependencies before the offline package check.
verify_review_ready_test_packages <- function(
  source_root,
  candidates = review_ready_test_package_candidates(source_root)
) {
  cat("\n==> Test dependency syntax\n")
  if (length(candidates) > 0L) {
    stop(
      "Tests refer to undeclared package candidates: ",
      paste(candidates, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Preserves spelling, jarl and package-aware lintr before admission.
review_ready_source_steps <- function(prerequisites) {
  list(
    `Package spelling` = list(
      command = prerequisites$rscript,
      args = c(
        "-e",
        paste0(
          "findings <- spelling::spell_check_package('.', vignettes = TRUE, ",
          "use_wordlist = TRUE); if (nrow(findings)) { print(findings); ",
          "stop('Package spelling found unknown words.', call. = FALSE) }; ",
          "cat('Package spelling passed.\\n')"
        )
      ),
      env = character()
    ),
    `jarl` = list(
      command = prerequisites$jarl,
      args = c("check", "."),
      env = character()
    ),
    `package-aware lintr` = list(
      command = prerequisites$rscript,
      args = c(
        "-e",
        paste0(
          "pkgload::load_all('.', quiet = TRUE); ",
          "lintr::lint_package()"
        )
      ),
      env = c(LINTR_ERROR_ON_LINT = "true")
    )
  )
}

# Runs one mandatory check and stops immediately on failure.
run_review_ready_step <- function(
  label,
  specification,
  source_root,
  runner = review_ready_run_system
) {
  cat("\n==> ", label, "\n", sep = "")
  result <- runner(
    specification$command,
    specification$args,
    directory = source_root,
    env = specification$env,
    capture = FALSE
  )
  if (result$status != 0L) {
    stop(label, " failed with status ", result$status, ".", call. = FALSE)
  }
  invisible(TRUE)
}

# Extracts only the submitted committed revision into the run directory.
archive_review_ready_source <- function(
  identity,
  workspace,
  runner = review_ready_run_system
) {
  source_root <- file.path(workspace, "source")
  dir.create(source_root, recursive = TRUE)
  archive <- file.path(workspace, "source.tar")
  result <- runner(
    "git",
    c("archive", "--format=tar", paste0("--output=", archive), identity$sha),
    directory = identity$root,
    capture = TRUE
  )
  if (result$status != 0L) {
    stop("git archive could not create the disposable source tree.", call. = FALSE)
  }
  utils::untar(archive, exdir = source_root)
  source_root
}

# Builds exactly one source tarball from the pinned source copy.
build_review_ready_tarball <- function(
  source_root,
  workspace,
  r_command,
  runner = review_ready_run_system
) {
  cat("\n==> Build source tarball\n")
  result <- runner(
    r_command,
    c("CMD", "build", source_root),
    directory = workspace,
    capture = FALSE
  )
  if (result$status != 0L) {
    stop("R CMD build failed with status ", result$status, ".", call. = FALSE)
  }
  tarballs <- list.files(
    workspace,
    pattern = "[.]tar[.]gz$",
    full.names = TRUE
  )
  if (length(tarballs) != 1L) {
    stop(
      "R CMD build produced ", length(tarballs), " source tarballs.",
      call. = FALSE
    )
  }
  tarballs[[1L]]
}

# Combines terminal process status and check findings using the shared NOTE policy.
review_ready_check_outcome <- function(result, cran_status) {
  classifications <- lapply(
    result$notes,
    classify_cran_note,
    cran_status = cran_status
  )
  unexpected <- if (length(classifications) == 0L) {
    logical()
  } else {
    !vapply(
      classifications,
      function(classification) identical(classification$status, "allowed-note"),
      logical(1)
    )
  }
  list(
    passed = identical(as.integer(result$status), 0L) &&
      !isTRUE(result$timeout) &&
      length(result$errors) == 0L &&
      length(result$warnings) == 0L,
    process_status = as.integer(result$status),
    timeout = isTRUE(result$timeout),
    errors = length(result$errors),
    warnings = length(result$warnings),
    notes = length(result$notes),
    unexpected_notes = which(unexpected),
    classifications = classifications
  )
}

# Enforces required Suggests, offline checks, snapshots and four test workers.
review_ready_rcmdcheck_env <- function(required_suggests) {
  c(
    `_R_CHECK_CRAN_INCOMING_REMOTE_` = "false",
    `_R_CHECK_SYSTEM_CLOCK_` = "false",
    NOT_CRAN = "true",
    TESTTHAT_PARALLEL = "true",
    TESTTHAT_CPUS = "4",
    MARGINPLYR_REQUIRED_SUGGESTS = paste(required_suggests, collapse = ",")
  )
}

# Provides the empty local repository indexes used by the offline check.
review_ready_offline_repositories <- function(workspace) {
  root <- file.path(workspace, "offline-repository")
  contribution <- file.path(root, "src", "contrib")
  dir.create(contribution, recursive = TRUE)
  if (!isTRUE(file.create(file.path(contribution, "PACKAGES")))) {
    stop("Could not create the offline repository index.", call. = FALSE)
  }
  path <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- if (.Platform$OS.type == "windows") "file:///" else "file://"
  repository <- paste0(prefix, utils::URLencode(path, reserved = FALSE))
  stats::setNames(
    rep(repository, 4L),
    c("CRAN", "BioCsoft", "BioCann", "BioCexp")
  )
}

# Prints every NOTE with its existing shared-policy disposition.
report_review_ready_notes <- function(result, outcome) {
  if (outcome$notes == 0L) {
    cat("No NOTEs.\n")
    return(invisible())
  }
  for (index in seq_along(result$notes)) {
    classification <- outcome$classifications[[index]]
    allowed <- identical(classification$status, "allowed-note")
    cat(
      "\nNOTE ", index, ": ",
      if (allowed) "classified by repository policy" else "review required",
      "\n",
      result$notes[[index]],
      "\n",
      sep = ""
    )
    if (allowed) {
      cat("Rationale: ", classification$rationale, "\n", sep = "")
    }
  }
  invisible()
}

# Refuses linked output roots so managed data cannot escape the worktree.
review_ready_runs_root <- function(root) {
  paths <- file.path(root, c(".review-ready", ".review-ready/runs"))
  links <- Sys.readlink(paths)
  if (any(!is.na(links) & nzchar(links))) {
    stop("Review-ready output directories must not be symlinks.", call. = FALSE)
  }
  paths[[2L]]
}

# Removes only owned inactive runs seven consecutive days after recorded UTC start.
# Missing metadata, linked paths and unavailable guards remain untouched.
review_ready_prune <- function(root) {
  runs <- review_ready_runs_root(root)
  if (!dir.exists(runs)) return(invisible(NULL))
  now <- Sys.time()
  for (path in list.dirs(runs, recursive = FALSE, full.names = TRUE)) {
    tryCatch({
      owned <- file.path(path, c("invocation.rds", "active.lock"))
      links <- Sys.readlink(c(path, owned))
      if (any(!is.na(links) & nzchar(links)) ||
          !all(file.exists(owned))) {
        cat("Retention skipped safety-undetermined data: ", path, "\n", sep = "")
        next
      }
      metadata <- readRDS(owned[[1L]])
      stamp <- metadata$started_utc
      if (!identical(metadata$version, 1L) || !is.character(stamp) || length(stamp) != 1L ||
          is.na(stamp) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[.][0-9]{6}Z$", stamp)) {
        stop("Unrecognized invocation start metadata.")
      }
      started <- as.POSIXct(strptime(stamp, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
      if (is.na(started)) stop("Invalid invocation start timestamp.")
      if (as.numeric(difftime(now, started, units = "secs")) < 7 * 24 * 60 * 60) next
      guard <- filelock::lock(owned[[2L]], timeout = 0)
      if (is.null(guard)) {
        cat("Retention skipped active run: ", path, "\n", sep = "")
        next
      }
      tryCatch({
        if (unlink(path, recursive = TRUE) != 0L || dir.exists(path)) {
          stop("Could not remove expired run.")
        }
        cat("Removed expired Review-ready run: ", path, "\n", sep = "")
      }, finally = filelock::unlock(guard))
    }, error = function(cnd) {
      cat("Retention skipped ", path, ": ", conditionMessage(cnd), "\n", sep = "")
    })
  }
  invisible(NULL)
}

# Creates a retained invocation directory outside Git's administrative files.
review_ready_new_run <- function(root, identity, selection) {
  runs <- review_ready_runs_root(root)
  dir.create(runs, recursive = TRUE, showWarnings = FALSE)
  directory <- tempfile(paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ-", tz = "UTC"),
    Sys.getpid(), "-"), tmpdir = runs)
  if (!dir.create(directory, mode = "0700")) stop("Cannot create run directory.")
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  guard <- filelock::lock(file.path(directory, "active.lock"), timeout = 0)
  if (is.null(guard)) stop("Cannot guard the new run directory.")
  metadata <- c(list(version = 1L, sha = identity$sha,
    started_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")), selection)
  saveRDS(metadata, file.path(directory, "invocation.rds"))
  list(directory = directory, guard = guard, metadata = metadata)
}

# Supported child temp/cache settings keep check by-products with the run.
review_ready_child_env <- function(directory) {
  # macOS Quarto uses HOME/Library/Caches and has no cache-directory override.
  paths <- c(TMPDIR = "tmp", TMP = "tmp", TEMP = "tmp", XDG_CACHE_HOME = "cache",
    R_USER_CACHE_DIR = "cache", R_USER_CONFIG_DIR = "config", R_USER_DATA_DIR = "data",
    DENO_DIR = "cache/deno", HOME = "cache/home", XDG_CONFIG_HOME = "config",
    XDG_DATA_HOME = "data", XDG_STATE_HOME = "state", XDG_RUNTIME_DIR = "runtime")
  values <- file.path(directory, paths)
  for (path in unique(values)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  c(stats::setNames(values, names(paths)),
    R_LIBS = paste(normalizePath(.libPaths(), mustWork = TRUE), collapse = .Platform$path.sep))
}

# Applies private storage only to check children; the runner keeps its account home.
review_ready_child_runner <- function(environment) {
  force(environment)
  function(command, args = character(), directory, env = character(), capture = FALSE) {
    values <- environment
    values[names(env)] <- env
    review_ready_run_system(command, args, directory, env = values, capture = capture)
  }
}

# Runs the five mandatory source/environment checks without requesting capacity.
run_review_ready_source <- function(source_root, prerequisites, runner = review_ready_run_system) {
  verify_review_ready_test_packages(source_root)
  cat("\n==> Required test Suggests\n")
  required <- required_test_suggests(source_root)
  for (label in names(steps <- review_ready_source_steps(prerequisites))) {
    run_review_ready_step(label, steps[[label]], source_root, runner = runner)
  }
  required
}

# Uses one stable account-wide advisory target across clones and worktrees.
review_ready_heavy_lock <- function() {
  target <- file.path(review_ready_account_home(), ".local", "state", "marginplyr",
    "review-ready-heavy.lock")
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  cat("\nWaiting for heavy package-check capacity.\n")
  flush.console()
  repeat {
    lock <- filelock::lock(target, timeout = 0)
    if (!is.null(lock)) {
      cat("Heavy package-check capacity acquired.\n")
      return(lock)
    }
    Sys.sleep(0.2)
  }
}

# Builds and checks once in a child whose temporary storage belongs to this run.
run_review_ready_heavy <- function(source_root, workspace, required, r_command) {
  tarball <- build_review_ready_tarball(source_root, workspace, r_command)
  env <- review_ready_rcmdcheck_env(required)
  env[["MARGINPLYR_REVIEW_PROGRESS_REPORTER"]] <- file.path(
    source_root, "tools", "review-ready-progress-reporter.R")
  cat("\n==> Source-tarball R CMD check --as-cran (one attempt)\n")
  result <- review_ready_progress_check(tarball, args = "--as-cran", build_args = NULL,
    check_dir = file.path(workspace, "check"), error_on = "never", env = env,
    repos = review_ready_offline_repositories(workspace))
  saveRDS(result, file.path(workspace, "check-result.rds"))
  outcome <- review_ready_check_outcome(result, cran_status_from_tarball(tarball))
  cat("\nR CMD check process status: ", outcome$process_status,
    "; timeout: ", outcome$timeout, ".\nR CMD check: ", outcome$errors,
    " ERROR(s), ", outcome$warnings, " WARNING(s), ", outcome$notes, " NOTE(s).\n", sep = "")
  report_review_ready_notes(result, outcome)
  if (!outcome$passed) stop("The source-tarball R CMD check did not pass.", call. = FALSE)
  if (length(outcome$unexpected_notes)) {
    cat("Review and explain every unclassified NOTE before publishing this commit for review.\n")
  }
  invisible(outcome)
}

# The same parent holds capacity until its isolated check child has been collected.
run_review_ready_full <- function(source_root, run, required, prerequisites, runner = review_ready_run_system) {
  lock <- review_ready_heavy_lock()
  on.exit(filelock::unlock(lock), add = TRUE)
  arguments <- list(source_root = source_root, workspace = run$directory,
    required = required, r_command = prerequisites$r)
  saveRDS(arguments, file.path(run$directory, "worker-arguments.rds"))
  worker <- file.path(run$directory, "worker.R")
  writeLines(c(
    'args <- readRDS(commandArgs(trailingOnly=TRUE)[[1L]])',
    'for (relative in c(".github/scripts/cran-note-policy.R", "tools/review-ready-check-lib.R", "tools/review-ready-progress.R")) source(file.path(args$source_root, relative))',
    'do.call(run_review_ready_heavy, args)'
  ), worker)
  result <- runner(prerequisites$rscript,
    c(worker, file.path(run$directory, "worker-arguments.rds")), directory = run$directory)
  if (result$status != 0L) stop("Package build/check failed with status ", result$status, ".", call. = FALSE)
}

# The command records one result for one committed revision; it never retries.
review_ready_check_cli <- function(args, expected_root) {
  run <- NULL
  console <- NULL
  status <- tryCatch({
    selection <- parse_review_ready_args(args)
    identity <- review_ready_identity(expected_root)
    review_ready_prune(expected_root)
    run <- review_ready_new_run(expected_root, identity, selection)
    on.exit(filelock::unlock(run$guard), add = TRUE)
    console <- file(file.path(run$directory, "console.log"), open = "wt")
    sink(console, split = TRUE)
    on.exit({ sink(); close(console) }, add = TRUE, after = FALSE)
    cat("Review-ready commit: ", identity$sha, "\nArtifacts: ", run$directory,
      "\nScope: ", selection$scope, "\nReason: ", selection$reason, "\n", sep = "")
    child_env <- review_ready_child_env(run$directory)
    runner <- review_ready_child_runner(child_env)
    source_root <- archive_review_ready_source(identity, run$directory, runner = runner)
    prerequisites <- review_ready_prerequisites(read.dcf(file.path(source_root, "DESCRIPTION")))
    required <- run_review_ready_source(source_root, prerequisites, runner = runner)
    if (selection$scope == "source") {
      cat("\nFive mandatory checks passed for ", identity$sha, ".\n",
        "Heavy package check omitted: ", selection$reason, "\n", sep = "")
    } else {
      run_review_ready_full(source_root, run, required, prerequisites, runner = runner)
      cat("\nFull package check passed for ", identity$sha, ".\n", sep = "")
    }
    saveRDS(c(run$metadata, list(status = "passed", heavy_check = if (selection$scope == "source") "omitted" else "passed")),
      file.path(run$directory, "result.rds"))
    0L
  }, interrupt = function(cnd) {
    cat("Review-ready cancelled.\n")
    130L
  }, error = function(cnd) {
    cat("Review-ready failed: ", conditionMessage(cnd), "\n", sep = "")
    1L
  })
  if (!is.null(run) && status != 0L) {
    saveRDS(c(run$metadata, list(status = if (status == 130L) "cancelled" else "failed")),
      file.path(run$directory, "result.rds"))
  }
  as.integer(status)
}
