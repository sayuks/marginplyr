# Implementation of the fixed, non-mutating review-ready check. It runs every
# step against a disposable archive of the clean committed HEAD so the reported
# SHA identifies the sources that were actually checked.

# Runs one subprocess from a named directory and returns its exit status and,
# when requested, its combined output.
review_ready_run_system <- function(
  command,
  args = character(),
  directory,
  env = character(),
  capture = FALSE
) {
  original_directory <- setwd(directory)
  on.exit(setwd(original_directory), add = TRUE)
  destination <- if (capture) TRUE else ""
  result <- suppressWarnings(system2(
    command,
    vapply(args, shQuote, character(1)),
    stdout = destination,
    stderr = destination,
    env = env
  ))
  if (is.character(result)) {
    status <- attr(result, "status")
    if (is.null(status)) {
      status <- 0L
    }
    return(list(status = as.integer(status), output = result))
  }
  list(status = as.integer(result), output = character())
}

# Returns one git query's output or refuses a failed query.
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

# Binds the check to one clean commit in the repository holding the entry point.
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

# Rejects configuration knobs: the command is one fixed gate by design.
parse_review_ready_args <- function(args) {
  if (length(args) > 0L) {
    stop("The review-ready check takes no arguments.", call. = FALSE)
  }
  list()
}

# Checks the tools needed before allocating or running the disposable checkout.
review_ready_prerequisites <- function(
  package_available = function(package) {
    requireNamespace(package, quietly = TRUE)
  },
  find_command = Sys.which,
  r_bin = R.home("bin")
) {
  packages <- c("testthat", "pkgload", "lintr", "rcmdcheck")
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

# Defines the three working-tree checks before the source-tarball boundary.
review_ready_source_steps <- function(prerequisites) {
  list(
    `Full testthat suite` = list(
      command = prerequisites$rscript,
      args = c(
        "-e",
        paste0(
          "testthat::test_local('.', reporter = 'summary', ",
          "stop_on_failure = TRUE)"
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
      env = "LINTR_ERROR_ON_LINT=true"
    )
  )
}

# Runs one named fixed step and stops before later, more expensive work.
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

# Expands the exact committed tree without allowing ignored local files to
# influence any check.
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

# Builds exactly one source tarball from the disposable committed tree.
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

# Reduces the source-tarball result to the automatic verdict and NOTE review.
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

# Names the environment entry because rcmdcheck applies names as variable keys.
review_ready_rcmdcheck_env <- function() {
  c(`_R_CHECK_CRAN_INCOMING_REMOTE_` = "false")
}

# Prints every NOTE and whether the shared CRAN policy already explains it.
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

# Runs the source-tarball check without remote incoming checks, which are the
# release flow's responsibility.
run_review_ready_rcmdcheck <- function(tarball, workspace) {
  cat("\n==> Source-tarball R CMD check --as-cran\n")
  result <- rcmdcheck::rcmdcheck(
    tarball,
    args = "--as-cran",
    build_args = NULL,
    check_dir = file.path(workspace, "check"),
    error_on = "never",
    env = review_ready_rcmdcheck_env()
  )
  outcome <- review_ready_check_outcome(
    result,
    cran_status = cran_status_from_tarball(tarball)
  )
  cat(
    "\nR CMD check process status: ", outcome$process_status, ".\n",
    "R CMD check: ", outcome$errors, " ERROR(s), ",
    outcome$warnings, " WARNING(s), ", outcome$notes, " NOTE(s).\n",
    sep = ""
  )
  report_review_ready_notes(result, outcome)
  if (!outcome$passed) {
    stop("The source-tarball R CMD check did not pass.", call. = FALSE)
  }
  outcome
}

# Runs the complete fixed check and returns the identity and NOTE disposition.
run_review_ready_check <- function(
  repository_root,
  identity = review_ready_identity(repository_root),
  prerequisites = review_ready_prerequisites()
) {
  workspace <- tempfile("marginplyr-review-ready-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  source_root <- archive_review_ready_source(identity, workspace)

  cat("Review-ready commit: ", identity$sha, "\n", sep = "")
  cat("Disposable source: ", source_root, "\n", sep = "")
  steps <- review_ready_source_steps(prerequisites)
  for (label in names(steps)) {
    run_review_ready_step(label, steps[[label]], source_root)
  }
  tarball <- build_review_ready_tarball(
    source_root,
    workspace,
    prerequisites$r
  )
  outcome <- run_review_ready_rcmdcheck(tarball, workspace)

  cat("\nReview-ready check passed for ", identity$sha, ".\n", sep = "")
  if (length(outcome$unexpected_notes) > 0L) {
    cat(
      "Review and explain every unclassified NOTE before publishing this ",
      "commit for review.\n",
      sep = ""
    )
  }
  invisible(list(identity = identity, outcome = outcome))
}

# Command-line boundary: invalid invocation or any failed step exits nonzero.
review_ready_check_cli <- function(args, expected_root) {
  result <- tryCatch({
    parse_review_ready_args(args)
    run_review_ready_check(expected_root)
    0L
  }, error = function(cnd) {
    cat("Review-ready check failed: ", conditionMessage(cnd), "\n", file = stderr(), sep = "")
    1L
  })
  as.integer(result)
}
