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

# Returns package names that R's test-source scan cannot match to DESCRIPTION.
# The fixed gate refuses them before using empty local repository indexes, so
# the offline check cannot hide a misspelled or undeclared dependency.
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

# Refuses a test dependency that an empty offline repository would otherwise
# make R's package-usage check discard as a non-package name.
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

# Defines the working-tree checks before the source-tarball boundary.
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
      env = "LINTR_ERROR_ON_LINT=true"
    ),
    `Strict line coverage` = list(
      command = prerequisites$rscript,
      args = "tools/coverage-check.R",
      env = character()
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

# Names the environment entries because rcmdcheck applies names as variable
# keys. Remote incoming checks and the external clock belong to the release
# flow; the local future-file-timestamp comparison remains enabled here.
review_ready_rcmdcheck_env <- function() {
  c(
    `_R_CHECK_CRAN_INCOMING_REMOTE_` = "false",
    `_R_CHECK_SYSTEM_CLOCK_` = "false"
  )
}

# Creates the four named repository indexes R consults under --as-cran without
# allowing the fixed local gate to reach a network service.
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

# Reads a tool version without letting an unavailable tool hide the check error.
review_ready_tool_version <- function(command) {
  if (!nzchar(command)) {
    return("unavailable (executable not found)")
  }
  tryCatch({
    output <- suppressWarnings(system2(
      command, "--version", stdout = TRUE, stderr = TRUE, timeout = 5
    ))
    status <- attr(output, "status")
    paste(c(command, output, if (!is.null(status)) paste("exit", status)),
      collapse = " | ")
  }, error = function(cnd) paste(command, conditionMessage(cnd), sep = " | "))
}

# Identifies the Quarto on the check's search path and its bundled or overridden
# Deno, rather than an unrelated Deno on PATH.
review_ready_render_versions <- function() {
  quarto <- Sys.getenv("QUARTO_PATH", unname(Sys.which("quarto")))
  deno <- Sys.getenv("QUARTO_DENO")
  if (!nzchar(deno) && file.exists(quarto) && !dir.exists(quarto)) {
    bin <- dirname(normalizePath(quarto, winslash = "/"))
    architecture <- if (grepl("arm|aarch64", Sys.info()[["machine"]])) {
      "aarch64"
    } else {
      "x86_64"
    }
    candidates <- file.path(bin, c(
      paste0("tools/", architecture, "/deno"),
      "tools/x86_64/deno.exe", "tools/deno.exe", "tools/deno"
    ))
    found <- candidates[file.exists(candidates)]
    if (length(found) > 0L) {
      deno <- found[[1L]]
    }
  }
  c(Quarto = review_ready_tool_version(quarto), Deno = review_ready_tool_version(deno))
}

# Offers native crash metadata locations without reading reports or changing
# crash collection. A missing native report does not invalidate the bundle.
review_ready_crash_guidance <- function(system = Sys.info()[["sysname"]]) {
  switch(system,
    Darwin = paste(
      "macOS: candidate reports: ~/Library/Logs/DiagnosticReports/deno-*.ips",
      "(also /Library/Logs/DiagnosticReports/). Open Console > Crash Reports",
      "and match the executable, timestamp, and image UUID."
    ),
    Windows = paste(
      "Windows: Event Viewer > Windows Logs > Application; look for",
      "Application Error (Event ID 1000) at the failure time and match",
      "the executable and faulting module."
    ),
    Linux = paste(
      "Linux with systemd-coredump: inspect coredumpctl list and",
      "coredumpctl info for the executable and failure time (metadata only).",
      "Do not use coredumpctl dump or debug."
    ),
    "Consult the operating system's application crash log for this timestamp."
  ) |>
    paste(
      "If the facility or matching report is unavailable, retain this bundle",
      "and record that absence; no OS setting change or memory dump is needed.",
      "Native reports are not copied automatically."
    )
}

# Retains only diagnostic text outside the repository before workspace cleanup.
# The caller supplies the original error even when rcmdcheck returned no result.
preserve_review_ready_failure <- function(
  workspace, identity, started, result, error, diagnostic_root
) {
  dir.create(diagnostic_root, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(diagnostic_root, winslash = "/", mustWork = TRUE)
  repository <- normalizePath(identity$root, winslash = "/", mustWork = TRUE)
  if (identical(root, repository) || startsWith(root, paste0(repository, "/"))) {
    stop("The diagnostic directory must be outside the repository.", call. = FALSE)
  }
  bundle <- tempfile(
    paste0(format(started, "%Y%m%dT%H%M%SZ-", tz = "UTC"),
      substr(identity$sha, 1L, 12L), "-"),
    tmpdir = root
  )
  if (!dir.create(bundle, mode = "0700")) {
    stop("Could not create the diagnostic bundle: ", bundle, call. = FALSE)
  }
  cat("Review-ready failure diagnostics: ", bundle, "\n", sep = "")
  status <- if (is.null(result$status)) "unavailable (check raised an error)" else result$status
  metadata <- c(
    paste("Commit:", identity$sha),
    paste("Started (UTC):", format(started, tz = "UTC", usetz = TRUE)),
    paste("Failed (UTC):", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste("Process status:", status),
    paste("Timeout:", if (is.null(result$timeout)) "unavailable" else result$timeout),
    paste("Error class:", paste(class(error), collapse = ", ")),
    paste("R:", R.version.string),
    paste("OS:", paste(Sys.info()[c("sysname", "release", "version", "machine")], collapse = " | "))
  )
  writeLines(metadata, file.path(bundle, "metadata.txt"))
  writeLines(conditionMessage(error), file.path(bundle, "error.txt"))
  for (stream in c("stdout", "stderr")) {
    writeLines(as.character(c(result[[stream]], error[[stream]])),
      file.path(bundle, paste0(stream, ".txt")))
  }
  logs <- list.files(workspace,
    pattern = "([.]log|[.]Rout([.]fail|[.]save)?)$",
    recursive = TRUE, all.files = TRUE
  )
  logs <- logs[logs == "console.log" | startsWith(logs, "check/")]
  for (relative in logs) {
    destination <- file.path(bundle, relative)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(workspace, relative), destination)) {
      cat("Could not retain log: ", relative, "\n", file = stderr(), sep = "")
    }
  }
  versions <- review_ready_render_versions()
  cat(paste(names(versions), versions, sep = ": "),
    file = file.path(bundle, "metadata.txt"), sep = "\n", append = TRUE)
  guidance <- review_ready_crash_guidance()
  writeLines(guidance, file.path(bundle, "native-crash-guidance.txt"))
  cat(guidance, "\n")
  invisible(bundle)
}

# Runs the source-tarball check once. A failed result or an exception preserves
# diagnostics before the caller removes the disposable workspace.
run_review_ready_rcmdcheck <- function(
  tarball, workspace, identity, diagnostic_root,
  checker = rcmdcheck::rcmdcheck
) {
  cat("\n==> Source-tarball R CMD check --as-cran\n")
  started <- Sys.time()
  result <- NULL
  tryCatch({
    console <- file(file.path(workspace, "console.log"), open = "wt")
    sink(console, split = TRUE)
    result <- tryCatch(withCallingHandlers(checker(
      tarball,
      args = "--as-cran",
      build_args = NULL,
      check_dir = file.path(workspace, "check"),
      error_on = "never",
      env = review_ready_rcmdcheck_env(),
      repos = review_ready_offline_repositories(workspace)
    ), message = function(cnd) {
      cat(conditionMessage(cnd), file = console)
    }, warning = function(cnd) {
      cat(conditionMessage(cnd), "\n", file = console)
    }), finally = {
      sink()
      close(console)
    })
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
  }, error = function(cnd) {
    tryCatch(
      preserve_review_ready_failure(workspace, identity, started, result, cnd, diagnostic_root),
      error = function(retention_error) {
        cat("Could not retain all diagnostics: ", conditionMessage(retention_error),
          "\n", file = stderr(), sep = "")
      }
    )
    stop(cnd)
  })
}

# Runs the complete fixed check and returns the identity and NOTE disposition.
run_review_ready_check <- function(
  repository_root,
  identity = review_ready_identity(repository_root),
  prerequisites = review_ready_prerequisites(),
  runner = review_ready_run_system,
  checker = rcmdcheck::rcmdcheck,
  diagnostic_root = file.path(tools::R_user_dir("marginplyr", "cache"), "review-ready-failures")
) {
  workspace <- tempfile("marginplyr-review-ready-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  source_root <- archive_review_ready_source(identity, workspace, runner = runner)

  cat("Review-ready commit: ", identity$sha, "\n", sep = "")
  cat("Disposable source: ", source_root, "\n", sep = "")
  verify_review_ready_test_packages(source_root)
  steps <- review_ready_source_steps(prerequisites)
  for (label in names(steps)) {
    run_review_ready_step(label, steps[[label]], source_root, runner = runner)
  }
  tarball <- build_review_ready_tarball(
    source_root,
    workspace,
    prerequisites$r,
    runner = runner
  )
  outcome <- run_review_ready_rcmdcheck(tarball, workspace, identity,
    checker = checker, diagnostic_root = diagnostic_root)

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
review_ready_check_cli <- function(args, expected_root, ...) {
  result <- tryCatch({
    parse_review_ready_args(args)
    run_review_ready_check(expected_root, ...)
    0L
  }, error = function(cnd) {
    cat("Review-ready check failed: ", conditionMessage(cnd), "\n", file = stderr(), sep = "")
    1L
  })
  as.integer(result)
}
