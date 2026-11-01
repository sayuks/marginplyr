# Implementation of the fixed, non-mutating review-ready check.

# Updates the private supervisor's stage report; stdout is never reuse evidence.
review_ready_report <- function(completed = NULL, unit = NULL, state = NULL,
                                check = NULL, timestamps = NULL, omitted = NULL) {
  path <- Sys.getenv("MARGINPLYR_REVIEW_STAGE_REPORT")
  if (!nzchar(path)) return(invisible())
  report <- if (file.exists(path)) jsonlite::fromJSON(path, simplifyVector = FALSE) else
    list(request = Sys.getenv("MARGINPLYR_REVIEW_REQUEST"), units = list(), completed = list())
  if (!is.null(completed)) report$completed <- c(report$completed, list(completed))
  if (!is.null(unit)) report$units[[unit]] <- state
  if (!is.null(check)) report$check <- check
  if (!is.null(timestamps)) report$timestamps <- timestamps
  if (!is.null(omitted)) report$omitted <- as.list(omitted)
  temporary <- paste0(path, ".new")
  writeLines(jsonlite::toJSON(report, auto_unbox = TRUE, null = "null"), temporary)
  if (!file.rename(temporary, path)) stop("Cannot retain supervised stage report.", call. = FALSE)
  invisible()
}

# Captures the top-level paths whose mtimes the supported R checker inspects.
review_ready_timestamps <- function(tarball, workspace) {
  copy <- file.path(workspace, "timestamp-inventory")
  dir.create(copy)
  utils::untar(tarball, exdir = copy)
  roots <- list.dirs(copy, recursive = FALSE, full.names = TRUE)
  if (length(roots) != 1L) stop("Unknown source tarball timestamp layout.", call. = FALSE)
  paths <- list.files(roots, all.files = TRUE, full.names = TRUE, include.dirs = TRUE, no.. = TRUE)
  values <- as.numeric(file.mtime(paths))
  if (!length(paths) || anyNA(values)) stop("Timestamp inventory unavailable.", call. = FALSE)
  list(observed = as.numeric(Sys.time()), paths = stats::setNames(as.list(values), basename(paths)))
}

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
  review_ready_report(completed = label)
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
# flow. The caller supplies the validated suite Suggests; NOT_CRAN enables
# snapshot comparisons within the tarball's single test-suite run.
review_ready_rcmdcheck_env <- function(required_suggests, path = "Full") {
  c(
    `_R_CHECK_CRAN_INCOMING_REMOTE_` = "false",
    `_R_CHECK_SYSTEM_CLOCK_` = "false",
    NOT_CRAN = "true",
    TESTTHAT_PARALLEL = if (identical(path, "Documentation")) "false" else "true",
    TESTTHAT_CPUS = "4",
    MARGINPLYR_REQUIRED_SUGGESTS = paste(required_suggests, collapse = ",")
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
review_ready_render_paths <- function() {
  quarto <- Sys.getenv("QUARTO_PATH", unname(Sys.which("quarto")))
  deno <- Sys.getenv("QUARTO_DENO")
  if (!nzchar(deno) && file.exists(quarto) && !dir.exists(quarto)) {
    bin <- dirname(normalizePath(quarto, winslash = "/"))
    machine <- Sys.info()[["machine"]]
    if (identical(Sys.info()[["sysname"]], "Darwin")) {
      brand <- suppressWarnings(system2("/usr/sbin/sysctl",
        c("-n", "machdep.cpu.brand_string"), stdout = TRUE, stderr = FALSE))
      if (any(grepl("Apple|ARM", brand))) machine <- "aarch64"
      if (any(grepl("Intel|Xeon|Core", brand))) machine <- "x86_64"
    }
    architecture <- if (grepl("arm|aarch64", machine)) {
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
  c(Quarto = quarto, Deno = deno)
}

# Records the versions of the tools selected by the rendering environment.
review_ready_render_versions <- function() {
  vapply(review_ready_render_paths(), review_ready_tool_version, character(1))
}

# Snapshots input bytes and the release launcher's selected executables. An
# unavailable identity disables recovery without preventing the initial check.
review_ready_retry_identity <- function(tarball) {
  paths <- review_ready_render_paths()
  files <- c(tarball = tarball, paths)
  if (!all(startsWith(files, "/")) || !all(file.exists(files)) ||
      any(dir.exists(files)) || any(file.access(paths, 1L) != 0L)) {
    return(NULL)
  }
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  names(files) <- c("tarball", "Quarto", "Deno")
  target <- file.path(dirname(files[["Quarto"]]), "quarto.js")
  if (!file.exists(target) || dir.exists(target)) return(NULL)
  files <- c(files, Quarto_script = target)
  versions <- vapply(files[c("Quarto", "Deno")], function(command) {
    output <- suppressWarnings(system2(command, "--version",
      stdout = TRUE, stderr = TRUE, timeout = 5))
    if (!is.null(attr(output, "status")) || length(output) == 0L) {
      stop("Could not identify rendering executable.", call. = FALSE)
    }
    paste(output, collapse = "\n")
  }, character(1))
  hashes <- tools::md5sum(files)
  names(hashes) <- names(files)
  if (anyNA(hashes)) return(NULL)
  list(paths = files[c("Quarto", "Deno")], hashes = hashes, versions = versions)
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

# Copies the available check and console logs, refusing a partial copy.
copy_review_ready_logs <- function(workspace, destination) {
  logs <- list.files(workspace,
    pattern = "([.]log|[.]Rout([.]fail|[.]save)?)$",
    recursive = TRUE, all.files = TRUE
  )
  logs <- logs[logs == "console.log" | startsWith(logs, "check/")]
  for (relative in logs) {
    target <- file.path(destination, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(workspace, relative), target)) {
      stop("Could not retain log: ", relative, call. = FALSE)
    }
  }
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
  writeLines(as.character(c(result$errors, result$warnings, result$notes)),
    file.path(bundle, "check-findings.txt"))
  copy_review_ready_logs(workspace, bundle)
  versions <- review_ready_render_versions()
  cat(paste(names(versions), versions, sep = ": "),
    file = file.path(bundle, "metadata.txt"), sep = "\n", append = TRUE)
  guidance <- review_ready_crash_guidance()
  writeLines(guidance, file.path(bundle, "native-crash-guidance.txt"))
  cat(guidance, "\n")
  invisible(bundle)
}

# Recognizes the measured macOS launcher/quarto-R failure form, including its
# complete R vignette envelope. Unknown wrappers or additional failures refuse
# recovery; this does not identify a particular native defect.
review_ready_native_crash <- function(result, tool_identity, system) {
  if (!identical(system, "Darwin") || is.null(tool_identity) ||
      !isTRUE(result$status == 1L) ||
      !identical(result$timeout, FALSE) || length(result$warnings) != 0L ||
      length(result$test_fail) != 0L || !is.character(result$errors) ||
      length(result$errors) != 1L || anyNA(result$errors)) return(FALSE)
  launcher <- tool_identity$paths[["Quarto"]]
  if (length(launcher) != 1L || is.na(launcher) || !nzchar(launcher)) return(FALSE)
  lines <- trimws(strsplit(result$errors, "\n", fixed = TRUE)[[1L]])
  lines <- chartr("‘’", "''", lines[nzchar(lines)])
  header <- paste0("^(\\* )?checking re-building of vignette outputs [.][.][.] ",
    "(\\[([0-9]+s/[0-9]+s|[0-9]+m/[0-9]+m)\\] )?ERROR$")
  if (length(lines) < 3L || !grepl(header, lines[[1L]]) ||
      lines[[2L]] != "Error(s) in re-building vignettes:") return(FALSE)
  cursor <- 3L
  failed <- character()
  while (cursor <= length(lines) && startsWith(lines[[cursor]], "--- re-building ")) {
    start <- regexec("^--- re-building '([^']+)' using ([[:alnum:]_.-]+)$", lines[[cursor]])
    fields <- regmatches(lines[[cursor]], start)[[1L]]
    if (length(fields) != 3L) return(FALSE)
    filename <- fields[[2L]]
    endings <- which(startsWith(lines, "--- failed re-building ") |
      startsWith(lines, "--- finished re-building "))
    endings <- endings[endings > cursor]
    if (length(endings) == 0L) return(FALSE)
    end <- endings[[1L]]
    body <- if (end == cursor + 1L) character() else lines[seq.int(cursor + 1L, end - 1L)]
    if (any(startsWith(body, "--- re-building "))) return(FALSE)
    prefix <- paste0(launcher, ": line ")
    signal <- startsWith(body, prefix) & grepl(
      '^[0-9]+: [[:space:]]*[0-9]+ Segmentation fault: 11[[:space:]]+"\\$\\{QUARTO_DENO\\}" ',
      substring(body, nchar(prefix) + 1L)
    )
    if (lines[[end]] == paste0("--- finished re-building '", filename, "'")) {
      if (any(signal)) return(FALSE)
    } else if (lines[[end]] == paste0("--- failed re-building '", filename, "'")) {
      if (fields[[3L]] != "html" || !endsWith(filename, ".qmd") ||
          sum(signal) != 1L) return(FALSE)
      wrapper <- c(
        paste0("Error: processing vignette '", filename, "' failed with diagnostics:"),
        "! Error running quarto CLI from R.",
        "Caused by error:",
        "! Could not evaluate cli `{}` expression: `QUARTO_DENO`.",
        "Caused by error:",
        "! object 'QUARTO_DENO' not found"
      )
      signal_index <- which(signal)
      invocation <- sub('^[0-9]+: [[:space:]]*[0-9]+ Segmentation fault: 11[[:space:]]+',
        "", substring(body[[signal_index]], nchar(prefix) + 1L))
      if (!identical(invocation, paste(
        '"${QUARTO_DENO}" ${QUARTO_ACTION} ${QUARTO_DENO_OPTIONS}',
        '${QUARTO_DENO_EXTRA_OPTIONS} "${QUARTO_TARGET}" "$@"'
      ))) return(FALSE)
      if (!identical(body[seq.int(signal_index + 1L, length(body))], wrapper)) return(FALSE)
      preceding <- head(body, signal_index - 1L)
      if (any(grepl("error|warning|failed|segmentation fault", preceding,
        ignore.case = TRUE))) return(FALSE)
      failed <- c(failed, filename)
    } else {
      return(FALSE)
    }
    cursor <- end + 1L
  }
  if (length(failed) != 1L || cursor > length(lines)) return(FALSE)
  remaining <- lines[seq.int(cursor, length(lines))]
  if (identical(tail(remaining, 1L), "Execution halted")) remaining <- head(remaining, -1L)
  identical(remaining, c("SUMMARY: processing the following file failed:",
    paste0("'", failed, "'"), "Error: Vignette re-building failed."))
}

# Runs one complete check in a fresh directory and captures its terminal state.
review_ready_check_attempt <- function(tarball, workspace, checker, repositories, env, args) {
  attempt <- tempfile("check-attempt-", tmpdir = workspace)
  dir.create(attempt)
  started <- Sys.time()
  result <- NULL
  outcome <- NULL
  error <- tryCatch({
    console <- file(file.path(attempt, "console.log"), open = "wt")
    sink(console, split = TRUE)
    result <- tryCatch(withCallingHandlers(checker(
      tarball,
      args = args,
      build_args = NULL,
      check_dir = file.path(attempt, "check"),
      error_on = "never",
      env = env,
      repos = repositories
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
    NULL
  }, error = function(cnd) cnd)
  list(workspace = attempt, started = started, finished = Sys.time(),
    result = result, outcome = outcome, error = error)
}

# Records both verdicts in the first failure bundle, including a pending retry
# if the invocation is interrupted before its second terminal result.
record_review_ready_retry <- function(bundle, identity, state, second = NULL,
                                      second_bundle = NULL) {
  writeLines(c(
    "Attempt 1: failed",
    "Eligibility: macOS Quarto launcher reported Deno SIGSEGV during vignette rebuilding.",
    paste("Attempt 2:", state),
    if (!is.null(second)) c(
      paste("Started (UTC):", format(second$started, tz = "UTC", usetz = TRUE)),
      paste("Finished (UTC):", format(second$finished, tz = "UTC", usetz = TRUE)),
      if (is.null(second$result)) "Check result: unavailable (check raised an error)" else c(
        paste("Process status:", second$result$status),
        paste("Timeout:", second$result$timeout),
        paste("ERRORs:", length(second$result$errors)),
        paste("WARNINGs:", length(second$result$warnings)),
        paste("NOTEs:", length(second$result$notes))
      ),
      if (!is.null(second$error)) paste("Error:", conditionMessage(second$error)),
      if (!is.null(second$error) && is.null(second_bundle)) "Diagnostics: incomplete",
      if (!is.null(second_bundle)) paste("Failure diagnostics:", second_bundle)
    )
  ), file.path(bundle, "retry-result.txt"))
  dput(identity, file = file.path(bundle, "retry-identity.txt"))
  if (!is.null(second$result)) {
    writeLines(as.character(c(second$result$errors, second$result$warnings,
      second$result$notes)), file.path(bundle, "retry-findings.txt"))
  }
  if (!is.null(second) && is.null(second$error)) {
    copy_review_ready_logs(second$workspace, file.path(bundle, "retry"))
  }
}

# Preserves every failed attempt; only the supported native crash can consume
# the single retry authorized by design/agents/local-checks.md.
run_review_ready_rcmdcheck <- function(
  tarball, workspace, identity, diagnostic_root, env,
  checker = review_ready_progress_check,
  system = Sys.info()[["sysname"]],
  retry_identity = review_ready_retry_identity,
  preserver = preserve_review_ready_failure,
  path = "Full"
) {
  args <- c("--as-cran", if (identical(path, "Documentation")) "--no-tests")
  cat("\n==> Source-tarball R CMD check ", paste(args, collapse = " "), "\n", sep = "")
  snapshot <- function() tryCatch(retry_identity(tarball), error = function(cnd) NULL)
  before <- if (identical(system, "Darwin")) snapshot() else NULL
  repositories <- review_ready_offline_repositories(workspace)
  first_failure <- NULL
  attempt_records <- list()
  for (number in 1:2) {
    cat("R CMD check attempt ", number, ".\n", sep = "")
    attempt <- review_ready_check_attempt(tarball, workspace, checker, repositories, env, args)
    if (!is.null(attempt$result)) {
      attempt_records[[number]] <- list(
        status = attempt$result$status, timeout = attempt$result$timeout,
        errors = length(attempt$result$errors), warnings = length(attempt$result$warnings),
        notes = as.list(vapply(attempt$outcome$classifications, `[[`, character(1), "status"))
      )
    }
    bundle <- NULL
    if (!is.null(attempt$error)) {
      bundle <- tryCatch({
        retained <- preserver(attempt$workspace, identity, attempt$started,
          attempt$result, attempt$error, diagnostic_root)
        dput(before, file = file.path(retained, "attempt-identity.txt"))
        writeLines(c(paste("Path:", path), paste("Arguments:", paste(args, collapse = " "))),
          file.path(retained, "attempt-selection.txt"))
        retained
      }, error = function(cnd) {
        cat("Could not retain all diagnostics: ", conditionMessage(cnd),
          "\n", file = stderr(), sep = "")
        NULL
      })
    }
    if (number == 2L) {
      record_review_ready_retry(first_failure, before,
        if (is.null(attempt$error)) "passed" else "failed", attempt, bundle)
    }
    if (is.null(attempt$error)) {
      outcome <- attempt$outcome
      outcome$attempts <- number
      outcome$retried <- number == 2L
      outcome$first_failure <- first_failure
      review_ready_report(check = list(
        args = as.list(args), status = attempt$result$status, timeout = attempt$result$timeout,
        errors = outcome$errors, warnings = outcome$warnings,
        notes = as.list(vapply(outcome$classifications, `[[`, character(1), "status")),
        attempts = attempt_records, retry_verified = number == 2L,
        first_failure = first_failure, retry_identity = if (number == 2L) before else NULL
      ))
      if (outcome$retried) {
        cat("Source-tarball check passed-after-retry; attempt 1 remains failed.\n",
          "First failure diagnostics: ", first_failure, "\n", sep = "")
      }
      return(outcome)
    }
    if (number == 2L) stop(attempt$error)
    if (is.null(bundle)) {
      cat("No retry: diagnostic preservation was incomplete.\n")
      stop(attempt$error)
    }
    if (is.null(before)) {
      cat("No retry: tarball or Quarto/Deno identity is unavailable.\n")
      stop(attempt$error)
    }
    if (is.null(attempt$outcome) ||
        !review_ready_native_crash(attempt$result, before, system)) {
      cat("No retry: this failure is not an unambiguous supported Deno crash.\n")
      stop(attempt$error)
    }
    after <- snapshot()
    if (is.null(after) || !identical(before, after)) {
      cat("No retry: tarball or Quarto/Deno identity is unavailable or changed.\n")
      stop(attempt$error)
    }
    first_failure <- bundle
    record_review_ready_retry(first_failure, before, "pending")
    cat("Retrying the same source tarball once after the recorded Deno SIGSEGV.\n")
  }
}

# Obtains one live manager authorization over its request-private channel.
review_ready_supervision_proof <- function() {
  supervisor <- Sys.getenv("MARGINPLYR_REVIEW_SUPERVISOR")
  command <- if (endsWith(supervisor, ".R")) file.path(R.home("bin"), "Rscript") else
    Sys.which("python3")
  if (!nzchar(supervisor) || !file.exists(supervisor) || !nzchar(command)) {
    stop(
      "Review-ready supervision is required; refresh the common CLI.",
      call. = FALSE
    )
  }
  output <- system2(
    command, c(shQuote(supervisor), "--gate-proof"), stdout = TRUE
  )
  if (!is.null(attr(output, "status"))) {
    stop("Review-ready supervisor refused gate authorization.", call. = FALSE)
  }
  connection <- textConnection(output)
  on.exit(close(connection), add = TRUE)
  proof <- read.dcf(connection)
  if (nrow(proof) != 1L || !all(c("Path", "Capacity") %in% colnames(proof))) {
    stop("Invalid Review-ready supervision proof.", call. = FALSE)
  }
  list(path = proof[[1L, "Path"]], capacity = proof[[1L, "Capacity"]])
}

# Missing or corrupt transport never grants Full; proof comes from the manager.
read_review_ready_selection <- function(
  proof = review_ready_supervision_proof
) {
  path <- Sys.getenv("MARGINPLYR_REVIEW_SELECTION")
  if (!nzchar(path)) {
    stop(
      "Supervised selection transport is required; no Full fallback.",
      call. = FALSE
    )
  }
  record <- read.dcf(path)
  required <- c(
    "Path", "Commit", "Request", "Verifiers", "Protocol", "Capacity", "Base"
  )
  if (nrow(record) != 1L || !all(required %in% colnames(record)) ||
        !record[[1L, "Path"]] %in% c("Full", "Repository", "Documentation") ||
        !record[[1L, "Protocol"]] %in% c("3", "4") ||
        !record[[1L, "Capacity"]] %in% c("Full", "exempt") ||
        !grepl("^[0-9a-f]{40}$", record[[1L, "Commit"]]) ||
        !nzchar(Sys.getenv("MARGINPLYR_REVIEW_REQUEST")) ||
        !identical(record[[1L, "Request"]],
                   Sys.getenv("MARGINPLYR_REVIEW_REQUEST"))) {
    stop("Invalid admitted Review-ready selection.", call. = FALSE)
  }
  verifiers <- record[[1L, "Verifiers"]]
  if (record[[1L, "Path"]] %in% c("Repository", "Documentation") &&
        (is.na(verifiers) || !nzchar(verifiers))) {
    stop("Selected path requires mapped artifact verification.", call. = FALSE)
  }
  if (is.na(record[[1L, "Base"]]) ||
      !grepl("^[0-9a-f]{40}$", record[[1L, "Base"]])) {
    stop("Invalid admitted comparison base; CI validation remains unresolved.",
      call. = FALSE)
  }
  authorization <- proof()
  reused <- if ("Reused" %in% colnames(record) && !is.na(record[[1L, "Reused"]]) &&
                 nzchar(record[[1L, "Reused"]])) strsplit(record[[1L, "Reused"]], " ", fixed = TRUE)[[1L]] else
    character()
  if (!all(reused %in% c("build-full", "build-documentation", "generation", "documentation-assertions")) ||
      anyDuplicated(reused)) stop("Invalid stage reuse transport.", call. = FALSE)
  if (!identical(authorization$path, record[[1L, "Path"]]) ||
        !identical(authorization$capacity, record[[1L, "Capacity"]]) ||
        (identical(authorization$path, "Full") &&
           !"build-full" %in% reused && !identical(authorization$capacity, "Full"))) {
    stop("Selection does not match owned supervisor capacity.", call. = FALSE)
  }
  selection <- list(
    path = record[[1L, "Path"]], sha = record[[1L, "Commit"]],
    base = record[[1L, "Base"]],
    verifiers = if (is.na(verifiers) || !nzchar(verifiers)) character() else
      strsplit(verifiers, " ", fixed = TRUE)[[1L]]
  )
  if (length(reused)) selection$reused <- reused
  selection
}

# Runs the manager's artifact routes in the pinned disposable Git checkout;
# reference verifiers need its tracked index, which an archive does not carry.
run_review_ready_verifiers <- function(selection, identity, prerequisites, runner, accepted_root = NULL) {
  for (path in selection$verifiers) {
    generation <- identical(path, "tools/review-ready-generated.R")
    if (generation && "generation" %in% selection$reused) {
      cat("Reused: generation (original execution in supervised plan).\n")
      next
    }
    if (!file.exists(file.path(identity$root, path))) {
      stop("Required Review-ready verifier is missing: ", path, call. = FALSE)
    }
    command <- if (endsWith(path, ".py")) unname(Sys.which("python3")) else
      prerequisites$rscript
    if (generation) review_ready_report(unit = "generation", state = "running")
    run_review_ready_step(
      paste("Artifact verification:", path),
      list(
        command = command, args = if (!is.null(accepted_root) &&
          identical(path, "tools/test-review-ready-runner.R"))
          c(file.path(accepted_root, path), identity$root) else path,
        env = c("TESTTHAT_PARALLEL=false", "TESTTHAT_CPUS=1", "MARGINPLYR_REVIEW_STAGE_REPORT=", paste0(
          "MARGINPLYR_REVIEW_BASE=", if (is.null(selection$base)) "" else selection$base
        ))
      ),
      identity$root, runner = runner
    )
    cat("Completed artifact verification: ", path, "\n", sep = "")
    if (generation) review_ready_report(unit = "generation", state = "passed")
  }
}

# Runs the selected fixed gate and returns its identity and path.
# Package paths also return source-tarball outcome and NOTE disposition.
run_review_ready_check <- function(
  repository_root,
  identity = review_ready_identity(repository_root),
  prerequisites = review_ready_prerequisites(),
  runner = review_ready_run_system,
  checker = review_ready_progress_check,
  diagnostic_root = file.path(tools::R_user_dir("marginplyr", "cache"), "review-ready-failures"),
  system = Sys.info()[["sysname"]],
  retry_identity = review_ready_retry_identity,
  preserver = preserve_review_ready_failure,
  selection = list(path = "Full", verifiers = character()),
  accepted_root = NULL
) {
  if (!is.null(selection$sha) && !identical(selection$sha, identity$sha)) {
    stop("Admitted selection does not match the checked commit.", call. = FALSE)
  }
  if (!selection$path %in% c("Full", "Repository", "Documentation")) {
    stop("Unsupported Review-ready path.", call. = FALSE)
  }
  workspace <- tempfile("marginplyr-review-ready-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  source_root <- archive_review_ready_source(identity, workspace, runner = runner)

  cat("Review-ready commit: ", identity$sha, "\n", sep = "")
  cat("Disposable source: ", source_root, "\n", sep = "")
  cat("Review-ready path: ", selection$path, "\n", sep = "")
  if (length(selection$reused)) cat("Reused units: ", paste(selection$reused, collapse = ", "),
    "; this invocation is not a new Full pass.\n", sep = "")
  review_ready_report(omitted = c(
    if (identical(selection$path, "Repository")) c("package build/check", "full suite", "snapshots"),
    if (identical(selection$path, "Documentation")) c("full suite", "snapshots")
  ))
  if (!identical(selection$path, "Repository")) {
    verify_review_ready_test_packages(source_root)
    review_ready_report(completed = "Test dependency syntax")
    cat("Completed: Test dependency syntax\n\n==> Required test Suggests\n")
    check_env <- review_ready_rcmdcheck_env(required_test_suggests(source_root), selection$path)
    cat("Completed: Required test Suggests\n")
    review_ready_report(completed = "Required test Suggests")
    check_env[["MARGINPLYR_REVIEW_PROGRESS_REPORTER"]] <- file.path(
      source_root, "tools", "review-ready-progress-reporter.R"
    )
  } else {
    cat("Repository omits package build and R CMD check (full suite and snapshots).\n")
  }
  steps <- review_ready_source_steps(prerequisites)
  for (label in names(steps)) {
    run_review_ready_step(label, steps[[label]], source_root, runner = runner)
    cat("Completed: ", label, "\n", sep = "")
  }
  run_review_ready_verifiers(selection, identity, prerequisites, runner, accepted_root)
  if (identical(selection$path, "Repository")) {
    cat("\nReview-ready Repository passed for ", identity$sha,
      "; package build/check omitted.\n", sep = "")
    return(invisible(list(identity = identity, path = "Repository")))
  }
  if (identical(selection$path, "Documentation")) {
    cat("Documentation: full suite and snapshots omitted; running selected assertions.\n")
    if ("documentation-assertions" %in% selection$reused) {
      cat("Reused: documentation-assertions (original execution in supervised plan).\n")
    } else {
      review_ready_report(unit = "documentation-assertions", state = "running")
      run_review_ready_step("Documentation assertions",
      list(command = prerequisites$rscript, args = "tools/review-ready-documentation.R",
        env = c("MARGINPLYR_REVIEW_STAGE_REPORT=", paste0("MARGINPLYR_REQUIRED_SUGGESTS=",
          check_env[["MARGINPLYR_REQUIRED_SUGGESTS"]]))),
        source_root, runner = runner)
      review_ready_report(unit = "documentation-assertions", state = "passed")
    }
  }
  unit <- if (identical(selection$path, "Full")) "build-full" else "build-documentation"
  if (unit %in% selection$reused) {
    cat("Reused: ", unit, "; package build/check not executed.\n", sep = "")
    return(invisible(list(identity = identity, path = selection$path, reused = selection$reused)))
  }
  review_ready_report(unit = unit, state = "running")
  tarball <- build_review_ready_tarball(
    source_root,
    workspace,
    prerequisites$r,
    runner = runner
  )
  cat("Completed: Build source tarball\n")
  if (nzchar(Sys.getenv("MARGINPLYR_REVIEW_STAGE_REPORT"))) {
    review_ready_report(timestamps = review_ready_timestamps(tarball, workspace))
  }
  outcome <- run_review_ready_rcmdcheck(tarball, workspace, identity,
    checker = checker, diagnostic_root = diagnostic_root, env = check_env, system = system,
    retry_identity = retry_identity, preserver = preserver, path = selection$path)
  review_ready_report(unit = unit, state = "passed")

  verdict <- if (outcome$retried) "passed-after-retry" else "passed"
  cat("Completed: R CMD check --as-cran\n")
  cat("\nReview-ready ", selection$path, " check ", verdict, " for ", identity$sha, ".\n", sep = "")
  if (length(outcome$unexpected_notes) > 0L) {
    cat(
      "Review and explain every unclassified NOTE before publishing this ",
      "commit for review.\n",
      sep = ""
    )
  }
  invisible(list(identity = identity, path = selection$path, outcome = outcome))
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
