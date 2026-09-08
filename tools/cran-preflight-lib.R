# Implementation of the non-mutating, tarball-based CRAN preflight command.
# `cran-preflight.R` is the public command; functions live here so deterministic
# policy and subprocess fixtures can exercise them without running a full check.

preflight_exit_code <- function(
  candidate_failed,
  tool_failed,
  interrupted = FALSE
) {
  if (isTRUE(interrupted)) {
    return(130L)
  }
  if (isTRUE(tool_failed)) {
    return(2L)
  }
  if (isTRUE(candidate_failed)) {
    return(1L)
  }
  0L
}

worktree_is_unchanged <- function(before, after) {
  identical(before, after)
}

new_preflight_state <- function(evidence_path) {
  state <- new.env(parent = emptyenv())
  state$started <- Sys.time()
  state$evidence_path <- normalizePath(evidence_path, mustWork = TRUE)
  state$steps <- data.frame(
    step = character(),
    status = character(),
    elapsed_seconds = numeric(),
    evidence = character(),
    detail = character(),
    stringsAsFactors = FALSE
  )
  state$candidate_failed <- FALSE
  state$tool_failed <- FALSE
  state$interrupted <- FALSE
  state$candidate_sha <- NA_character_
  state$package <- NA_character_
  state$version <- NA_character_
  state$cran_status <- NA_character_
  state$tarball <- NA_character_
  state$tarball_sha256 <- NA_character_
  state$counts <- c(
    errors = NA_integer_,
    warnings = NA_integer_,
    notes = NA_integer_
  )
  state$note_classifications <- list()
  state$tool_versions <- character()
  state$worktree_before <- NA_character_
  state$worktree_after <- NA_character_
  state
}

record_preflight_step <- function(
  state,
  step,
  status,
  elapsed,
  evidence,
  detail = ""
) {
  permitted <- c("passed", "failed", "allowed-note", "unavailable", "skipped")
  if (!(status %in% permitted)) {
    stop("Unknown preflight step status: ", status, call. = FALSE)
  }
  state$steps <- rbind(
    state$steps,
    data.frame(
      step = step,
      status = status,
      elapsed_seconds = round(as.numeric(elapsed), 3L),
      evidence = as.character(evidence),
      detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  )
  invisible(status)
}

single_line <- function(value) {
  value <- if (length(value) == 0L || is.na(value[[1L]])) "" else value[[1L]]
  gsub("[\r\n\t]+", " ", as.character(value))
}

display_or <- function(value, missing) {
  rendered <- single_line(value)
  if (nzchar(rendered)) rendered else missing
}

printable_worktree_status <- function(status) {
  if (is.na(status)) {
    return("<unavailable>")
  }
  if (!nzchar(status)) {
    return("<clean>")
  }
  gsub("\n", "; ", status, fixed = TRUE)
}

write_preflight_evidence <- function(state, exit_code) {
  evidence_path <- state$evidence_path
  utils::write.table(
    state$steps,
    file.path(evidence_path, "steps.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )

  tool_versions <- if (length(state$tool_versions) == 0L) {
    ""
  } else {
    paste(
      names(state$tool_versions),
      state$tool_versions,
      sep = "=",
      collapse = "; "
    )
  }
  machine <- data.frame(
    `Exit-Code` = as.character(exit_code),
    `Candidate-SHA` = single_line(state$candidate_sha),
    Package = single_line(state$package),
    Version = single_line(state$version),
    `CRAN-Status` = single_line(state$cran_status),
    Tarball = single_line(state$tarball),
    `Tarball-SHA256` = single_line(state$tarball_sha256),
    Errors = single_line(state$counts[["errors"]]),
    Warnings = single_line(state$counts[["warnings"]]),
    Notes = single_line(state$counts[["notes"]]),
    `Tool-Versions` = single_line(tool_versions),
    `Worktree-Before` = single_line(printable_worktree_status(
      state$worktree_before
    )),
    `Worktree-After` = single_line(printable_worktree_status(
      state$worktree_after
    )),
    `Evidence-Path` = evidence_path,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  write.dcf(machine, file.path(evidence_path, "results.dcf"))

  notes <- if (length(state$note_classifications) == 0L) {
    "- None recorded."
  } else {
    unlist(lapply(seq_along(state$note_classifications), function(index) {
      note <- state$note_classifications[[index]]
      c(
        sprintf("- NOTE %d: `%s`", index, note$status),
        sprintf("  - Policy: %s", single_line(note$policy)),
        sprintf("  - Rationale: %s", single_line(note$rationale)),
        "",
        "  ```text",
        paste0("  ", strsplit(note$normalized, "\n", fixed = TRUE)[[1L]]),
        "  ```"
      )
    }))
  }

  steps <- if (nrow(state$steps) == 0L) {
    "| _No steps recorded._ |  |  |  |"
  } else {
    c(
      "| Step | Status | Seconds | Evidence |",
      "|---|---:|---:|---|",
      apply(state$steps, 1L, function(row) {
        sprintf(
          "| %s | %s | %s | `%s` |",
          row[["step"]], row[["status"]], row[["elapsed_seconds"]],
          row[["evidence"]]
        )
      })
    )
  }
  if (nrow(state$steps) == 0L) {
    steps <- c(
      "| Step | Status | Seconds | Evidence |",
      "|---|---:|---:|---|",
      steps
    )
  }

  summary <- c(
    "# marginplyr CRAN preflight",
    "",
    sprintf("**Exit code: %d**", exit_code),
    "",
    sprintf("- Candidate SHA: `%s`", single_line(state$candidate_sha)),
    sprintf(
      "- Package: `%s` `%s` (`%s`)",
      single_line(state$package), single_line(state$version),
      single_line(state$cran_status)
    ),
    sprintf("- Tarball: `%s`", display_or(state$tarball, "not produced")),
    sprintf(
      "- SHA-256: `%s`",
      display_or(state$tarball_sha256, "not calculated")
    ),
    sprintf(
      "- Check counts: %s ERROR(s), %s WARNING(s), %s NOTE(s)",
      display_or(state$counts[["errors"]], "not run"),
      display_or(state$counts[["warnings"]], "not run"),
      display_or(state$counts[["notes"]], "not run")
    ),
    sprintf("- Tool versions: %s", single_line(tool_versions)),
    sprintf(
      "- Worktree before: `%s`",
      single_line(printable_worktree_status(state$worktree_before))
    ),
    sprintf(
      "- Worktree after: `%s`",
      single_line(printable_worktree_status(state$worktree_after))
    ),
    sprintf("- Evidence: `%s`", evidence_path),
    "",
    "## Steps",
    "",
    steps,
    "",
    "## NOTE classifications",
    "",
    notes,
    "",
    "## Pending release stages",
    "",
    "- Required CI for this exact SHA.",
    "- Semantic CRAN review for the frozen candidate.",
    "- Targeted R-hub v2 and exact-tarball win-builder checks.",
    "- Human release issue, comments, submission, and publication actions."
  )
  writeLines(summary, file.path(evidence_path, "summary.md"))
  invisible(summary)
}

read_description <- function(path) {
  description <- read.dcf(path)
  if (nrow(description) != 1L) {
    stop("DESCRIPTION must contain exactly one DCF record.", call. = FALSE)
  }
  description
}

description_value <- function(description, field, required = TRUE) {
  if (!(field %in% colnames(description))) {
    if (required) {
      stop("DESCRIPTION is missing `", field, "`.", call. = FALSE)
    }
    return(NA_character_)
  }
  trimws(description[[1L, field]])
}

dependency_requirements <- function(field) {
  if (length(field) == 0L || is.na(field) || !nzchar(field)) {
    return(data.frame(
      package = character(),
      operator = character(),
      version = character()
    ))
  }
  entries <- trimws(strsplit(gsub("\n", " ", field), ",", fixed = TRUE)[[1L]])
  pattern <- paste0(
    "^([A-Za-z][A-Za-z0-9.]*)",
    "(?:[[:space:]]*\\((>=|<=|==|>|<)[[:space:]]*([^()]+)\\))?$"
  )
  matched <- regexec(pattern, entries, perl = TRUE)
  parts <- regmatches(entries, matched)
  if (any(lengths(parts) == 0L)) {
    stop(
      "Cannot parse dependency requirement(s): ",
      paste(entries[lengths(parts) == 0L], collapse = ", "),
      call. = FALSE
    )
  }
  data.frame(
    package = vapply(parts, `[[`, character(1), 2L),
    operator = vapply(parts, function(part) {
      if (length(part) >= 3L) part[[3L]] else ""
    }, character(1)),
    version = vapply(parts, function(part) {
      if (length(part) >= 4L) trimws(part[[4L]]) else ""
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

version_satisfies <- function(installed, operator, required) {
  if (!nzchar(operator)) {
    return(TRUE)
  }
  installed <- package_version(installed)
  required <- package_version(required)
  switch(
    operator,
    `>=` = installed >= required,
    `<=` = installed <= required,
    `==` = installed == required,
    `>` = installed > required,
    `<` = installed < required,
    FALSE
  )
}

check_package_requirements <- function(requirements) {
  if (nrow(requirements) == 0L) {
    return(data.frame(
      package = character(), required = character(), installed = character(),
      available = logical(), stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(seq_len(nrow(requirements)), function(index) {
    package <- requirements$package[[index]]
    available <- requireNamespace(package, quietly = TRUE)
    installed <- if (available) {
      as.character(utils::packageVersion(package))
    } else {
      ""
    }
    required <- paste(
      requirements$operator[[index]],
      requirements$version[[index]]
    )
    required <- trimws(required)
    satisfies <- available && version_satisfies(
      installed,
      requirements$operator[[index]],
      requirements$version[[index]]
    )
    data.frame(
      package = package,
      required = required,
      installed = installed,
      available = satisfies,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

sha256_file <- function(path) {
  sha256sum <- get0("sha256sum", envir = asNamespace("tools"), inherits = FALSE)
  if (is.null(sha256sum)) {
    stop("This R installation cannot calculate SHA-256.", call. = FALSE)
  }
  unname(as.character(sha256sum(path)))
}

verify_candidate_tarball <- function(
  tarball,
  expected_package,
  expected_version,
  unpack_dir
) {
  contents <- utils::untar(tarball, list = TRUE)
  descriptions <- contents[grepl("^[^/]+/DESCRIPTION$", contents)]
  if (length(descriptions) != 1L) {
    stop(
      "The source tarball must contain exactly one package DESCRIPTION.",
      call. = FALSE
    )
  }
  top <- sub("/DESCRIPTION$", "", descriptions[[1L]])
  expected_top <- expected_package
  if (!identical(top, expected_top)) {
    stop(
      "The source tarball directory is `", top,
      "`, expected `", expected_top, "`.",
      call. = FALSE
    )
  }
  dir.create(unpack_dir, recursive = TRUE, showWarnings = FALSE)
  utils::untar(tarball, exdir = unpack_dir)
  package_path <- file.path(unpack_dir, top)
  description <- read_description(file.path(package_path, "DESCRIPTION"))
  package <- description_value(description, "Package")
  version <- description_value(description, "Version")
  identity_matches <- identical(package, expected_package) &&
    identical(version, expected_version)
  if (!identity_matches) {
    stop(
      "The tarball DESCRIPTION does not match the candidate identity.",
      call. = FALSE
    )
  }
  expected_name <- paste0(expected_package, "_", expected_version, ".tar.gz")
  if (!identical(basename(tarball), expected_name)) {
    stop(
      "The source tarball name does not match its DESCRIPTION.",
      call. = FALSE
    )
  }
  list(
    package = package,
    version = version,
    package_path = normalizePath(package_path, mustWork = TRUE),
    sha256 = sha256_file(tarball)
  )
}

cran_comments_problems <- function(path, counts, allowed_notes) {
  if (!file.exists(path)) {
    return("cran-comments.md does not exist.")
  }
  lines <- readLines(path, warn = FALSE)
  count_pattern <- paste0(
    "^([0-9]+) errors? \\| ([0-9]+) warnings? \\| ([0-9]+) notes?$"
  )
  positions <- grep(count_pattern, lines, ignore.case = TRUE)
  problems <- character()
  if (length(positions) != 1L) {
    problems <- c(
      problems,
      "cran-comments.md must contain exactly one leading check-count line."
    )
  } else {
    matched <- regmatches(lines[[positions]], regexec(
      count_pattern,
      lines[[positions]],
      ignore.case = TRUE
    ))[[1L]]
    recorded <- as.integer(matched[2L:4L])
    names(recorded) <- c("errors", "warnings", "notes")
    if (!identical(unname(recorded), as.integer(counts))) {
      problems <- c(
        problems,
        sprintf(
          paste0(
            "cran-comments.md records %d/%d/%d ERROR/WARNING/NOTE, ",
            "but the check produced %d/%d/%d."
          ),
          recorded[[1L]], recorded[[2L]], recorded[[3L]],
          counts[["errors"]], counts[["warnings"]], counts[["notes"]]
        )
      )
    }
  }

  for (note in allowed_notes) {
    marker <- note$marker
    if (is.na(marker) || !nzchar(marker)) {
      next
    }
    if (!any(grepl(marker, lines, fixed = TRUE))) {
      problems <- c(
        problems,
        paste0(
          "cran-comments.md does not explain allowed NOTE marker `",
          marker,
          "`."
        )
      )
    }
  }
  problems
}

path_is_inside <- function(path, directory) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  identical(path, directory) || startsWith(path, paste0(directory, "/"))
}

parse_preflight_args <- function(args) {
  output <- NULL
  while (length(args) > 0L) {
    argument <- args[[1L]]
    args <- args[-1L]
    if (identical(argument, "--output")) {
      if (length(args) == 0L) {
        stop("`--output` requires a directory path.", call. = FALSE)
      }
      output <- args[[1L]]
      args <- args[-1L]
    } else if (startsWith(argument, "--output=")) {
      output <- sub("^--output=", "", argument)
    } else {
      stop("Unknown argument: ", argument, call. = FALSE)
    }
  }
  if (!is.null(output) && !nzchar(output)) {
    stop("`--output` requires a non-empty directory path.", call. = FALSE)
  }
  list(output = output)
}

run_system <- function(
  command,
  args = character(),
  stdout = TRUE,
  env = character()
) {
  quoted <- vapply(args, shQuote, character(1))
  status <- suppressWarnings(system2(
    command,
    quoted,
    stdout = stdout,
    stderr = stdout,
    env = env
  ))
  if (is.character(status)) {
    code <- attr(status, "status")
    if (is.null(code)) {
      code <- 0L
    }
    return(list(status = as.integer(code), output = status))
  }
  list(status = as.integer(status), output = character())
}

git_output <- function(args) {
  result <- run_system("git", args)
  if (result$status != 0L) {
    stop(paste(result$output, collapse = "\n"), call. = FALSE)
  }
  result$output
}

git_status_record <- function() {
  paste(
    git_output(c("status", "--porcelain=v1", "--untracked-files=all")),
    collapse = "\n"
  )
}

default_evidence_path <- function() {
  root <- file.path(tools::R_user_dir("marginplyr", "cache"), "cran-preflight")
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
  file.path(root, paste0(stamp, "-", Sys.getpid()))
}

create_evidence_path <- function(requested, repository_root) {
  path <- if (is.null(requested)) default_evidence_path() else requested
  path <- path.expand(path)
  if (path_is_inside(path, repository_root)) {
    stop(
      "The evidence directory must be outside the repository.",
      call. = FALSE
    )
  }
  if (file.exists(path) || dir.exists(path)) {
    stop("The evidence path already exists: ", path, call. = FALSE)
  }
  if (!dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create the evidence directory: ", path, call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

write_text_record <- function(value, path) {
  if (is.na(value) || !nzchar(value)) {
    writeLines(character(), path)
  } else {
    writeLines(strsplit(value, "\n", fixed = TRUE)[[1L]], path)
  }
}

checker_versions <- function(packages) {
  versions <- vapply(packages, function(package) {
    if (requireNamespace(package, quietly = TRUE)) {
      as.character(utils::packageVersion(package))
    } else {
      "MISSING"
    }
  }, character(1))
  c(
    R = R.version.string,
    platform = R.version$platform,
    versions
  )
}

preflight_prerequisites <- function(description, package, version) {
  suggests <- dependency_requirements(description_value(
    description,
    "Suggests",
    required = FALSE
  ))
  preflight <- dependency_requirements(description_value(
    description,
    "Config/Needs/preflight",
    required = FALSE
  ))
  if (nrow(preflight) == 0L) {
    stop(
      "DESCRIPTION declares no `Config/Needs/preflight` packages.",
      call. = FALSE
    )
  }
  requirements <- rbind(suggests, preflight)
  requirements <- requirements[
    !duplicated(requirements$package),
    ,
    drop = FALSE
  ]
  package_requirements <- check_package_requirements(requirements)

  self_available <- requireNamespace(package, quietly = TRUE)
  self_version <- if (self_available) {
    as.character(utils::packageVersion(package))
  } else {
    ""
  }
  self_ok <- self_available && identical(self_version, version)

  executables <- Sys.which(c("quarto", "pdflatex", "makeindex"))
  quarto <- if (nzchar(executables[["quarto"]])) {
    run_system(executables[["quarto"]], "--version")
  } else {
    list(status = 1L, output = character())
  }
  pandoc <- if (nzchar(executables[["quarto"]])) {
    run_system(executables[["quarto"]], c("pandoc", "--version"))
  } else {
    list(status = 1L, output = character())
  }

  missing <- package_requirements$package[!package_requirements$available]
  problems <- character()
  if (length(missing) > 0L) {
    problems <- c(
      problems,
      paste0(
        "Missing or too-old R packages: ",
        paste(missing, collapse = ", "),
        "."
      )
    )
  }
  if (!self_ok) {
    problems <- c(
      problems,
      paste0(
        "Installed `", package, "` must be version ", version,
        " so Quarto can build this candidate's vignettes."
      )
    )
  }
  missing_executables <- names(executables)[!nzchar(executables)]
  if (length(missing_executables) > 0L) {
    problems <- c(
      problems,
      paste0(
        "Missing command-line prerequisites: ",
        paste(missing_executables, collapse = ", "),
        "."
      )
    )
  }
  if (quarto$status != 0L || pandoc$status != 0L) {
    problems <- c(
      problems,
      "Quarto or its bundled Pandoc could not report a version."
    )
  }

  quarto_version <- if (length(quarto$output) > 0L) quarto$output[[1L]] else ""
  pandoc_version <- if (length(pandoc$output) > 0L) pandoc$output[[1L]] else ""

  list(
    problems = problems,
    package_requirements = package_requirements,
    self_version = if (self_available) self_version else "MISSING",
    quarto_version = quarto_version,
    pandoc_version = pandoc_version,
    preflight_packages = preflight$package,
    suggest_packages = suggests$package
  )
}

build_candidate_tarball <- function(
  repository_root,
  evidence_path,
  package,
  version
) {
  workspace <- file.path(evidence_path, "build-workspace")
  source <- file.path(workspace, "source")
  dir.create(source, recursive = TRUE)
  archive <- file.path(workspace, "candidate.tar")
  archive_log <- file.path(evidence_path, "git-archive.log")
  original_directory <- setwd(repository_root)
  on.exit(setwd(original_directory), add = TRUE)
  archive_result <- run_system(
    "git",
    c("archive", "--format=tar", paste0("--output=", archive), "HEAD"),
    stdout = archive_log
  )
  if (archive_result$status != 0L) {
    stop(
      "git archive could not create the disposable source tree.",
      call. = FALSE
    )
  }
  setwd(original_directory)
  utils::untar(archive, exdir = source)

  build_log <- file.path(evidence_path, "build.log")
  setwd(workspace)
  build_result <- run_system(
    file.path(R.home("bin"), "R"),
    c("CMD", "build", source),
    stdout = build_log
  )
  if (build_result$status != 0L) {
    stop("R CMD build failed; see build.log.", call. = FALSE)
  }
  tarballs <- list.files(
    workspace,
    pattern = "[.]tar[.]gz$",
    full.names = TRUE
  )
  if (length(tarballs) != 1L) {
    stop(
      sprintf(
        "R CMD build produced %d source tarballs; expected exactly one.",
        length(tarballs)
      ),
      call. = FALSE
    )
  }
  expected <- paste0(package, "_", version, ".tar.gz")
  destination <- file.path(evidence_path, expected)
  if (!file.rename(tarballs[[1L]], destination)) {
    stop(
      "Cannot retain the candidate tarball in the evidence bundle.",
      call. = FALSE
    )
  }
  destination
}

capture_console <- function(path, code) {
  connection <- file(path, open = "wt")
  sink(connection, type = "output")
  sink(connection, type = "message")
  on.exit({
    sink(type = "message")
    sink(type = "output")
    close(connection)
  }, add = TRUE)
  force(code)
}

run_spelling_check <- function(package_path, evidence_path) {
  log <- file.path(evidence_path, "spelling.log")
  findings <- capture_console(
    log,
    spelling::spell_check_package(
      package_path,
      vignettes = TRUE,
      use_wordlist = TRUE
    )
  )
  table_path <- file.path(evidence_path, "spelling.tsv")
  utils::write.table(
    as.data.frame(findings),
    table_path,
    sep = "\t",
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  list(findings = findings, evidence = table_path)
}

run_checktor <- function(repository_root, tarball, evidence_path) {
  report <- file.path(evidence_path, "checktor-report.md")
  log <- file.path(evidence_path, "checktor.log")
  old <- setwd(repository_root)
  on.exit(setwd(old), add = TRUE)
  result <- run_system(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(".github", "scripts", "check-cran-readiness.R"),
      tarball,
      report
    ),
    stdout = log
  )
  report_text <- if (file.exists(report)) {
    paste(readLines(report, warn = FALSE), collapse = "\n")
  } else {
    ""
  }
  list(
    status = result$status,
    tooling_failure = grepl(
      "checktor could not complete:",
      report_text,
      fixed = TRUE
    ) ||
      !file.exists(report),
    report = report,
    log = log
  )
}

run_rcmdcheck <- function(tarball, evidence_path) {
  check_dir <- file.path(evidence_path, "check")
  dir.create(check_dir)
  console <- file.path(evidence_path, "rcmdcheck-console.log")
  result <- capture_console(
    console,
    rcmdcheck::rcmdcheck(
      tarball,
      args = "--as-cran",
      build_args = NULL,
      check_dir = check_dir,
      error_on = "never",
      env = c(
        `_R_CHECK_CRAN_INCOMING_REMOTE_` = "true",
        `_R_CHECK_FORCE_SUGGESTS_` = "true",
        `_R_CHECK_DEPENDS_ONLY_` = "false"
      )
    )
  )
  list(result = result, check_dir = check_dir, console = console)
}

check_reports_url_problem <- function(result) {
  conditions <- c(result$errors, result$warnings, result$notes)
  if (length(conditions) == 0L) {
    return(FALSE)
  }
  patterns <- c(
    "checking URLs",
    "invalid URL",
    "URL.*(?:unavailable|redirect|status|timeout|resolve)",
    "Found the following.*URL"
  )
  any(vapply(patterns, function(pattern) {
    any(grepl(pattern, conditions, ignore.case = TRUE, perl = TRUE))
  }, logical(1)))
}

character_url_results <- function(results) {
  data <- as.data.frame(results)
  data[] <- lapply(data, function(column) {
    if (is.list(column)) {
      vapply(column, paste, character(1), collapse = "; ")
    } else {
      as.character(column)
    }
  })
  data
}

classify_url_results <- function(results) {
  data <- character_url_results(results)
  if (nrow(data) == 0L) {
    return(character())
  }
  vapply(seq_len(nrow(data)), function(index) {
    value <- function(name) {
      if (name %in% names(data)) data[[name]][[index]] else ""
    }
    status <- suppressWarnings(as.integer(value("Status")))
    text <- paste(value("Status"), value("Message"), value("New"))
    if (nzchar(value("New")) || identical(status, 301L)) {
      return("failed")
    }
    if (
      !is.na(status) && status >= 400L && status < 500L &&
        !(status %in% c(408L, 429L))
    ) {
      return("failed")
    }
    if (grepl(
      "malformed|invalid|contains spaces|not in canonical form",
      text,
      ignore.case = TRUE
    )) {
      return("failed")
    }
    service_status <- !is.na(status) &&
      (status >= 500L || status %in% c(408L, 429L))
    unavailable_message <- grepl(
      "timeout|timed out|rate.?limit|could not resolve|name resolution|DNS",
      text,
      ignore.case = TRUE
    )
    if (service_status || unavailable_message) {
      return("unavailable")
    }
    "unavailable"
  }, character(1))
}

run_url_diagnostic <- function(package_path, evidence_path, delay = 5) {
  Sys.sleep(delay)
  log <- file.path(evidence_path, "url-diagnostic.log")
  results <- capture_console(log, {
    setTimeLimit(elapsed = 120, transient = TRUE)
    on.exit(
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE),
      add = TRUE
    )
    urlchecker::url_check(
      package_path,
      parallel = TRUE,
      progress = FALSE,
      fail = FALSE
    )
  })
  table <- character_url_results(results)
  table_path <- file.path(evidence_path, "url-diagnostic.tsv")
  utils::write.table(
    table,
    table_path,
    sep = "\t",
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  list(
    classifications = classify_url_results(results),
    evidence = table_path,
    log = log
  )
}

write_note_results <- function(classifications, evidence_path) {
  data <- if (length(classifications) == 0L) {
    data.frame(
      status = character(), policy = character(), rationale = character(),
      marker = character(), normalized = character(), stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, lapply(classifications, function(note) {
      data.frame(
        status = note$status,
        policy = single_line(note$policy),
        rationale = single_line(note$rationale),
        marker = single_line(note$marker),
        normalized = single_line(note$normalized),
        stringsAsFactors = FALSE
      )
    }))
  }
  path <- file.path(evidence_path, "notes.tsv")
  utils::write.table(
    data,
    path,
    sep = "\t",
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  path
}

record_problem <- function(state, kind) {
  if (identical(kind, "candidate")) {
    state$candidate_failed <- TRUE
  } else {
    state$tool_failed <- TRUE
  }
  invisible(kind)
}

run_preflight_pipeline <- function(state, repository_root, description) {
  evidence <- state$evidence_path
  transient_directories <- file.path(
    evidence,
    c("build-workspace", "candidate-source")
  )
  on.exit(unlink(transient_directories, recursive = TRUE), add = TRUE)

  started <- proc.time()[["elapsed"]]
  prerequisites <- preflight_prerequisites(
    description,
    state$package,
    state$version
  )
  versions <- checker_versions(unique(c(
    prerequisites$preflight_packages,
    "spelling"
  )))
  state$tool_versions <- c(
    versions,
    Quarto = prerequisites$quarto_version,
    Pandoc = prerequisites$pandoc_version,
    TeX = unname(Sys.which("pdflatex"))
  )
  prerequisite_table <- file.path(evidence, "prerequisites.tsv")
  utils::write.table(
    prerequisites$package_requirements,
    prerequisite_table,
    sep = "\t",
    row.names = FALSE,
    quote = TRUE,
    na = ""
  )
  if (length(prerequisites$problems) > 0L) {
    record_preflight_step(
      state,
      "prerequisites",
      "unavailable",
      proc.time()[["elapsed"]] - started,
      prerequisite_table,
      paste(prerequisites$problems, collapse = " ")
    )
    cat(paste(prerequisites$problems, collapse = "\n"), "\n", file = stderr())
    cat(
      "Preflight-tool hint: install Config/Needs/preflight packages: ",
      paste(prerequisites$preflight_packages, collapse = ", "), ".\n",
      file = stderr(),
      sep = ""
    )
    cat(
      "Full-Suggests hint: install every declared Suggest at its recorded ",
      "minimum version. Preflight installs nothing.\n",
      file = stderr(),
      sep = ""
    )
    record_problem(state, "tool")
    return(invisible(NULL))
  }
  record_preflight_step(
    state,
    "prerequisites",
    "passed",
    proc.time()[["elapsed"]] - started,
    prerequisite_table,
    "All declared tools, Suggests, Quarto, Pandoc, and TeX are available."
  )

  started <- proc.time()[["elapsed"]]
  tarball <- tryCatch(
    build_candidate_tarball(
      repository_root,
      evidence,
      state$package,
      state$version
    ),
    error = function(cnd) cnd
  )
  if (inherits(tarball, "condition")) {
    record_preflight_step(
      state,
      "build",
      "failed",
      proc.time()[["elapsed"]] - started,
      file.path(evidence, "build.log"),
      conditionMessage(tarball)
    )
    record_problem(state, "candidate")
    return(invisible(NULL))
  }
  record_preflight_step(
    state,
    "build",
    "passed",
    proc.time()[["elapsed"]] - started,
    tarball,
    "Built exactly one source tarball from git archive HEAD."
  )

  started <- proc.time()[["elapsed"]]
  unpacked <- file.path(evidence, "candidate-source")
  identity <- tryCatch(
    verify_candidate_tarball(
      tarball,
      state$package,
      state$version,
      unpacked
    ),
    error = function(cnd) cnd
  )
  if (inherits(identity, "condition")) {
    record_preflight_step(
      state,
      "tarball-identity",
      "failed",
      proc.time()[["elapsed"]] - started,
      tarball,
      conditionMessage(identity)
    )
    record_problem(state, "tool")
    return(invisible(NULL))
  }
  state$tarball <- basename(tarball)
  state$tarball_sha256 <- identity$sha256
  manifest <- file.path(evidence, "SHA256SUMS")
  writeLines(paste(identity$sha256, basename(tarball)), manifest)
  record_preflight_step(
    state,
    "tarball-identity",
    "passed",
    proc.time()[["elapsed"]] - started,
    manifest,
    "Tarball name, directory, DESCRIPTION, and SHA-256 agree."
  )

  started <- proc.time()[["elapsed"]]
  spelling <- tryCatch(
    run_spelling_check(identity$package_path, evidence),
    error = function(cnd) cnd
  )
  if (inherits(spelling, "condition")) {
    record_preflight_step(
      state,
      "spelling",
      "unavailable",
      proc.time()[["elapsed"]] - started,
      file.path(evidence, "spelling.log"),
      conditionMessage(spelling)
    )
    record_problem(state, "tool")
  } else {
    spelling_failed <- nrow(as.data.frame(spelling$findings)) > 0L
    record_preflight_step(
      state,
      "spelling",
      if (spelling_failed) "failed" else "passed",
      proc.time()[["elapsed"]] - started,
      spelling$evidence,
      if (spelling_failed) {
        "Unrecognized words were found."
      } else {
        "No unrecognized words."
      }
    )
    if (spelling_failed) {
      record_problem(state, "candidate")
    }
  }

  started <- proc.time()[["elapsed"]]
  checktor <- tryCatch(
    run_checktor(repository_root, tarball, evidence),
    error = function(cnd) cnd
  )
  if (inherits(checktor, "condition")) {
    record_preflight_step(
      state,
      "checktor",
      "unavailable",
      proc.time()[["elapsed"]] - started,
      file.path(evidence, "checktor.log"),
      conditionMessage(checktor)
    )
    record_problem(state, "tool")
  } else if (checktor$status != 0L) {
    kind <- if (checktor$tooling_failure) "tool" else "candidate"
    record_preflight_step(
      state,
      "checktor",
      if (identical(kind, "tool")) "unavailable" else "failed",
      proc.time()[["elapsed"]] - started,
      checktor$report,
      "The shared checktor gate did not pass."
    )
    record_problem(state, kind)
  } else {
    record_preflight_step(
      state,
      "checktor",
      "passed",
      proc.time()[["elapsed"]] - started,
      checktor$report,
      "The reviewed checktor baseline matched exactly."
    )
  }

  started <- proc.time()[["elapsed"]]
  check <- tryCatch(
    run_rcmdcheck(tarball, evidence),
    error = function(cnd) cnd
  )
  if (inherits(check, "condition")) {
    record_preflight_step(
      state,
      "R-CMD-check",
      "unavailable",
      proc.time()[["elapsed"]] - started,
      file.path(evidence, "rcmdcheck-console.log"),
      conditionMessage(check)
    )
    record_problem(state, "tool")
    return(invisible(NULL))
  }

  result <- check$result
  state$counts <- c(
    errors = length(result$errors),
    warnings = length(result$warnings),
    notes = length(result$notes)
  )
  classifications <- lapply(
    result$notes,
    classify_cran_note,
    cran_status = state$cran_status
  )
  state$note_classifications <- classifications
  note_path <- write_note_results(classifications, evidence)
  unknown_notes <- vapply(
    classifications,
    function(note) identical(note$status, "unknown-note"),
    logical(1)
  )
  has_failures <- state$counts[["errors"]] > 0L ||
    state$counts[["warnings"]] > 0L || any(unknown_notes)
  has_allowed <- length(classifications) > 0L && !any(unknown_notes)
  check_status <- if (has_failures) {
    "failed"
  } else if (has_allowed) {
    "allowed-note"
  } else {
    "passed"
  }
  record_preflight_step(
    state,
    "R-CMD-check",
    check_status,
    proc.time()[["elapsed"]] - started,
    check$check_dir,
    sprintf(
      "%d ERROR(s), %d WARNING(s), %d NOTE(s); classifications: %s.",
      state$counts[["errors"]], state$counts[["warnings"]],
      state$counts[["notes"]], note_path
    )
  )
  if (has_failures) {
    record_problem(state, "candidate")
  }

  started <- proc.time()[["elapsed"]]
  if (check_reports_url_problem(result)) {
    diagnostic <- tryCatch(
      run_url_diagnostic(identity$package_path, evidence),
      error = function(cnd) cnd
    )
    if (inherits(diagnostic, "condition")) {
      record_preflight_step(
        state,
        "URL-diagnostic",
        "unavailable",
        proc.time()[["elapsed"]] - started,
        file.path(evidence, "url-diagnostic.log"),
        conditionMessage(diagnostic)
      )
      record_problem(state, "tool")
    } else {
      statuses <- diagnostic$classifications
      status <- if (any(statuses == "unavailable")) {
        "unavailable"
      } else if (any(statuses == "failed")) {
        "failed"
      } else {
        "passed"
      }
      record_preflight_step(
        state,
        "URL-diagnostic",
        status,
        proc.time()[["elapsed"]] - started,
        diagnostic$evidence,
        "One bounded read-only retry followed the check's URL finding."
      )
      if (identical(status, "unavailable")) {
        record_problem(state, "tool")
      } else if (identical(status, "failed")) {
        record_problem(state, "candidate")
      }
    }
  } else {
    record_preflight_step(
      state,
      "URL-diagnostic",
      "skipped",
      proc.time()[["elapsed"]] - started,
      "",
      "R CMD check reported no URL problem."
    )
  }

  started <- proc.time()[["elapsed"]]
  allowed_notes <- Filter(
    function(note) identical(note$status, "allowed-note"),
    classifications
  )
  comments_problems <- cran_comments_problems(
    file.path(repository_root, "cran-comments.md"),
    state$counts,
    allowed_notes
  )
  comments_evidence <- file.path(evidence, "cran-comments-result.txt")
  writeLines(
    if (length(comments_problems) == 0L) "Passed." else comments_problems,
    comments_evidence
  )
  record_preflight_step(
    state,
    "cran-comments",
    if (length(comments_problems) == 0L) "passed" else "failed",
    proc.time()[["elapsed"]] - started,
    comments_evidence,
    if (length(comments_problems) == 0L) {
      "Counts and allowed-NOTE explanations match."
    } else {
      paste(comments_problems, collapse = " ")
    }
  )
  if (length(comments_problems) > 0L) {
    record_problem(state, "candidate")
  }
  invisible(NULL)
}

cran_preflight_cli <- function(args, expected_root) {
  parsed <- tryCatch(parse_preflight_args(args), error = function(cnd) cnd)
  if (inherits(parsed, "condition")) {
    cat(conditionMessage(parsed), "\n", file = stderr())
    return(2L)
  }

  expected_root <- normalizePath(expected_root, mustWork = TRUE)
  actual_root <- tryCatch(
    normalizePath(
      git_output(c("rev-parse", "--show-toplevel"))[[1L]],
      mustWork = TRUE
    ),
    error = function(cnd) cnd
  )
  current_root <- normalizePath(getwd(), mustWork = TRUE)
  root_matches <- !inherits(actual_root, "condition") &&
    identical(actual_root, expected_root) &&
    identical(current_root, expected_root)
  if (!root_matches) {
    cat(
      paste0(
        "Run `Rscript tools/cran-preflight.R` from the marginplyr ",
        "repository root.\n"
      ),
      file = stderr()
    )
    return(2L)
  }

  description <- tryCatch(
    read_description(file.path(expected_root, "DESCRIPTION")),
    error = function(cnd) cnd
  )
  is_marginplyr <- !inherits(description, "condition") &&
    identical(description_value(description, "Package"), "marginplyr")
  if (!is_marginplyr) {
    cat(
      "The current directory is not the marginplyr repository root.\n",
      file = stderr()
    )
    return(2L)
  }

  evidence <- tryCatch(
    create_evidence_path(parsed$output, expected_root),
    error = function(cnd) cnd
  )
  if (inherits(evidence, "condition")) {
    cat(conditionMessage(evidence), "\n", file = stderr())
    return(2L)
  }
  state <- new_preflight_state(evidence)
  state$package <- description_value(description, "Package")
  state$version <- description_value(description, "Version")
  state$cran_status <- description_value(
    description,
    "Config/marginplyr/cran-status"
  )
  state$candidate_sha <- tryCatch(
    git_output(c("rev-parse", "HEAD"))[[1L]],
    error = function(cnd) NA_character_
  )
  state$worktree_before <- tryCatch(
    git_status_record(),
    error = function(cnd) NA_character_
  )
  write_text_record(
    state$worktree_before,
    file.path(evidence, "worktree-before.txt")
  )

  if (is.na(state$worktree_before)) {
    record_preflight_step(
      state, "worktree-before", "unavailable", 0,
      file.path(evidence, "worktree-before.txt"),
      "git status could not be recorded."
    )
    state$tool_failed <- TRUE
  } else if (nzchar(state$worktree_before)) {
    record_preflight_step(
      state, "worktree-before", "failed", 0,
      file.path(evidence, "worktree-before.txt"),
      paste0(
        "The release candidate has tracked, index, or untracked non-ignored ",
        "changes."
      )
    )
    state$candidate_failed <- TRUE
  } else {
    record_preflight_step(
      state, "worktree-before", "passed", 0,
      file.path(evidence, "worktree-before.txt"),
      "The candidate is clean."
    )
  }

  if (!state$candidate_failed && !state$tool_failed) {
    tryCatch(
      run_preflight_pipeline(state, expected_root, description),
      interrupt = function(cnd) {
        state$interrupted <- TRUE
        record_preflight_step(
          state,
          "interruption",
          "unavailable",
          difftime(Sys.time(), state$started, units = "secs"),
          evidence,
          "The preflight was interrupted."
        )
      },
      error = function(cnd) {
        state$tool_failed <- TRUE
        record_preflight_step(
          state,
          "preflight",
          "unavailable",
          difftime(Sys.time(), state$started, units = "secs"),
          evidence,
          conditionMessage(cnd)
        )
      }
    )
  }

  after_started <- proc.time()[["elapsed"]]
  state$worktree_after <- tryCatch(
    git_status_record(),
    error = function(cnd) NA_character_
  )
  write_text_record(
    state$worktree_after,
    file.path(evidence, "worktree-after.txt")
  )
  unchanged <- !is.na(state$worktree_after) && worktree_is_unchanged(
    state$worktree_before,
    state$worktree_after
  )
  record_preflight_step(
    state,
    "worktree-after",
    if (unchanged) "passed" else "failed",
    proc.time()[["elapsed"]] - after_started,
    file.path(evidence, "worktree-after.txt"),
    if (unchanged) {
      "The byte-for-byte git status record is unchanged."
    } else {
      paste0(
        "The repository status changed while preflight ran; this is a tool ",
        "failure."
      )
    }
  )
  if (!unchanged) {
    state$tool_failed <- TRUE
  }

  exit_code <- preflight_exit_code(
    state$candidate_failed,
    state$tool_failed,
    state$interrupted
  )
  evidence_result <- tryCatch(
    write_preflight_evidence(state, exit_code),
    error = function(cnd) cnd
  )
  if (inherits(evidence_result, "condition")) {
    cat(
      "Could not finish the evidence bundle: ",
      conditionMessage(evidence_result),
      "\n",
      file = stderr(),
      sep = ""
    )
    exit_code <- if (state$interrupted) 130L else 2L
  }
  cat("CRAN preflight evidence: ", evidence, "\n", sep = "")
  exit_code
}
