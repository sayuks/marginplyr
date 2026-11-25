# Shared by `tools/submit-win-builder.R` and `tools/submit-cran.R`, which each
# send one retained source tarball through a web form.

stop_submission <- function(..., status = 1L) {
  stop(structure(
    class = c("marginplyr_submission_error", "error", "condition"),
    list(message = paste0(...), call = NULL, status = status)
  ))
}

# Runs curl without a shell, so no argument is parsed or expanded by one.
run_submission_curl <- function(args, directory) {
  result <- tryCatch(
    processx::run("curl", args, wd = directory, error_on_status = FALSE),
    error = function(error) list(status = NA_integer_, stdout = "",
                                 stderr = conditionMessage(error))
  )
  result[c("status", "stdout", "stderr")]
}

# Answers the options of one command line: `--send`, an optional
# `--ca-bundle`, and a value for each name in `required`.
parse_submission_args <- function(args, required) {
  flags <- c(required, "--ca-bundle")
  usage <- paste(
    "Usage:", paste(required, "VALUE", collapse = " "),
    "[--ca-bundle PATH] [--send]"
  )
  options <- list(send = FALSE)
  while (length(args) > 0L) {
    flag <- args[[1L]]
    name <- gsub("-", "_", sub("^--", "", flag), fixed = TRUE)
    if (identical(flag, "--send")) {
      options$send <- TRUE
      args <- args[-1L]
    } else if (flag %in% flags && length(args) >= 2L &&
                 is.null(options[[name]])) {
      options[[name]] <- args[[2L]]
      args <- args[-(1:2)]
    } else {
      stop_submission(usage)
    }
  }
  given <- gsub("-", "_", sub("^--", "", required), fixed = TRUE)
  if (!all(given %in% names(options))) stop_submission(usage)
  if (!is.null(options$ca_bundle)) {
    if (!file.exists(options$ca_bundle)) {
      stop_submission("The CA bundle does not exist: ", options$ca_bundle)
    }
    options$ca_bundle <- normalizePath(options$ca_bundle)
  }
  options
}

# Stops unless `path` has the approved digest. The caller passes the staged copy,
# so the bytes verified are the bytes sent.
check_submission_digest <- function(path, sha256) {
  actual <- unname(tools::sha256sum(path))
  if (!identical(actual, sha256)) {
    stop_submission(
      "SHA-256 of ", basename(path), " is ", actual, ", not ", sha256, "."
    )
  }
}

# Copies the tarball into `directory` and answers its name and size.
stage_submission_tarball <- function(path, sha256, directory) {
  name <- basename(path)
  if (!grepl("^marginplyr_[0-9]+([.][0-9]+)+[.]tar[.]gz$", name)) {
    stop_submission("Not a marginplyr source tarball: ", name)
  }
  staged <- file.path(directory, name)
  if (!file.exists(path) || !file.copy(path, staged)) {
    stop_submission("The tarball cannot be read: ", path)
  }
  check_submission_digest(staged, sha256)
  list(name = name, size = format(file.size(staged), scientific = FALSE))
}

html_matches <- function(pattern, html) {
  regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1L]]
}

# The page as one string. Comments are dropped, because both sites keep retired
# sections and notices in them.
read_submission_page <- function(path) {
  html <- if (file.exists(path)) readLines(path, warn = FALSE, encoding = "UTF-8")
  gsub("(?s)<!--.*?-->", "", paste(html, collapse = "\n"), perl = TRUE)
}

# curl options every call takes. --disable keeps a .curlrc out, which could turn
# off verification or retry.
submission_curl_options <- function(ca_bundle) {
  c(
    "--disable", "--silent", "--show-error", "--fail", "--proto", "=https",
    "--max-time", "300",
    if (!is.null(ca_bundle)) c("--cacert", ca_bundle)
  )
}

submission_curl_failure <- function(result) {
  paste0(
    "curl status ", result$status, ": ", trimws(result$stderr),
    if (identical(result$status, 60L)) {
      paste0(
        "\ncurl could not verify the certificate. If its CA list lacks the ",
        "site's root, rerun with --ca-bundle naming a current CA bundle."
      )
    }
  )
}

# Evaluates everything from the first sent byte to the last printed line. A
# condition there takes exit 2, and not the exit that invites a rerun.
after_sending <- function(expr) {
  attempted <- function(condition) {
    reason <- conditionMessage(condition)
    stop_submission(
      "A send was attempted; do not repeat it without a new approval. ",
      if (nzchar(reason)) reason else class(condition)[[1L]],
      status = 2L
    )
  }
  tryCatch(expr, error = attempted, interrupt = attempted)
}

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Runs one submission as a command, with its exit status.
run_submission_command <- function(submit) {
  tryCatch(
    submit(commandArgs(trailingOnly = TRUE)),
    marginplyr_submission_error = function(error) {
      message(conditionMessage(error))
      quit(status = error$status)
    }
  )
}
