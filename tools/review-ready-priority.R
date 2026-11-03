# Proposes tracked membership/order from one local full invocation (ADR 0040).
# Generation writes only invocation artifacts; native testthat owns dispatch.

# Reads exact filename stems; ambiguous glob registrations need human resolution.
review_ready_priority_read <- function(description) {
  field <- "Config/testthat/start-first"
  connection <- textConnection(description)
  on.exit(close(connection), add = TRUE)
  value <- read.dcf(connection)
  if (!field %in% colnames(value)) return(character())
  priority <- trimws(strsplit(value[[1L, field]], ",", fixed = TRUE)[[1L]])
  if (!all(nzchar(priority)) || anyDuplicated(priority) ||
      !all(grepl("^[A-Za-z0-9_.-]+$", priority))) {
    stop("priority must contain unique literal test filename stems")
  }
  priority
}

# Replaces just the DCF priority field, preserving every other source line.
review_ready_priority_description <- function(description, priority) {
  start <- grep("^Config/testthat/start-first:", description)
  if (length(start)) {
    following <- which(seq_along(description) > start & grepl("^[^[:space:]]", description))
    end <- if (length(following)) following[[1L]] - 1L else length(description)
    before <- head(description, start - 1L)
    after <- if (end < length(description)) description[seq.int(end + 1L, length(description))] else character()
  } else {
    before <- description
    after <- character()
  }
  replacement <- if (length(priority)) c("Config/testthat/start-first:",
    paste0("    ", priority, c(rep(",", length(priority) - 1L), ""))) else character()
  c(before, replacement, after)
}

# Greedy list scheduling uses all observed intervals, including skipped work.
review_ready_priority_model <- function(order, durations) {
  workers <- rep(0, 4L)
  for (file in order) {
    worker <- which.min(workers)
    workers[[worker]] <- workers[[worker]] + durations[[file]]
  }
  max(workers)
}

# Separates mandatory registration from conservative optional stable insertion.
review_ready_priority_calculate <- function(priority, files) {
  stems <- sub("[.][rR]$", "", sub("^test[-_.]?", "", files$file))
  if (anyDuplicated(stems)) stop("test filenames do not have unique priority stems")
  duration <- stats::setNames(files$duration, stems)
  eligible <- stats::setNames(files$eligible, stems)
  native <- c(intersect(priority, stems), setdiff(stems, priority))
  registration <- intersect(priority, stems)
  new <- setdiff(stems[eligible], registration)
  new <- new[order(-duration[new], new, method = "radix")]
  for (file in new) {
    shorter <- which(eligible[registration] & duration[registration] < duration[[file]])
    position <- if (length(shorter)) shorter[[1L]] - 1L else length(registration)
    registration <- append(registration, file, after = position)
  }
  candidate <- registration
  for (i in seq_along(candidate)) {
    position <- i
    while (position > 1L) {
      later <- candidate[[position]]
      earlier <- candidate[[position - 1L]]
      if (!eligible[[earlier]] || !eligible[[later]] ||
          duration[[later]] <= 1.5 * duration[[earlier]] ||
          duration[[later]] - duration[[earlier]] <= 10) break
      candidate[c(position - 1L, position)] <- c(later, earlier)
      position <- position - 1L
    }
  }
  schedule <- function(order) review_ready_priority_model(c(order, setdiff(stems, order)), duration)
  models <- c(native = schedule(native), registration = schedule(registration), candidate = schedule(candidate))
  adopt <- models[["registration"]] - models[["candidate"]] >= 10
  selected <- if (adopt) candidate else registration
  models <- c(models, selected = schedule(selected))
  list(eligible = TRUE, before = priority, registered = new, removed = setdiff(priority, stems),
    native = native, registration = registration, candidate = candidate, selected = selected,
    adopt_reordering = adopt, models = models, files = files)
}

# Revalidates retained raw evidence, unchanged test inputs and dependency environment.
review_ready_priority_evidence <- function(root, run) {
  metadata <- readRDS(file.path(run, "invocation.rds"))
  terminal <- readRDS(file.path(run, "result.rds"))
  directory <- file.path(run, "timing")
  manifest <- readRDS(file.path(directory, "manifest.rds"))
  evidence <- readRDS(file.path(directory, "evidence.rds"))
  started <- as.POSIXct(strptime(metadata$started_utc, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
  age <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  if (length(age) != 1L || is.na(age) || age < 0 || age >= 7 * 24 * 60 * 60) {
    stop("invocation evidence expired or has an invalid start time; defer to the next ordinary full check")
  }
  if (!identical(metadata$version, 1L) || !identical(metadata$scope, "full") ||
      !is.character(metadata$sha) || length(metadata$sha) != 1L ||
      is.na(metadata$sha) || !grepl("^[0-9a-f]{40}$", metadata$sha) ||
      !identical(terminal, c(metadata, list(status = "passed", heavy_check = "passed"))) ||
      !identical(manifest$version, 1L) || !identical(manifest$sha, metadata$sha) ||
      !identical(manifest$started_utc, metadata$started_utc) ||
      !identical(evidence$version, 1L) || !isTRUE(evidence$eligible)) {
    stop("one successful eligible local full result is required")
  }
  hashes <- vapply(manifest$contents, digest::digest, character(1),
    algo = "md5", serialize = FALSE)
  if (!identical(hashes, manifest$md5)) stop("archived input identities changed")
  verified <- review_ready_timing_evidence(directory, terminal, "Starting 4 test processes.")
  if (!identical(verified, evidence)) stop("available timing evidence changed or is incomplete")
  discovered <- testthat::find_test_scripts(file.path(root, "tests", "testthat"), full.names = FALSE)
  if (!identical(discovered, manifest$discovered)) stop("test membership changed; defer to the next ordinary full check")
  inputs <- paste0("tests/", list.files(file.path(root, "tests"), recursive = TRUE, all.files = TRUE))
  expected <- setdiff(names(manifest$contents), "DESCRIPTION")
  if (!identical(inputs, expected)) stop("test/helper membership changed; defer to the next ordinary full check")
  for (input in inputs) {
    path <- file.path(root, input)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    if (!identical(bytes, manifest$contents[[input]])) stop("test/helper content changed: ", input)
  }
  description <- readLines(file.path(root, "DESCRIPTION"))
  old_description <- strsplit(rawToChar(manifest$contents[["DESCRIPTION"]]), "\n", fixed = TRUE)[[1L]]
  if (!identical(review_ready_priority_description(description, character()),
      review_ready_priority_description(old_description, character()))) {
    stop("DESCRIPTION changed beyond priority; defer to the next ordinary full check")
  }
  dependencies <- utils::installed.packages()[, c("Package", "Version", "Built"), drop = FALSE]
  session <- utils::sessionInfo()
  environment_fields <- c("R.version", "platform", "running", "locale", "time.zone",
    "matprod", "BLAS", "LAPACK")
  if (!identical(dependencies, manifest$dependencies) ||
      !identical(session[environment_fields], manifest$session[environment_fields])) {
    stop("dependency environment changed; defer to the next ordinary full check")
  }
  for (pid in unique(evidence$observations$pid)) {
    worker <- readRDS(file.path(directory, paste0(pid, ".worker.rds")))
    if (!identical(worker$session[environment_fields], manifest$session[environment_fields])) {
      stop("worker environment differs from the measured environment")
    }
    own_package <- read.dcf(file.path(root, "DESCRIPTION"))[[1L, "Package"]]
    packages <- c(worker$session$otherPkgs, worker$session$loadedOnly)
    for (package in setdiff(names(packages), own_package)) {
      versions <- manifest$dependencies[manifest$dependencies[, "Package"] == package, "Version"]
      if (!packages[[package]]$Version %in% versions) stop("worker dependency differs: ", package)
    }
  }
  if (any(!is.finite(evidence$files$duration) | evidence$files$duration < 0)) stop("invalid model durations")
  list(manifest = manifest, evidence = evidence, description = description)
}

# Writes a deterministic patch and explanation under the identified invocation.
# Any optimization/output failure leaves the ordinary check result authoritative.
review_ready_priority_propose <- function(root, run) {
  output <- NULL
  tryCatch(withCallingHandlers({
    input <- review_ready_priority_evidence(root, run)
    proposal <- review_ready_priority_calculate(review_ready_priority_read(input$description), input$evidence$files)
    proposal$sha <- input$manifest$sha
    proposal$identities <- input$manifest$md5
    proposal$movements <- data.frame(file = proposal$selected,
      from = match(proposal$selected, proposal$before), to = seq_along(proposal$selected))
    output <- tempfile("priority-", tmpdir = run)
    if (!dir.create(output)) stop("cannot create invocation-owned proposal directory")
    after <- review_ready_priority_description(input$description, proposal$selected)
    changed <- !identical(proposal$before, proposal$selected)
    # Diff two artifact copies; neither the checked source nor installed metadata is edited.
    writeLines(input$description, file.path(output, "before"))
    writeLines(after, file.path(output, "after"))
    patch <- if (changed) processx::run("git", c("diff", "--no-index", "--no-ext-diff", "--no-color",
      "--src-prefix=a/", "--dst-prefix=b/", "before", "after"), wd = output, error_on_status = FALSE) else NULL
    if (changed && patch$status != 1L) stop("could not calculate DESCRIPTION patch")
    report <- c("# Local test priority proposal", paste("Measured SHA:", proposal$sha),
      "Eligibility: one successful complete local full result; four native workers.",
      paste("Registrations:", paste(proposal$registered, collapse = ", ")),
      paste("Removals:", paste(proposal$removed, collapse = ", ")),
      paste("Optional existing-file reordering adopted:", proposal$adopt_reordering),
      "Greedy four-worker schedule models (seconds; not observed speedups or statistical guarantees):",
      paste(names(proposal$models), sprintf("%.6f", proposal$models), sep = ": "),
      paste("Modeled gain over registration:", sprintf("%.6f", proposal$models[["registration"]] - proposal$models[["selected"]])),
      paste("Modeled gain over native registered-first/append-last dispatch:",
        sprintf("%.6f", proposal$models[["native"]] - proposal$models[["selected"]])),
      "", "Selected priority:", proposal$selected, "", "File eligibility and measured durations:",
      utils::capture.output(print(proposal$files, row.names = FALSE)),
      "", "Archived input identities (MD5):", paste(names(proposal$identities), proposal$identities),
      "", "Before / registration / optional candidate orders:",
      paste("Before:", paste(proposal$before, collapse = ", ")),
      paste("Registration:", paste(proposal$registration, collapse = ", ")),
      paste("Candidate:", paste(proposal$candidate, collapse = ", ")),
      "", "Proposed positions (NA denotes new registration):",
      utils::capture.output(print(proposal$movements, row.names = FALSE)),
      "", "Recalculate against current priority immediately before explicit Git application.",
      "Changed tests/helpers, DESCRIPTION beyond priority, dependency environment or expired/unavailable evidence defer adoption.",
      "Include an eligible update alongside the next ordinary code change; otherwise defer.",
      "The adopted commit needs its own applicable verification; never automatically apply a subsequent proposal.")
    writeLines(report, file.path(output, "explanation.md"))
    saveRDS(proposal, file.path(output, "proposal.rds"))
    if (changed) {
      patch_text <- strsplit(patch$stdout, "\n", fixed = TRUE)[[1L]]
      patch_text <- sub("a/before", "a/DESCRIPTION", patch_text, fixed = TRUE)
      patch_text <- sub("b/after", "b/DESCRIPTION", patch_text, fixed = TRUE)
      writeLines(patch_text, file.path(output, "priority.patch"))
    }
    unlink(file.path(output, c("before", "after")))
    cat("Review-ready priority proposal: ", output,
      if (changed) " (reviewable patch; source unchanged)\n" else " (unchanged priority; no patch)\n", sep = "")
    proposal$directory <- output
    proposal
  }, warning = function(cnd) stop(conditionMessage(cnd))), error = function(cnd) {
    if (!is.null(output)) unlink(file.path(output, "priority.patch"))
    reason <- conditionMessage(cnd)
    cat("Review-ready priority proposal disabled: ", reason, "\n", sep = "")
    list(eligible = FALSE, reason = reason)
  })
}
