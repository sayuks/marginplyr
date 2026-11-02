# Throwaway base-R saved-input cost model; no package or production code changes.
cost_clock <- function() as.numeric(Sys.time())

cost_promotions <- function(order, durations) {
  result <- character()
  promoted <- character()
  crossings <- list()
  for (name in order) {
    result <- c(result, name)
    index <- length(result)
    moved <- FALSE
    while (index > 1L) {
      before <- result[[index - 1L]]
      if (!(durations[[name]] > 1.5 * durations[[before]] &&
            durations[[name]] - durations[[before]] > 10)) {
        break
      }
      result[c(index - 1L, index)] <- c(name, before)
      crossings[[length(crossings) + 1L]] <- c(name, before)
      moved <- TRUE
      index <- index - 1L
    }
    if (moved) promoted <- c(promoted, name)
  }
  list(order = result, promoted = promoted, crossings = crossings)
}

cost_replay <- function(order, durations) {
  workers <- numeric(4L)
  for (name in order) {
    index <- which.min(workers)
    workers[[index]] <- workers[[index]] + durations[[name]]
  }
  max(workers)
}

cost_pipeline <- function(source_root, output_directory) {
  started <- cost_clock()
  cost_root <- source_root
  cost_archive <- file.path(
    cost_root, "investigation/2026-11-02-review-ready-order-calibration"
  )
  files <- read.csv(file.path(cost_archive, "files.csv"),
                    colClasses = c("character", "integer", "character", "character"))
  expected <- files$file
  registered <- expected[files$registered == "TRUE"]
  unregistered <- sort(expected[files$registered == "FALSE"], method = "radix")
  stopifnot(length(expected) == 196L, !anyDuplicated(expected),
            identical(files$priority, seq_len(196L)),
            all(files$registered %in% c("TRUE", "FALSE")),
            all(nchar(files$md5) == 32L), length(registered) == 186L,
            length(unregistered) == 10L)
  archived_summary <- read.csv(file.path(cost_archive, "run-summary.csv"))
  run <- archived_summary[archived_summary$run == 1L, , drop = FALSE]
  stopifnot(nrow(run) == 1L, run$exit_code == 0L, run$files == 196L,
            run$workers == 4L, run$errors == 0L, run$warnings == 0L, run$notes == 0L)
  state_lines <- readLines(file.path(cost_archive, "executions.json"), warn = FALSE)
  sha_line <- grep('^  "sha":', state_lines, value = TRUE)
  measurement_sha <- sub('.*"([0-9a-f]{40})".*', '\\1', sha_line)
  stopifnot(length(measurement_sha) == 1L, nchar(measurement_sha) == 40L)
  paths <- sort(list.files(file.path(cost_archive, "run-1/check/file-timings"),
                           pattern = "[.]tsv$", full.names = TRUE))
  stopifnot(length(paths) == 4L)
  events <- do.call(rbind, lapply(paths, function(path) {
    read.table(path, sep = "\t", header = FALSE, quote = "", comment.char = "",
               colClasses = c("character", "character", "integer", "numeric", "numeric"),
               col.names = c("event", "file", "pid", "utc", "elapsed"))
  }))
  stopifnot(nrow(events) == 392L, setequal(events$file, expected),
            all(events$event %in% c("start", "end")),
            length(unique(events$pid)) == 4L,
            all(is.finite(events$utc)), all(is.finite(events$elapsed)))
  pairs <- split(seq_len(nrow(events)), events$file)
  durations <- setNames(numeric(length(expected)), expected)
  for (name in expected) {
    pair <- events[pairs[[name]], , drop = FALSE]
    stopifnot(nrow(pair) == 2L, identical(pair$event, c("start", "end")),
              pair$pid[[1L]] == pair$pid[[2L]])
    duration <- pair$elapsed[[2L]] - pair$elapsed[[1L]]
    stopifnot(is.finite(duration), duration >= 0)
    durations[[name]] <- round(duration, 6L)
  }
  console <- readLines(file.path(cost_archive, "run-1/console.log"), warn = FALSE)
  terminal <- grep("R CMD check: [0-9]+ ERROR\\(s\\), [0-9]+ WARNING\\(s\\), [0-9]+ NOTE\\(s\\)",
                   console, value = TRUE)
  stopifnot(length(terminal) >= 1L,
            grepl("R CMD check: 0 ERROR(s), 0 WARNING(s), 0 NOTE(s)",
                  tail(terminal, 1L), fixed = TRUE))
  description <- readLines(file.path(cost_root, "DESCRIPTION"), warn = FALSE)
  begin <- which(description == "Config/testthat/start-first:")
  stopifnot(length(begin) == 1L)
  end <- begin + 1L
  while (end <= length(description) && grepl("^[[:space:]]", description[[end]])) {
    end <- end + 1L
  }
  old_field <- description[seq.int(begin, end - 1L)]
  recorded <- trimws(sub(",$", "", old_field[-1L]))
  stopifnot(identical(paste0("test-", recorded, ".R"), registered))
  parsed <- cost_clock()

  # This deterministic insertion/tie rule is a benchmark choice, not an agreed spec.
  baseline <- registered
  insertion_order <- unregistered[order(-durations[unregistered], unregistered,
                                         method = "radix")]
  for (name in insertion_order) {
    shorter <- which(durations[baseline] < durations[[name]])
    after <- if (length(shorter)) shorter[[1L]] - 1L else length(baseline)
    baseline <- append(baseline, name, after = after)
  }
  stopifnot(identical(baseline[baseline %in% registered], registered))
  candidate <- cost_promotions(baseline, durations)
  actual <- c(unregistered, registered)
  models <- c(registration_only = cost_replay(baseline, durations),
              candidate = cost_replay(candidate$order, durations),
              actual_unregistered_first = cost_replay(actual, durations))
  adopt_reordering <- unname(models[["registration_only"]] - models[["candidate"]] >= 10)
  selected <- if (adopt_reordering) candidate$order else baseline
  selected_model <- models[[if (adopt_reordering) "candidate" else "registration_only"]]
  computed <- cost_clock()

  proposal <- list(
    kind = "throwaway_base_R_benchmark_proposal",
    measurement_sha = measurement_sha, measurement_run = 1L,
    files = length(expected), workers = 4L,
    eligibility_limit = "Archived intervals contain no all-skipped flags; no finished eligibility implementation is claimed.",
    registration_tie_rule = "descending duration, filename tie; insert before first strictly shorter entry",
    registrations = unregistered, optional_reordering_adopted = adopt_reordering,
    optional_promotions = candidate$promoted, optional_crossings = candidate$crossings,
    modeled_seconds = models,
    selected_gain_vs_actual_s = models[["actual_unregistered_first"]] - selected_model,
    selected_priority = selected
  )
  names_short <- sub("[.]R$", "", sub("^test-", "", selected))
  replacement <- c("Config/testthat/start-first:",
                   paste0("    ", names_short,
                          c(rep(",", length(names_short) - 1L), "")))
  # This sample retains existing relative order. The patch includes full-field
  # context so it needs neither an external diff process nor a JSON dependency.
  stopifnot(identical(selected[selected %in% registered], registered))
  changes <- " Config/testthat/start-first:"
  for (i in seq_along(selected)) {
    name <- selected[[i]]
    new_line <- replacement[[i + 1L]]
    old_index <- match(name, registered)
    if (is.na(old_index)) {
      changes <- c(changes, paste0("+", new_line))
    } else {
      old_line <- old_field[[old_index + 1L]]
      changes <- c(changes, if (identical(new_line, old_line)) paste0(" ", new_line)
                   else c(paste0("-", old_line), paste0("+", new_line)))
    }
  }
  patch <- c("--- a/DESCRIPTION", "+++ b/DESCRIPTION",
             sprintf("@@ -%d,%d +%d,%d @@", begin, length(old_field), begin,
                     length(replacement)), changes)
  saveRDS(proposal, file.path(output_directory, "proposal.rds"), compress = FALSE)
  writeLines(patch, file.path(output_directory, "proposal.patch"), useBytes = TRUE)
  finished <- cost_clock()
  list(parse_validate_ms = (parsed - started) * 1000,
       registration_reorder_replay_ms = (computed - parsed) * 1000,
       serialize_write_ms = (finished - computed) * 1000,
       pipeline_ms = (finished - started) * 1000,
       output_rds_bytes = unname(file.info(file.path(output_directory, "proposal.rds"))$size),
       output_patch_bytes = unname(file.info(file.path(output_directory, "proposal.patch"))$size),
       registered_files = length(registered), new_files = length(unregistered),
       models = models, optional_reordering_adopted = adopt_reordering)
}
