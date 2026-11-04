# Serializes one bounded response; JSON escaping is included in the byte count.
retrieval_json <- function(value) {
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = NA))
}

# Requires an integer in the supported range before using it as a byte/page count.
retrieval_integer <- function(value, minimum = 1L, maximum = 1000000L) {
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || is.na(number) || !is.finite(number) ||
      number != floor(number) || number < minimum || number > maximum) {
    stop("Invalid integer option.", call. = FALSE)
  }
  number
}

# Builds immutable, independently identified document pages in a new bundle.
# Callers supply UTF-8 text and source metadata; no receipt state is advanced.
retrieval_bundle <- function(sources, output, budget) {
  if (file.exists(output)) stop("Output already exists.", call. = FALSE)
  if (!dir.create(output)) stop("Cannot create output directory.", call. = FALSE)
  complete <- FALSE
  on.exit(if (!complete) unlink(output, recursive = TRUE), add = TRUE)
  page <- 0L
  for (index in seq_along(sources)) {
    source <- sources[[index]]
    text <- source$text
    if (is.na(iconv(text, from = "UTF-8", to = "UTF-8"))) {
      stop("Source must be valid UTF-8 text.", call. = FALSE)
    }
    characters <- nchar(text, type = "chars")
    offset <- 0
    position <- 0
    repeat {
      template <- list(page = page + 1L, document = index, documents = length(sources),
        source = source$identity, md5 = source$md5, total_bytes = nchar(text, type = "bytes"),
        start_byte = offset, end_byte = offset, content = "")
      # Reserve room for the final page count, continuation and receipt.
      allowance <- budget - 160L
      if (nchar(retrieval_json(template), type = "bytes") > allowance) {
        stop("Budget too small for source identity.", call. = FALSE)
      }
      low <- 0
      high <- min(characters - position, budget)
      while (low < high) {
        middle <- ceiling((low + high) / 2)
        template$content <- substr(text, position + 1, position + middle)
        template$end_byte <- offset + nchar(template$content, type = "bytes")
        if (nchar(retrieval_json(template), type = "bytes") <= allowance) {
          low <- middle
        } else {
          high <- middle - 1
        }
      }
      if (low == 0 && position < characters) {
        stop("Budget cannot hold the next character.", call. = FALSE)
      }
      template$content <- substr(text, position + 1, position + low)
      template$end_byte <- offset + nchar(template$content, type = "bytes")
      page <- page + 1L
      saveRDS(template, file.path(output, paste0(page, ".rds")))
      position <- position + low
      offset <- template$end_byte
      if (position == characters) break
    }
  }
  saveRDS(list(pages = page, budget = budget), file.path(output, "manifest.rds"))
  complete <- TRUE
  list(pages = page, budget = budget, next_page = 1L)
}

# Returns one replayable page. A missing END_PAGE receipt means no accepted read.
retrieval_page <- function(bundle, page) {
  manifest <- readRDS(file.path(bundle, "manifest.rds"))
  page <- retrieval_integer(page, maximum = manifest$pages)
  value <- readRDS(file.path(bundle, paste0(page, ".rds")))
  value$pages <- manifest$pages
  value$next_page <- if (page < manifest$pages) page + 1L else NULL
  result <- paste0(retrieval_json(value), "\nEND_PAGE ", page, "\n")
  if (nchar(result, type = "bytes") > manifest$budget) {
    stop("Page exceeds its budget.", call. = FALSE)
  }
  result
}

# Snapshots a regular UTF-8 file, preserving byte identity including final newlines.
retrieval_document <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  if (dir.exists(path)) stop("Expected a file.", call. = FALSE)
  bytes <- readBin(path, "raw", n = file.size(path))
  list(identity = path, md5 = retrieval_hash(bytes), text = rawToChar(bytes))
}

# Hashes the captured bytes rather than a live file that may change after reading.
retrieval_hash <- function(bytes) {
  path <- tempfile("retrieval-hash-")
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  unname(tools::md5sum(path))
}

# Reads a dotted object field, distinguishing an explicit null from a missing key.
retrieval_field <- function(record, path) {
  for (key in strsplit(path, ".", fixed = TRUE)[[1L]]) {
    if (!is.list(record) || !(key %in% names(record))) {
      return(list(present = FALSE, value = NULL))
    }
    record <- record[[key]]
  }
  list(present = TRUE, value = record)
}

# Accepts UTC RFC3339 instants, with optional fractional seconds.
retrieval_time <- function(value) {
  if (!is.character(value) || length(value) != 1L ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]+)?Z$", value)) {
    return(NA_real_)
  }
  as.numeric(as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
}

# Projects selected events; every unreadable record/field is an explicit diagnostic.
# The projection and query summary are paginated by the same document boundary.
retrieval_logs <- function(sources, options) {
  if (is.null(options$fields)) stop("Logs require --fields.", call. = FALSE)
  fields <- strsplit(options$fields, ",", fixed = TRUE)[[1L]]
  if (length(fields) == 0L || !all(grepl("^[A-Za-z0-9_-]+([.][A-Za-z0-9_-]+)*$", fields)) ||
      anyDuplicated(fields)) {
    stop("Supply unique dotted field paths.", call. = FALSE)
  }
  defaults <- list("type-field" = "type", "tool-field" = "payload.name", "time-field" = "timestamp")
  for (name in names(defaults)) {
    if (is.null(options[[name]])) options[[name]] <- defaults[[name]]
  }
  bounds <- c(after = -Inf, before = Inf)
  for (name in names(bounds)) {
    if (!is.null(options[[name]])) {
      bounds[[name]] <- retrieval_time(options[[name]])
      if (is.na(bounds[[name]])) stop("Time bounds require UTC RFC3339 instants.", call. = FALSE)
    }
  }
  if (bounds[["after"]] > bounds[["before"]]) stop("Reversed time interval.", call. = FALSE)
  lapply(sources, function(source) {
    lines <- strsplit(source$text, "\n", fixed = TRUE)[[1L]]
    records <- list()
    summary <- list(status = "query", input_md5 = source$md5,
      query = options[setdiff(names(options), c("output", "budget"))],
      scanned = length(lines), matched = 0L, malformed = 0L,
      missing_filter = 0L, missing_fields = 0L, selection_complete = TRUE)
    for (line in seq_along(lines)) {
      record <- tryCatch(jsonlite::fromJSON(lines[[line]], simplifyVector = FALSE),
        error = function(error) NULL)
      if (!is.list(record) || is.null(names(record))) {
        summary$malformed <- summary$malformed + 1L
        records[[length(records) + 1L]] <- list(line = line, status = "malformed")
        next
      }
      selected <- TRUE
      for (selector in c("type", "tool", "time")) {
        if (selector != "time" && is.null(options[[selector]])) next
        if (selector == "time" && all(is.infinite(bounds))) next
        field <- retrieval_field(record, options[[paste0(selector, "-field")]])
        valid <- field$present && is.character(field$value) && length(field$value) == 1L
        if (selector == "time" && valid) valid <- !is.na(retrieval_time(field$value))
        if (!valid) {
          summary$missing_filter <- summary$missing_filter + 1L
          records[[length(records) + 1L]] <- list(line = line, status = "missing_filter",
            field = options[[paste0(selector, "-field")]])
          selected <- FALSE
          break
        }
        matches <- if (selector == "time") {
          time <- retrieval_time(field$value)
          time >= bounds[["after"]] && time <= bounds[["before"]]
        } else {
          field$value == options[[selector]]
        }
        if (!matches) {
          selected <- FALSE
          break
        }
      }
      if (!selected) next
      projected <- lapply(fields, function(path) retrieval_field(record, path))
      missing <- fields[!vapply(projected, function(field) field$present, logical(1L))]
      summary$matched <- summary$matched + 1L
      summary$missing_fields <- summary$missing_fields + length(missing)
      records[[length(records) + 1L]] <- list(line = line, status = "selected",
        missing_fields = unname(as.list(missing)),
        fields = setNames(lapply(projected, function(field) field$value), fields))
    }
    summary$selection_complete <- summary$malformed + summary$missing_filter + summary$missing_fields == 0L
    text <- paste0(paste(vapply(c(list(summary), records), retrieval_json, character(1L)), collapse = "\n"), "\n")
    list(identity = paste0(source$identity, " (projection)"),
      md5 = retrieval_hash(charToRaw(text)), text = text)
  })
}

# Parses CLI options with explicit positional separation; duplicates are errors.
retrieval_options <- function(args, allowed) {
  values <- list()
  while (length(args) > 0L && args[[1L]] != "--") {
    name <- sub("^--", "", args[[1L]])
    if (!startsWith(args[[1L]], "--") || !(name %in% allowed) ||
        name %in% names(values) || length(args) < 2L) {
      stop("Unknown, duplicate or incomplete option.", call. = FALSE)
    }
    values[[name]] <- args[[2L]]
    args <- args[-c(1L, 2L)]
  }
  if (length(args) > 0L) args <- args[-1L]
  list(values = values, paths = args)
}

# Dispatches the public command. Data output is bounded by the bundle's budget.
retrieval_main <- function(args) {
  if (length(args) == 0L || args[[1L]] == "--help") {
    cat("Usage: Rscript tools/retrieve-agent.R docs --output DIR [--budget BYTES] -- FILE...\n",
      "       Rscript tools/retrieve-agent.R page DIR PAGE\n",
      "       Rscript tools/retrieve-agent.R logs --output DIR [query options] -- FILE\n",
      "See tools/retrieve-agent.md for query options and receipt accounting.\n", sep = "")
    return(invisible(NULL))
  }
  mode <- args[[1L]]
  if (mode == "page") {
    if (length(args) != 3L) stop("Supply bundle and page.", call. = FALSE)
    cat(retrieval_page(args[[2L]], args[[3L]]))
    return(invisible(NULL))
  }
  if (!(mode %in% c("docs", "logs"))) stop("Unknown command.", call. = FALSE)
  options <- retrieval_options(args[-1L], c("output", "budget",
    if (mode == "logs") c("type", "tool", "after", "before", "fields",
      "type-field", "tool-field", "time-field")))
  values <- options$values
  if (is.null(values$output) || length(options$paths) == 0L) {
    stop("Supply --output and input files after --.", call. = FALSE)
  }
  budget <- retrieval_integer(if (is.null(values$budget)) "6000" else values$budget,
    minimum = 1200L)
  sources <- lapply(options$paths, retrieval_document)
  if (mode == "logs") sources <- retrieval_logs(sources, values)
  info <- retrieval_bundle(sources, values$output, budget)
  cat(retrieval_json(info), "\n", sep = "")
}
