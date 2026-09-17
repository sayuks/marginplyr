# Confirms that Codex and Claude Code give sandboxed CRAN preflight commands
# the same fail-closed network boundary. The required hosts come from the two
# sources the preflight itself asks: R's standard repositories and the URLs in
# the current package candidate. A redirect the candidate currently follows is
# named beside its source URL so deleting that URL also deletes the exception.
#
# Run it locally with:
#
#     Rscript .github/scripts/verify-agent-network-policy.R

# Extracts HTTP(S) hosts without asking a network service.
url_hosts <- function(urls) {
  matches <- regexec(
    "^https?://([^/:?#]+)",
    urls,
    ignore.case = TRUE,
    perl = TRUE
  )
  parts <- regmatches(urls, matches)
  sort(unique(tolower(vapply(
    parts[lengths(parts) == 2L],
    `[[`,
    character(1),
    2L
  ))))
}

# Reads one exact TOML table and refuses entries other than explicit allows.
codex_allowed_domains <- function(lines) {
  header <- "[permissions.marginplyr.network.domains]"
  start <- match(header, lines)
  if (is.na(start)) {
    stop("Codex configuration has no network-domain table.", call. = FALSE)
  }
  following_headers <- which(
    seq_along(lines) > start & grepl("^\\[[^]]+\\]$", lines)
  )
  end <- if (length(following_headers) > 0L) {
    following_headers[[1L]] - 1L
  } else {
    length(lines)
  }
  entries <- trimws(lines[seq.int(start + 1L, end)])
  entries <- entries[nzchar(entries) & !startsWith(entries, "#")]
  pattern <- '^"([^"]+)"[[:space:]]*=[[:space:]]*"allow"$'
  if (length(entries) == 0L || any(!grepl(pattern, entries))) {
    stop(
      "Codex network-domain entries must all be explicit allows.",
      call. = FALSE
    )
  }
  sort(unique(sub(pattern, "\\1", entries)))
}

# Reads the repository-owned JSON array in its deliberately one-entry-per-line
# form, so this base-R verifier needs no package installation before lint.
claude_allowed_domains <- function(lines) {
  start <- grep('^[[:space:]]*"allowedDomains"[[:space:]]*:[[:space:]]*\\[$', lines)
  if (length(start) != 1L) {
    stop(
      "Claude Code configuration must have one allowedDomains array.",
      call. = FALSE
    )
  }
  following <- lines[seq.int(start + 1L, length(lines))]
  close <- which(grepl("^[[:space:]]*]", following))
  if (length(close) == 0L) {
    stop("Claude Code allowedDomains array is not closed.", call. = FALSE)
  }
  entries <- trimws(following[seq_len(close[[1L]] - 1L)])
  entries <- entries[nzchar(entries)]
  pattern <- '^"([^"]+)"[,]?$'
  if (length(entries) == 0L || any(!grepl(pattern, entries))) {
    stop(
      "Claude Code allowedDomains entries must be JSON strings.",
      call. = FALSE
    )
  }
  sort(unique(sub(pattern, "\\1", entries)))
}

standard_urls <- unname(tools:::.get_standard_repository_URLs())
candidate_urls <- tools:::url_db_from_package_sources(".")$URL

redirect_source <- paste0(
  "https://contributor-covenant.org/version/2/1/",
  "CODE_OF_CONDUCT.html"
)
if (!redirect_source %in% candidate_urls) {
  stop(
    "The recorded candidate redirect no longer has its source URL.",
    call. = FALSE
  )
}
required_domains <- sort(unique(c(
  url_hosts(standard_urls),
  url_hosts(candidate_urls),
  "www.contributor-covenant.org"
)))

codex_config <- readLines(".codex/config.toml", warn = FALSE)
claude_config <- readLines(".claude/settings.json", warn = FALSE)
codex_domains <- codex_allowed_domains(codex_config)
claude_domains <- claude_allowed_domains(claude_config)

if (!identical(codex_domains, claude_domains)) {
  stop(
    "Codex and Claude Code network allowlists differ.",
    call. = FALSE
  )
}
if (!identical(codex_domains, required_domains)) {
  stop(
    paste0(
      "Agent network allowlists do not exactly match the hosts required by ",
      "the standard repositories and current candidate URLs."
    ),
    call. = FALSE
  )
}
if (any(codex_domains == "*")) {
  stop("Agent network allowlists must not allow the public internet.", call. = FALSE)
}

required_codex_lines <- c(
  "network_proxy = true",
  "[permissions.marginplyr.network]",
  "enabled = true",
  'mode = "limited"'
)
if (!all(required_codex_lines %in% codex_config)) {
  stop("Codex sandboxed networking is not fail-closed.", call. = FALSE)
}

claude_text <- paste(claude_config, collapse = "\n")
required_claude_patterns <- c(
  '"enabled"[[:space:]]*:[[:space:]]*true',
  '"failIfUnavailable"[[:space:]]*:[[:space:]]*true',
  '"autoAllowBashIfSandboxed"[[:space:]]*:[[:space:]]*true',
  '"allowUnsandboxedCommands"[[:space:]]*:[[:space:]]*false'
)
if (any(!vapply(
  required_claude_patterns,
  grepl,
  logical(1),
  x = claude_text,
  perl = TRUE
))) {
  stop("Claude Code sandboxed networking is not fail-closed.", call. = FALSE)
}

build_ignore <- readLines(".Rbuildignore", warn = FALSE)
if (!all(c("^\\.claude$", "^\\.codex$") %in% build_ignore)) {
  stop("Agent project settings must stay out of the source package.", call. = FALSE)
}

message(
  "Verified matching fail-closed agent network allowlists for ",
  length(required_domains),
  " required hosts."
)
