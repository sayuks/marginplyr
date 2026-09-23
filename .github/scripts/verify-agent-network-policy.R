# Confirms that both agents allow exactly the hosts sandboxed CRAN preflight
# needs, while Codex also allows api.github.com for the GitHub CLI. Codex uses
# full proxy mode because gh sends GraphQL POST requests; the host allowlist
# still applies. Preflight hosts come from R's standard repositories and the
# URLs in the current package candidate. A redirect the candidate currently
# follows is named beside its source URL so deleting that URL also deletes the
# exception.
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

# Finds HTTP(S) targets in the Markdown files R includes in its package URL
# database. R's own scanner reads these files only when xml2 is installed, but
# this verifier must give the same answer before lint installs any packages.
markdown_urls <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0L) {
    return(character())
  }
  documents <- vapply(
    paths,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
  document_urls <- function(document) {
    matches <- gregexpr(
      "]\\(https?://[^[:space:]()]+\\)",
      document,
      perl = TRUE
    )[[1L]]
    if (identical(matches, -1L)) {
      return(character())
    }
    lengths <- attr(matches, "match.length")
    urls <- character()
    for (index in seq_along(matches)) {
      close <- matches[[index]]
      depth <- 1L
      open <- close - 1L
      while (open > 0L && depth > 0L) {
        character <- substr(document, open, open)
        if (identical(character, "]")) {
          depth <- depth + 1L
        } else if (identical(character, "[")) {
          depth <- depth - 1L
        }
        open <- open - 1L
      }
      is_image <- depth > 0L ||
        (open > 0L && identical(substr(document, open, open), "!"))
      if (!is_image) {
        match <- substr(document, close, close + lengths[[index]] - 1L)
        urls <- c(urls, sub("^]\\((.*)\\)$", "\\1", match))
      }
    }
    urls
  }
  unique(unlist(lapply(documents, document_urls), use.names = FALSE))
}

# Reads one exact TOML table and refuses entries other than its requested value.
codex_table_keys <- function(lines, header, value, description) {
  start <- match(header, lines)
  if (is.na(start)) {
    stop(
      paste("Codex configuration has no", description, "table."),
      call. = FALSE
    )
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
  pattern <- paste0(
    '^"([^"]+)"[[:space:]]*=[[:space:]]*"',
    value,
    '"$'
  )
  if (length(entries) == 0L || any(!grepl(pattern, entries))) {
    stop(
      paste(
        "Codex",
        description,
        "entries must all be explicit",
        value,
        "values."
      ),
      call. = FALSE
    )
  }
  sort(unique(sub(pattern, "\\1", entries)))
}

# Reads the repository-owned JSON array in its deliberately one-entry-per-line
# form, so this base-R verifier needs no package installation before lint.
claude_string_array <- function(lines, name) {
  array_pattern <- paste0(
    '^[[:space:]]*"',
    name,
    '"[[:space:]]*:[[:space:]]*\\[$'
  )
  start <- grep(array_pattern, lines)
  if (length(start) != 1L) {
    stop(
      paste("Claude Code configuration must have one", name, "array."),
      call. = FALSE
    )
  }
  following <- lines[seq.int(start + 1L, length(lines))]
  close <- which(grepl("^[[:space:]]*]", following))
  if (length(close) == 0L) {
    stop(paste("Claude Code", name, "array is not closed."), call. = FALSE)
  }
  entries <- trimws(following[seq_len(close[[1L]] - 1L)])
  entries <- entries[nzchar(entries)]
  pattern <- '^"([^"]+)"[,]?$'
  if (length(entries) == 0L || any(!grepl(pattern, entries))) {
    stop(
      paste("Claude Code", name, "entries must be JSON strings."),
      call. = FALSE
    )
  }
  sort(unique(sub(pattern, "\\1", entries)))
}

repository_path <- if (
  exists("agent_policy_repository_path", inherits = FALSE)
) agent_policy_repository_path else "."

standard_urls <- unname(tools:::.get_standard_repository_URLs())
candidate_urls <- unique(c(
  tools:::url_db_from_package_sources(repository_path)$URL,
  markdown_urls(file.path(repository_path, c("README.md", "NEWS.md")))
))

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

codex_config <- readLines(
  file.path(repository_path, ".codex", "config.toml"),
  warn = FALSE
)
claude_config <- readLines(
  file.path(repository_path, ".claude", "settings.json"),
  warn = FALSE
)
codex_domains <- codex_table_keys(
  codex_config,
  "[permissions.marginplyr.network.domains]",
  "allow",
  "network-domain"
)
claude_domains <- claude_string_array(claude_config, "allowedDomains")

if (!identical(codex_domains, sort(unique(c(claude_domains, "api.github.com"))))) {
  stop(
    "Codex must allow only Claude Code's preflight hosts plus api.github.com.",
    call. = FALSE
  )
}
if (!identical(claude_domains, required_domains)) {
  stop(
    paste0(
      "Agent preflight allowlists do not exactly match the hosts required by ",
      "the standard repositories and current candidate URLs."
    ),
    call. = FALSE
  )
}
if (any(codex_domains == "*")) {
  stop("Agent network allowlists must not allow the public internet.", call. = FALSE)
}

required_cache_paths <- c(
  "~/.cache/R/marginplyr",
  "~/Library/Caches/org.R-project.R/R/marginplyr"
)
required_codex_write_paths <- c(
  required_cache_paths,
  "~/Library/Caches/quarto"
)
codex_writable_paths <- codex_table_keys(
  codex_config,
  "[permissions.marginplyr.filesystem]",
  "write",
  "filesystem-write"
)
claude_writable_paths <- claude_string_array(claude_config, "allowWrite")
if (!identical(codex_writable_paths, sort(required_codex_write_paths))) {
  stop(
    "Codex filesystem writes must be limited to agent tool caches.",
    call. = FALSE
  )
}
if (!identical(claude_writable_paths, sort(required_cache_paths))) {
  stop(
    "Claude Code filesystem writes must be limited to preflight evidence caches.",
    call. = FALSE
  )
}

required_codex_lines <- c(
  "network_proxy = true",
  "[permissions.marginplyr.network]",
  "enabled = true",
  'mode = "full"'
)
if (!all(required_codex_lines %in% codex_config)) {
  stop("Codex proxy must allow GitHub CLI requests.", call. = FALSE)
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

build_ignore <- readLines(file.path(repository_path, ".Rbuildignore"), warn = FALSE)
if (!all(c("^\\.claude$", "^\\.codex$") %in% build_ignore)) {
  stop("Agent project settings must stay out of the source package.", call. = FALSE)
}

message(
  "Verified agent preflight allowlists for ",
  length(required_domains),
  " required hosts and Codex GitHub API access."
)
