#!/usr/bin/env Rscript

# Stateless deterministic helpers for the repository CRAN release playbook.
# The Markdown manual owns orchestration and every external or judgement step.

release_abort <- function(message, status = 1L) {
  condition <- structure(
    list(message = message, call = NULL, status = as.integer(status)),
    class = c("marginplyr_release_error", "error", "condition")
  )
  stop(condition)
}

# Reads one single-line DCF field from already loaded DESCRIPTION lines.
release_dcf_field <- function(lines, field) {
  matched <- grep(paste0("^", field, ":[[:space:]]*"), lines)
  if (length(matched) != 1L) {
    release_abort(sprintf(
      "DESCRIPTION must contain exactly one `%s` field.",
      field
    ))
  }
  sub(paste0("^", field, ":[[:space:]]*"), "", lines[[matched]])
}

# Replaces one single-line DCF field without reformatting the rest of the file.
release_set_dcf_field <- function(lines, field, value) {
  matched <- grep(paste0("^", field, ":[[:space:]]*"), lines)
  if (length(matched) != 1L) {
    release_abort(sprintf(
      "DESCRIPTION must contain exactly one `%s` field.",
      field
    ))
  }
  lines[[matched]] <- paste0(field, ": ", value)
  lines
}

release_version <- function(value, label = "version") {
  if (
    length(value) != 1L || is.na(value) ||
      !grepl("^[0-9]+[.][0-9]+[.][0-9]+$", value)
  ) {
    release_abort(sprintf(
      "The %s must have the release form `major.minor.patch`, not `%s`.",
      label,
      paste(value, collapse = ", ")
    ), status = 2L)
  }
  value
}

release_status <- function(description) {
  status <- release_dcf_field(
    description,
    "Config/marginplyr/cran-status"
  )
  if (!(status %in% c("unpublished", "published"))) {
    release_abort(sprintf(
      paste0(
        "`Config/marginplyr/cran-status` must be `unpublished` or ",
        "`published`, not `%s`."
      ),
      status
    ))
  }
  status
}

release_news_heading <- function(version) {
  paste("# marginplyr", version)
}

release_replace_news_heading <- function(lines, current, target) {
  expected <- release_news_heading(current)
  headings <- grep("^# marginplyr ", lines)
  if (length(headings) == 0L || headings[[1L]] != 1L) {
    release_abort("NEWS.md must begin with the current marginplyr heading.")
  }
  if (!identical(lines[[1L]], expected)) {
    release_abort(sprintf(
      "NEWS.md begins `%s`, but DESCRIPTION records version `%s`.",
      lines[[1L]],
      current
    ))
  }
  lines[[1L]] <- release_news_heading(target)
  lines
}

# Plans the release-version edit. An unpublished first release is already at
# its release number; an update advances one committed development version.
release_prepare_files <- function(files, version) {
  version <- release_version(version)
  description <- files$DESCRIPTION
  news <- files$`NEWS.md`
  current <- release_dcf_field(description, "Version")
  status <- release_status(description)

  if (identical(status, "unpublished")) {
    if (!identical(current, version)) {
      release_abort(sprintf(
        paste0(
          "The initial candidate is already version `%s`; refusing to ",
          "prepare unrelated version `%s`."
        ),
        current,
        version
      ))
    }
    release_replace_news_heading(news, current, version)
    return(files)
  }

  development <- grepl("^[0-9]+[.][0-9]+[.][0-9]+[.]9000$", current)
  already_prepared <- identical(current, version)
  if (!development && !already_prepared) {
    release_abort(sprintf(
      paste0(
        "A published package must be at a `.9000` development version or ",
        "the requested release version, not `%s`."
      ),
      current
    ))
  }

  if (development) {
    base <- sub("[.]9000$", "", current)
    if (numeric_version(version) <= numeric_version(base)) {
      release_abort(sprintf(
        "Release version `%s` must be newer than development base `%s`.",
        version,
        base
      ))
    }
    news <- release_replace_news_heading(news, current, version)
    description <- release_set_dcf_field(description, "Version", version)
  } else {
    news <- release_replace_news_heading(news, current, version)
  }

  files$DESCRIPTION <- description
  files$`NEWS.md` <- news
  files
}

release_readme_cran_parts <- function(lines) {
  c(
    badge = sum(grepl(
      "badges/version/marginplyr",
      lines,
      fixed = TRUE
    )),
    link = sum(grepl(
      "[CRAN](https://CRAN.R-project.org/package=marginplyr)",
      lines,
      fixed = TRUE
    )),
    install = sum(lines == "install.packages(\"marginplyr\")")
  )
}

release_readme_claims_cran <- function(lines) {
  identical(
    unname(release_readme_cran_parts(lines)),
    c(1L, 1L, 1L)
  )
}

release_published_installation <- function() {
  c(
    "`marginplyr` is available from",
    "[CRAN](https://CRAN.R-project.org/package=marginplyr):",
    "",
    "``` r",
    "install.packages(\"marginplyr\")",
    "```",
    "",
    "Install the development version from",
    "[GitHub](https://github.com/sayuks/marginplyr) with:",
    "",
    "``` r",
    "install.packages(\"pak\")",
    "pak::pkg_install(\"sayuks/marginplyr\")",
    "```"
  )
}

# Adds the publication-only README source exactly once.
release_publish_readme <- function(lines) {
  if (release_readme_claims_cran(lines)) {
    return(lines)
  }
  parts <- release_readme_cran_parts(lines)
  if (any(parts != 0L)) {
    release_abort(paste0(
      "README.Rmd has partial or duplicated CRAN publication text; ",
      "refusing to guess which initial-only edit is intended."
    ))
  }

  badge_end <- which(lines == "<!-- badges: end -->")
  if (length(badge_end) != 1L) {
    release_abort("README.Rmd must contain exactly one badge block.")
  }
  badge <- paste0(
    "[![CRAN status](https://www.r-pkg.org/badges/version/marginplyr)]",
    "(https://CRAN.R-project.org/package=marginplyr)"
  )
  lines <- append(lines, badge, after = badge_end - 1L)

  installation <- which(lines == "## Installation")
  if (length(installation) != 1L) {
    release_abort("README.Rmd must contain exactly one Installation heading.")
  }
  later_headings <- which(
    seq_along(lines) > installation & grepl("^## ", lines)
  )
  if (length(later_headings) == 0L) {
    release_abort(paste(
      "README.Rmd Installation must be followed by another section."
    ))
  }
  next_heading <- later_headings[[1L]]
  c(
    lines[seq_len(installation)],
    "",
    release_published_installation(),
    "",
    lines[next_heading:length(lines)]
  )
}

release_add_development_news <- function(lines, release, development) {
  if (identical(lines[[1L]], release_news_heading(development))) {
    return(lines)
  }
  expected <- release_news_heading(release)
  if (!identical(lines[[1L]], expected)) {
    release_abort(sprintf(
      "NEWS.md must begin `%s` before the development heading is added.",
      expected
    ))
  }
  c(release_news_heading(development), "", lines)
}

# Plans the deterministic state that follows confirmed CRAN publication.
release_post_files <- function(files, version) {
  version <- release_version(version)
  description <- files$DESCRIPTION
  status <- release_status(description)
  current <- release_dcf_field(description, "Version")
  development <- paste0(version, ".9000")

  if (!(current %in% c(version, development))) {
    release_abort(sprintf(
      paste0(
        "Post-release expected DESCRIPTION version `%s` or `%s`, not `%s`."
      ),
      version,
      development,
      current
    ))
  }

  if (identical(status, "unpublished")) {
    if (!identical(current, version)) {
      release_abort("Initial-only publication edits cannot start after a bump.")
    }
    files$`README.Rmd` <- release_publish_readme(files$`README.Rmd`)
    description <- release_set_dcf_field(
      description,
      "Config/marginplyr/cran-status",
      "published"
    )
  } else if (!release_readme_claims_cran(files$`README.Rmd`)) {
    release_abort(paste0(
      "Published update state is missing the initial-only CRAN README text; ",
      "refusing to recreate it as an update edit."
    ))
  }

  description <- release_set_dcf_field(
    description,
    "Version",
    development
  )
  files$DESCRIPTION <- description
  files$`NEWS.md` <- release_add_development_news(
    files$`NEWS.md`,
    version,
    development
  )
  files
}

# Verifies the requested release against the authoritative source index.
verify_cran_publication <- function(version, packages = NULL) {
  version <- release_version(version)
  if (is.null(packages)) {
    packages <- tryCatch(
      {
        connection <- url(
          "https://cran.r-project.org/src/contrib/PACKAGES.rds",
          open = "rb"
        )
        on.exit(close(connection), add = TRUE)
        readRDS(connection)
      },
      error = function(error) {
        release_abort(
          paste(
            "Could not read CRAN source package data:",
            conditionMessage(error)
          ),
          status = 2L
        )
      }
    )
  }
  if (
    is.null(dim(packages)) ||
      !all(c("Package", "Version") %in% colnames(packages))
  ) {
    release_abort("CRAN package data lacks Package and Version columns.", 2L)
  }
  matches <- which(packages[, "Package"] == "marginplyr")
  if (length(matches) != 1L) {
    release_abort(sprintf(
      "CRAN source data contains %d marginplyr record(s), expected one.",
      length(matches)
    ))
  }
  published <- unname(packages[matches, "Version"])
  if (!identical(published, version)) {
    release_abort(sprintf(
      "CRAN publishes marginplyr `%s`, not requested version `%s`.",
      published,
      version
    ))
  }
  version
}

release_git_status <- function(root) {
  output <- suppressWarnings(system2(
    "git",
    c("-C", shQuote(root), "status", "--porcelain=v1", "--untracked-files=all"),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) {
    release_abort(
      paste(
        "Could not inspect the release worktree:",
        paste(output, collapse = "\n")
      ),
      status = 2L
    )
  }
  output
}

release_assert_clean <- function(root) {
  status <- release_git_status(root)
  if (length(status) > 0L) {
    release_abort(paste0(
      "The release helper requires a clean worktree. Current status:\n",
      paste(status, collapse = "\n")
    ))
  }
  invisible(TRUE)
}

release_read_files <- function(root, paths) {
  missing <- paths[!file.exists(file.path(root, paths))]
  if (length(missing) > 0L) {
    release_abort(paste(
      "Missing required release file(s):",
      paste(missing, collapse = ", ")
    ))
  }
  stats::setNames(lapply(
    file.path(root, paths),
    readLines,
    warn = FALSE,
    encoding = "UTF-8"
  ), paths)
}

release_changed_files <- function(before, after) {
  names(after)[vapply(
    names(after),
    function(path) !identical(before[[path]], after[[path]]),
    logical(1)
  )]
}

release_print_diff <- function(path, before, after) {
  old <- tempfile("cran-release-old-")
  new <- tempfile("cran-release-new-")
  on.exit(unlink(c(old, new)), add = TRUE)
  writeLines(before, old, useBytes = TRUE)
  writeLines(after, new, useBytes = TRUE)
  output <- suppressWarnings(system2(
    "diff",
    c(
      "-u", "--label", shQuote(paste0("a/", path)),
      "--label", shQuote(paste0("b/", path)),
      shQuote(old), shQuote(new)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (!(status %in% c(0L, 1L))) {
    release_abort(
      paste(
        "Could not render the preview diff:",
        paste(output, collapse = "\n")
      ),
      status = 2L
    )
  }
  writeLines(output)
}

release_write_atomic <- function(path, lines) {
  temporary <- tempfile("cran-release-write-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  writeLines(lines, temporary, useBytes = TRUE)
  if (!file.rename(temporary, path)) {
    release_abort(paste("Could not replace", path), status = 2L)
  }
  invisible(path)
}

release_command <- function(command, args, root) {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  output <- suppressWarnings(system2(
    command,
    args,
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = paste(output, collapse = "\n"))
}

# Regenerates README.md with the repository-pinned Pandoc after installing the
# edited tree into a disposable library, so its chunks never load an old build.
release_render_readme <- function(root, runner = release_command) {
  library <- tempfile("marginplyr-release-library-")
  dir.create(library)
  on.exit(unlink(library, recursive = TRUE), add = TRUE)

  install <- runner(
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "INSTALL", "--no-multiarch",
      paste0("--library=", shQuote(library)),
      shQuote(root)
    ),
    root
  )
  if (!identical(install$status, 0L)) {
    release_abort(
      paste("Temporary package installation failed:", install$output),
      status = 2L
    )
  }

  expression <- paste0(
    ".libPaths(c(", deparse(library), ", .libPaths())); ",
    "if (as.character(rmarkdown::pandoc_version()) != '3.10.1') ",
    "stop('README regeneration requires Pandoc 3.10.1.'); ",
    "rmarkdown::render('README.Rmd', quiet = TRUE)"
  )
  render <- runner(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(expression)),
    root
  )
  if (!identical(render$status, 0L)) {
    release_abort(
      paste("README regeneration failed:", render$output),
      status = 2L
    )
  }
  invisible(file.path(root, "README.md"))
}

cran_release_operation <- function(
  operation,
  version,
  root = getwd(),
  apply = FALSE,
  packages = NULL,
  runner = release_command
) {
  root <- normalizePath(root, mustWork = TRUE)
  description <- file.path(root, "DESCRIPTION")
  if (!file.exists(description)) {
    release_abort(
      "Run the release helper from the marginplyr repository root.",
      2L
    )
  }
  package <- release_dcf_field(
    readLines(description, warn = FALSE),
    "Package"
  )
  if (!identical(package, "marginplyr")) {
    release_abort(
      "Run the release helper from the marginplyr repository root.",
      2L
    )
  }
  release_assert_clean(root)

  if (identical(operation, "verify-publication")) {
    verified <- verify_cran_publication(version, packages = packages)
    message("CRAN publishes marginplyr ", verified, ".")
    return(invisible(character()))
  }

  paths <- if (identical(operation, "prepare")) {
    c("DESCRIPTION", "NEWS.md")
  } else if (identical(operation, "post-release")) {
    verify_cran_publication(version, packages = packages)
    c("DESCRIPTION", "NEWS.md", "README.Rmd")
  } else {
    release_abort(paste("Unknown release operation:", operation), 2L)
  }
  before <- release_read_files(root, paths)
  after <- if (identical(operation, "prepare")) {
    release_prepare_files(before, version)
  } else {
    release_post_files(before, version)
  }
  changed <- release_changed_files(before, after)

  if (identical(operation, "prepare")) {
    message(paste0(
      "After manual source edits, regenerate with ",
      "`Rscript -e 'roxygen2::roxygenise()'`; install the working tree; ",
      "then run `Rscript -e ",
      "'rmarkdown::render(\"README.Rmd\", quiet = TRUE)'`."
    ))
  }

  if (length(changed) == 0L) {
    message(operation, " is already complete for ", version, ".")
    return(invisible(changed))
  }
  if (!isTRUE(apply)) {
    message("Preview only; no tracked file was changed.")
    for (path in changed) {
      release_print_diff(path, before[[path]], after[[path]])
    }
    if (identical(operation, "post-release")) {
      message("README.md will be regenerated with Pandoc 3.10.1 on --apply.")
    }
    return(invisible(changed))
  }

  for (path in changed) {
    release_write_atomic(file.path(root, path), after[[path]])
  }
  if ("README.Rmd" %in% changed) {
    release_render_readme(root, runner = runner)
    rendered <- readLines(file.path(root, "README.md"), warn = FALSE)
    if (!release_readme_claims_cran(rendered)) {
      release_abort(
        "Regenerated README.md does not contain both CRAN publication claims.",
        status = 2L
      )
    }
  }
  message(
    "Applied ", operation, " edits: ",
    paste(changed, collapse = ", "), "."
  )
  invisible(changed)
}

parse_cran_release_args <- function(args) {
  usage <- paste(
    "Usage:",
    "  Rscript tools/cran-release.R prepare --version <version> [--apply]",
    "  Rscript tools/cran-release.R verify-publication --version <version>",
    "  Rscript tools/cran-release.R post-release --version <version> [--apply]",
    sep = "\n"
  )
  if (length(args) == 1L && args[[1L]] %in% c("-h", "--help")) {
    writeLines(usage)
    return(list(help = TRUE))
  }
  if (length(args) == 0L) release_abort(usage, 2L)
  operation <- args[[1L]]
  args <- args[-1L]
  version_at <- which(args == "--version")
  if (length(version_at) != 1L || version_at[[1L]] == length(args)) {
    release_abort(usage, 2L)
  }
  version <- args[[version_at + 1L]]
  consumed <- c(version_at, version_at + 1L)
  apply <- "--apply" %in% args
  consumed <- c(consumed, which(args == "--apply"))
  if (length(setdiff(seq_along(args), consumed)) > 0L) {
    release_abort(usage, 2L)
  }
  if (identical(operation, "verify-publication") && apply) {
    release_abort("verify-publication is always read-only.", 2L)
  }
  list(
    operation = operation,
    version = release_version(version),
    apply = apply,
    help = FALSE
  )
}

cran_release_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  tryCatch({
    parsed <- parse_cran_release_args(args)
    if (isTRUE(parsed$help)) return(0L)
    cran_release_operation(
      parsed$operation,
      parsed$version,
      apply = parsed$apply
    )
    0L
  }, marginplyr_release_error = function(error) {
    writeLines(conditionMessage(error), stderr())
    error$status
  }, error = function(error) {
    writeLines(conditionMessage(error), stderr())
    2L
  })
}

if (sys.nframe() == 0L) {
  quit(status = cran_release_cli(), save = "no")
}
