# Accepted workload registrations are read from pinned local main, never from
# the candidate's declaration of its own capabilities (ADR 0039).
rr_runner_sources <- c("tools/review-ready.R", "tools/review-ready-core.R",
  "tools/review-ready-registry.R", "tools/review-ready-manager.R", "tools/review-ready-evidence.R",
  "tools/review-ready-signals.c")

rr_expectation <- ".github/scripts/verify-cran-preflight.R"
rr_vector <- paste0('(expect_identical\\(\\s*review_requirements\\$package,\\s*)',
  '(c\\(\\s*"[A-Za-z][A-Za-z0-9.]*"(?:\\s*,\\s*"[A-Za-z][A-Za-z0-9.]*")*\\s*\\))',
  '(,\\s*"the review dependency set"\\s*\\))')
rr_protected_expectation <- function(bytes) {
  text <- rawToChar(bytes)
  matches <- gregexpr(rr_vector, text, perl = TRUE)[[1L]]
  if (length(matches) != 1L || matches[[1L]] < 0L) return(NULL)
  charToRaw(sub(rr_vector, "\\1<review dependency vector>\\3", text, perl = TRUE))
}

rr_comparison_inputs <- function(root) {
  overrides <- c("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
    "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES", "GIT_NAMESPACE",
    "GIT_REPLACE_REF_BASE", "GIT_ATTR_SOURCE", "GIT_TEMPLATE_DIR", "GIT_EXEC_PATH")
  if (any(nzchar(Sys.getenv(overrides)))) rr_stop("Unregistered Git context override.")
  if (nzchar(rr_git(root, "replace", "-l"))) rr_stop("Unregistered Git replacement objects.")
  graft <- rr_git(root, "rev-parse", "--path-format=absolute", "--git-path", "info/grafts")
  if (file.exists(graft)) rr_stop("Unregistered Git grafts input.")
  invisible(TRUE)
}

rr_ordinary_text <- function(bytes) {
  if (any(as.integer(bytes) < 32L & !as.integer(bytes) %in% c(9L, 10L, 13L))) return(FALSE)
  !is.na(iconv(rawToChar(bytes), "UTF-8", "UTF-8"))
}

# The raw NUL-delimited final-tree diff includes both rename sides and deletes.
rr_select <- function(root, sha, main) {
  selection <- list(submitted = sha, main = main, base = NULL, path = "Full", reasons = list(), verifiers = list())
  tryCatch({
    rr_comparison_inputs(root)
    bases <- strsplit(rr_git(root, "merge-base", "--all", sha, main), "\n", fixed = TRUE)[[1L]]
    if (length(bases) != 1L || !rr_match("[0-9a-f]{40}", bases)) rr_stop("Missing or ambiguous merge-base.")
    selection$base <- bases[[1L]]
    records <- rr_split0(rr_git_raw(root, "diff", "--raw", "-z", "--no-abbrev", "--no-ext-diff",
      "--no-textconv", "--ignore-submodules=none", "--no-relative", "--find-renames", bases, sha, "--"))
    if (!length(records)) {
      rr_comparison_inputs(root)
      selection$reasons <- list("Empty cumulative diff; Full retains the established comparison base.")
      selection$verifiers <- as.list(c(unlist(rr_reference_checks), "tools/verify-ci-artifacts.py"))
      return(selection)
    }
    names <- character()
    recognized <- character()
    reasons <- character()
    documentation <- FALSE
    roxygen <- FALSE
    index <- 1L
    while (index <= length(records)) {
      header <- regmatches(records[[index]], regexec(
        "^:([0-7]{6}) ([0-7]{6}) ([0-9a-f]{40}) ([0-9a-f]{40}) ([AMDTRCUXB])([0-9]*)$", records[[index]], perl = TRUE))[[1L]]
      if (length(header) != 7L) rr_stop("Unrecognized raw diff record.")
      before <- header[[2L]]; after <- header[[3L]]; status <- header[[6L]]
      count <- if (status %in% c("R", "C")) 2L else 1L
      if (index + count > length(records)) rr_stop("Missing diff path.")
      paths <- records[seq.int(index + 1L, index + count)]
      if (!all(nzchar(paths)) || anyNA(iconv(paths, "UTF-8", "UTF-8"))) rr_stop("Unsupported diff filename encoding.")
      names <- c(names, paths)
      if (identical(paths, rr_expectation) && status == "M" && before == "100644" && after == before) {
        old <- rr_protected_expectation(rr_git_raw(root, "cat-file", "blob", header[[4L]]))
        new <- rr_protected_expectation(rr_git_raw(root, "cat-file", "blob", header[[5L]]))
        if (!is.null(old) && identical(old, new)) recognized <- c(recognized, paths)
      }
      modes <- setdiff(c(before, after), "000000")
      if (!setequal(modes, "100644") || (before != after && !status %in% c("A", "D")))
        reasons <- c(reasons, paste("Unsupported file mode/type:", rr_json(as.list(paths))))
      if (!status %in% c("A", "M", "D", "R") || (status == "R" && !nzchar(header[[7L]])))
        reasons <- c(reasons, paste("Unsupported change status:", rr_json(as.list(paths))))
      for (name in setdiff(paths, recognized)) {
        doc <- rr_match(rr_documentation, name)
        prose <- rr_match(rr_prose, name) || any(vapply(rr_artifacts, function(route) rr_match(route[[1L]], name), logical(1)))
        documentation <- documentation || doc
        if (rr_match("R/[^/]+[.]R", name) && status == "M" && before == after && before == "100644") {
          roxygen <- TRUE; documentation <- TRUE
        } else if (!doc && !prose) reasons <- c(reasons, paste("Unsupported path:", rr_json(name)))
      }
      if (all(vapply(paths, function(name) rr_match(rr_prose, name) || rr_match(rr_documentation, name) ||
          any(vapply(rr_artifacts, function(route) rr_match(route[[1L]], name), logical(1))), logical(1)))) {
        for (side in 1:2) {
          mode <- if (side == 1L) before else after
          name <- if (side == 1L) paths[[1L]] else tail(paths, 1L)
          if (mode != "100644" || grepl("[.](png|jpg|jpeg|gif|pdf)$", name)) next
          bytes <- rr_git_raw(root, "cat-file", "blob", header[[side + 3L]])
          if (!rr_ordinary_text(bytes)) reasons <- c(reasons, paste("Unsupported prose content:", rr_json(name)))
        }
      }
      index <- index + count + 1L
    }
    verifiers <- unlist(rr_reference_checks, use.names = FALSE)
    for (route in c(rr_artifacts, rr_contracts)) {
      if (any(rr_match(route[[1L]], setdiff(names, recognized)))) verifiers <- union(verifiers, unlist(route[[2L]], use.names = FALSE))
    }
    if (any(rr_match(rr_checking, names))) verifiers <- union(verifiers,
      c("tools/test-review-ready-runner.R", "tools/test-review-ready-renderer.R"))
    if (length(recognized)) {
      verifiers <- union(verifiers, c("tools/review-ready-followup-contract.R", rr_expectation, ".github/scripts/verify-review-ready-check.R"))
      selection$expectation_followup <- TRUE
    }
    selection$verifiers <- as.list(verifiers)
    if (!length(reasons)) {
      selection$path <- if (documentation) "Documentation" else "Repository"
      selection$roxygen_pending <- roxygen
      reasons <- if (roxygen) "R documentation eligibility pending supervised proof" else "All cumulative changes have registered artifact routes."
    }
    selection$reasons <- as.list(reasons)
    rr_comparison_inputs(root)
    selection
  }, error = function(e) {
    selection$path <- "Full"
    selection$base <- NULL
    selection$reasons <- list(paste("Comparison unavailable:", conditionMessage(e)))
    selection$verifiers <- as.list(c(unlist(rr_reference_checks), "tools/verify-ci-artifacts.py"))
    selection
  })
}

rr_capabilities <- function(root, sha, patterns) {
  tree <- rr_tree(root, sha)
  expressions <- vapply(patterns, glob2rx, "")
  matches <- Reduce(`|`, lapply(expressions, grepl, x = names(tree)))
  selected <- tree[matches]
  literal <- patterns[!grepl("[?*\\[]", patterns)]
  if (!all(literal %in% names(selected))) rr_stop("Missing accepted contract sources: ", paste(setdiff(literal, names(selected)), collapse = ", "))
  blobs <- rr_blobs(root, vapply(selected, `[[`, "", "oid"))
  lapply(selected, function(entry) {
    if (!entry$mode %in% c("100644", "100755") || entry$kind != "blob") rr_stop("Unsupported contract source type: ", entry$name)
    bytes <- blobs[[entry$oid]]
    if (entry$name == rr_expectation) {
      protected <- rr_protected_expectation(bytes)
      if (!is.null(protected)) bytes <- protected
    }
    list(mode = entry$mode, digest = rr_hash(bytes))
  })
}

rr_eligibility <- function(root, sha, selection, main) {
  if (rr_full_required(selection)) return(NULL)
  patterns <- c(rr_runner_sources, unlist(rr_core_sources))
  if (selection$path == "Documentation") patterns <- c(patterns, unlist(rr_documentation_sources))
  for (stage in unlist(selection$verifiers)) {
    registration <- rr_stage_contracts[[stage]]
    if (is.null(registration) || !registration[[1L]] %in% c("serial", "process-fixture", "parallel-fixture-2")) return(paste("Unregistered limited workload:", stage))
    patterns <- c(patterns, stage, unlist(registration[[2L]]))
  }
  if (!identical(rr_capabilities(root, main, unique(patterns)), rr_capabilities(root, sha, unique(patterns))))
    return("Selected workload source differs from pinned main's accepted registration.")
  NULL
}

rr_prose <- "(AGENTS\\.md|CLAUDE\\.md|CONTEXT\\.md|CODE_OF_CONDUCT\\.md|(?:design|investigation)/(?:[^/]+/)*[^/]+\\.md|tools/[^/]+\\.md)"

rr_documentation <- "(README\\.(?:md|qmd)|NEWS\\.md|man/[^/]+\\.Rd|man/figures/(?:[^/]+/)*[^/]+\\.(?:png|jpg|jpeg|gif|svg|pdf)|vignettes/[^/]+\\.qmd|vignettes/(?:[^/]+/)*[^/]+\\.(?:png|jpg|jpeg|gif|svg|pdf|bib|csl))"

rr_generated_inputs <- "(README\\.(?:md|qmd)|NAMESPACE|R/.*|DESCRIPTION|inst/.*|data/.*|man/.*|vignettes/.*)"

rr_reference_checks <- list(".github/scripts/verify-doc-references.R",
  ".github/scripts/verify-context-budget.R",
  ".github/scripts/verify-verifier-invocation.R")

rr_workflows <- list("R-CMD-check.yaml",
  "altdoc.yaml",
  "document.yaml",
  "lint.yaml",
  "postgres-transactions.yaml",
  "release-matrix.yaml",
  "rhub.yaml",
  "test-coverage.yaml")

rr_artifacts <- list(list("\\.github/workflows/(R\\-CMD\\-check\\.yaml|altdoc\\.yaml|document\\.yaml|lint\\.yaml|postgres\\-transactions\\.yaml|release\\-matrix\\.yaml|rhub\\.yaml|test\\-coverage\\.yaml)",
  list("tools/verify-ci-artifacts.py")),
  list("\\.github/scripts/summarize-check-log\\.R",
  list("tools/test-ci-artifacts.py")),
  list("altdoc/quarto_website\\.yml",
  list("tools/verify-site-artifacts.R")),
  list("codecov\\.yml",
  list(".github/scripts/verify-coverage-policy.R")),
  list("\\.(?:codex/config\\.toml|claude/settings\\.json)",
  list(".github/scripts/verify-agent-network-policy.R",
  ".github/scripts/verify-codex-config.R",
  "tools/verify-ci-artifacts.py")))

rr_checking <- "(?:tools/(?:review-ready[^/]*|test-review-ready[^/]*|install-review-ready\\.py|dependency-requirements\\.R|required-test-suggests\\.R|verify-ci-artifacts\\.py|test-ci-artifacts\\.py|ci-workflow-contracts\\.json|verify-site-artifacts\\.R)|\\.github/scripts/(?:cran-note-policy\\.R|ci-helpers\\.R|verify-(?:review-ready-check|doc-references|context-budget|verifier-invocation|site|coverage-policy|cran-preflight|agent-network-policy|codex-config)\\.R))"

rr_contracts <- list(list("design/(?:agents/local-checks\\.md|specs/review-ready[^/]*\\.md)",
  list(".github/scripts/verify-review-ready-check.R",
  "tools/test-review-ready-documentation.R",
  "tools/test-review-ready-roxygen.R")),
  list("(README\\.(?:md|qmd)|NAMESPACE|R/.*|DESCRIPTION|inst/.*|data/.*|man/.*|vignettes/.*)",
  list("tools/review-ready-generated.R")),
  list("\\.github/scripts/verify-cran-preflight\\.R",
  list(".github/scripts/verify-cran-preflight.R")),
  list("\\.github/scripts/verify-coverage-policy\\.R",
  list(".github/scripts/verify-coverage-policy.R")),
  list("\\.github/scripts/verify-agent-network-policy\\.R",
  list(".github/scripts/verify-agent-network-policy.R")),
  list("\\.github/scripts/verify-codex-config\\.R",
  list(".github/scripts/verify-codex-config.R")),
  list("tools/cran-release\\.md",
  list(".github/scripts/verify-cran-release.R")),
  list("tools/cran-preflight\\.md",
  list(".github/scripts/verify-cran-preflight.R",
  ".github/scripts/verify-agent-network-policy.R",
  ".github/scripts/verify-codex-config.R")),
  list("(?:tools/(?:review-ready[^/]*|test-review-ready[^/]*|install-review-ready\\.py|dependency-requirements\\.R|required-test-suggests\\.R|verify-ci-artifacts\\.py|test-ci-artifacts\\.py|ci-workflow-contracts\\.json|verify-site-artifacts\\.R)|\\.github/scripts/(?:cran-note-policy\\.R|ci-helpers\\.R|verify-(?:review-ready-check|doc-references|context-budget|verifier-invocation|site|coverage-policy|cran-preflight|agent-network-policy|codex-config)\\.R))",
  list(".github/scripts/verify-review-ready-check.R",
  "tools/test-review-ready-admission.py",
  "tools/test-review-ready-progress.R",
  "tools/test-review-ready-evidence.py",
  "tools/test-ci-artifacts.py",
  "tools/verify-ci-artifacts.py",
  "tools/test-review-ready-documentation.R",
  "tools/test-review-ready-roxygen.R")),
  list("\\.github/.*",
  list("tools/verify-ci-artifacts.py")),
  list("(?:altdoc/.*|\\.(?:codex|claude)/.*)",
  list("tools/verify-ci-artifacts.py")),
  list("(?:\\.covrignore|inst/\\.covrignore|tools/coverage-check(?:-lib)?\\.R)",
  list(".github/scripts/verify-coverage-policy.R")),
  list("(?:tools/verify-site-artifacts\\.R|\\.github/scripts/verify-site\\.R)",
  list("tools/verify-site-artifacts.R")))

rr_core_sources <- list("tools/review-ready-check.R",
  "tools/review-ready-check-lib.R",
  "tools/review-ready-progress.R",
  "tools/review-ready-progress-reporter.R",
  "tools/dependency-requirements.R",
  "tools/required-test-suggests.R",
  "tests/testthat/helper-optional-backends.R",
  ".github/scripts/cran-note-policy.R",
  "DESCRIPTION",
  "NAMESPACE",
  ".Rbuildignore",
  "jarl.toml",
  ".lintr*",
  ".Rprofile*",
  "tools/review-ready-roxygen.R",
  "tools/review-ready-evidence.py",
  "tools/review-ready-environment.R",
  "tools/review-ready-followup-contract.R",
  "inst/suggests/guard.R")

rr_documentation_sources <- list("tools/review-ready-documentation.R",
  "tests/testthat/test-documentation.R",
  "tests/testthat/test-duckdb-storage.R",
  "tests/testthat/test-package-metadata.R",
  "tests/testthat/helper*.R",
  "tests/testthat/setup*.R",
  "tests/testthat/teardown*.R",
  "inst/suggests/guard.R")

rr_stage_contracts <- list(
  "tools/review-ready-followup-contract.R" = list("serial",
  list("tools/dependency-requirements.R",
  "tools/review-ready-generated.R",
  "DESCRIPTION")),
  "tools/test-review-ready-evidence.py" = list("process-fixture",
  list("tools/review-ready-evidence.py",
  "tools/review-ready-environment.R",
  "tools/review-ready-followup-contract.R",
  "tools/review-ready-admission.py",
  "tools/test-review-ready-admission.py")),
  ".github/scripts/verify-doc-references.R" = list("serial",
  list(".github/scripts/ci-helpers.R")),
  ".github/scripts/verify-context-budget.R" = list("serial",
  list(".github/scripts/ci-helpers.R")),
  ".github/scripts/verify-verifier-invocation.R" = list("serial",
  list(".github/scripts/ci-helpers.R")),
  ".github/scripts/verify-review-ready-check.R" = list("serial",
  list("tools/review-ready-check.R",
  "tools/review-ready-check-lib.R",
  "tools/review-ready-progress.R",
  "tools/review-ready-progress-reporter.R",
  "tools/dependency-requirements.R",
  "tools/required-test-suggests.R",
  "tests/testthat/helper-optional-backends.R",
  ".github/scripts/cran-note-policy.R",
  "DESCRIPTION",
  "NAMESPACE",
  ".Rbuildignore",
  "jarl.toml",
  ".lintr*",
  ".Rprofile*",
  "tools/review-ready-roxygen.R",
  "tools/review-ready-evidence.py",
  "tools/review-ready-environment.R",
  "tools/review-ready-followup-contract.R",
  "inst/suggests/guard.R")),
  "tools/test-review-ready-documentation.R" = list("serial",
  list("tools/review-ready-check.R",
  "tools/review-ready-check-lib.R",
  "tools/review-ready-progress.R",
  "tools/review-ready-progress-reporter.R",
  "tools/dependency-requirements.R",
  "tools/required-test-suggests.R",
  "tests/testthat/helper-optional-backends.R",
  ".github/scripts/cran-note-policy.R",
  "DESCRIPTION",
  "NAMESPACE",
  ".Rbuildignore",
  "jarl.toml",
  ".lintr*",
  ".Rprofile*",
  "tools/review-ready-roxygen.R",
  "tools/review-ready-evidence.py",
  "tools/review-ready-environment.R",
  "tools/review-ready-followup-contract.R",
  "inst/suggests/guard.R",
  "tools/review-ready-documentation.R",
  "tools/review-ready-generated.R")),
  "tools/test-review-ready-admission.py" = list("process-fixture",
  list("tools/review-ready-admission.py",
  "tools/review-ready-check.R",
  "tools/install-review-ready.py",
  ".github/scripts/verify-doc-references.R",
  ".github/scripts/ci-helpers.R")),
  "tools/test-review-ready-progress.R" = list("parallel-fixture-2",
  list("tools/review-ready-check.R",
  "tools/review-ready-check-lib.R",
  "tools/review-ready-progress.R",
  "tools/review-ready-progress-reporter.R",
  "tools/dependency-requirements.R",
  "tools/required-test-suggests.R",
  "tests/testthat/helper-optional-backends.R",
  ".github/scripts/cran-note-policy.R",
  "DESCRIPTION",
  "NAMESPACE",
  ".Rbuildignore",
  "jarl.toml",
  ".lintr*",
  ".Rprofile*",
  "tools/review-ready-roxygen.R",
  "tools/review-ready-evidence.py",
  "tools/review-ready-environment.R",
  "tools/review-ready-followup-contract.R",
  "inst/suggests/guard.R",
  "tests/testthat.R")),
  "tools/test-review-ready-roxygen.R" = list("serial",
  list("tools/review-ready-check.R",
  "tools/review-ready-check-lib.R",
  "tools/review-ready-progress.R",
  "tools/review-ready-progress-reporter.R",
  "tools/dependency-requirements.R",
  "tools/required-test-suggests.R",
  "tests/testthat/helper-optional-backends.R",
  ".github/scripts/cran-note-policy.R",
  "DESCRIPTION",
  "NAMESPACE",
  ".Rbuildignore",
  "jarl.toml",
  ".lintr*",
  ".Rprofile*",
  "tools/review-ready-roxygen.R",
  "tools/review-ready-evidence.py",
  "tools/review-ready-environment.R",
  "tools/review-ready-followup-contract.R",
  "inst/suggests/guard.R")),
  "tools/review-ready-generated.R" = list("serial",
  list()),
  ".github/scripts/verify-cran-release.R" = list("serial",
  list("tools/cran-release.R",
  "tools/dependency-requirements.R")),
  ".github/scripts/verify-cran-preflight.R" = list("serial",
  list(".github/scripts/cran-note-policy.R",
  ".github/scripts/checktor-baseline.R",
  ".github/scripts/verify-agent-network-policy.R",
  "tools/dependency-requirements.R",
  "tools/cran-preflight-lib.R",
  "tools/cran-preflight.R")),
  ".github/scripts/verify-agent-network-policy.R" = list("serial",
  list()),
  ".github/scripts/verify-codex-config.R" = list("serial",
  list()),
  ".github/scripts/verify-coverage-policy.R" = list("serial",
  list("tools/coverage-check-lib.R",
  "tools/required-test-suggests.R")),
  "tools/verify-ci-artifacts.py" = list("serial",
  list("tools/review-ready-admission.py",
  "tools/ci-workflow-contracts.json",
  ".github/scripts/verify-verifier-invocation.R",
  ".github/scripts/ci-helpers.R")),
  "tools/test-ci-artifacts.py" = list("serial",
  list("tools/verify-ci-artifacts.py",
  "tools/review-ready-admission.py",
  "tools/ci-workflow-contracts.json",
  ".github/scripts/summarize-check-log.R",
  ".github/scripts/verify-doc-references.R",
  ".github/scripts/verify-verifier-invocation.R",
  ".github/scripts/ci-helpers.R")),
  "tools/verify-site-artifacts.R" = list("serial",
  list(".github/scripts/verify-site.R",
  ".github/scripts/ci-helpers.R"))
)
rr_stage_contracts[["tools/test-review-ready-renderer.R"]] <- list("serial", as.list(c(
  rr_runner_sources, "tools/review-ready-check-lib.R", "tools/review-ready-generated.R",
  "tools/verify-site-artifacts.R", ".github/scripts/verify-site.R", ".github/scripts/ci-helpers.R")))
rr_stage_contracts[["tools/test-review-ready-runner.R"]] <- list("process-fixture", as.list(c(
  rr_runner_sources, "tools/review-ready-environment.R", "tools/review-ready-check.R",
  "tools/review-ready-check-lib.R", "tools/test-review-ready-inherited.R", "tools/verify-site-artifacts.R")))
