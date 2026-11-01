# Checks accepted capability comparisons without launching verification work.
run_capability_contracts <- function() {
  root <- tempfile("runner-capabilities-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  rr_run("git", c("clone", "--quiet", "--shared", "--no-checkout", candidate, root))
  accepted <- rr_git(candidate, "rev-parse", "HEAD")
  rr_git(root, "checkout", "--quiet", "--detach", accepted)
  rr_git(root, "config", "user.name", "Capability fixture")
  rr_git(root, "config", "user.email", "fixture@example.invalid")
  selection <- list(path = "Repository", verifiers = list(".github/scripts/verify-doc-references.R"),
    plan = list(units = list()))
  expect(is.null(rr_eligibility(root, accepted, selection, accepted)), "Matching accepted sources permit the registered workload")
  unknown <- selection; unknown$verifiers <- list("tools/unclassified-suite.R")
  expect(grepl("Unregistered limited workload", rr_eligibility(root, accepted, unknown, accepted), fixed = TRUE),
    "A declaration cannot grant an unclassified suite a limited capability")
  bounded <- rr_eligibility
  environment(bounded) <- new.env(parent = environment(rr_eligibility))
  for (kind in c("unclassified", "parallel-fixture-4")) {
    registration <- rr_stage_contracts
    registration[[selection$verifiers[[1L]]]][[1L]] <- kind
    environment(bounded)$rr_stage_contracts <- registration
    expect(grepl("Unregistered limited workload", bounded(root, accepted, selection, accepted), fixed = TRUE),
      paste("Limited work refuses an unbounded registration:", kind))
  }
  for (path in c(".github/scripts/verify-doc-references.R", ".github/scripts/ci-helpers.R", "tools/review-ready-registry.R")) {
    rr_git(root, "checkout", "--quiet", "--detach", "-f", accepted)
    cat("\n# Changed capability source.\n", file = file.path(root, path), append = TRUE)
    rr_git(root, "add", "."); rr_git(root, "commit", "-qm", "Changed capability fixture")
    sha <- rr_git(root, "rev-parse", "HEAD")
    expect(grepl("differs from pinned main", rr_eligibility(root, sha, selection, accepted), fixed = TRUE),
      paste("Protocol equality cannot accept changed checker, helper or registration bytes:", path))
  }
  cat("Accepted workload, helper and registration capability boundaries passed.\n")
}
run_capability_contracts()
