# Classifies the complete normalized text of one R CMD check NOTE. Local
# preflight and release-matrix checks source this file so strict release policy
# has one allowlist (#505).

# Removes presentation-only variation while retaining every substantive line.
# The allowlist patterns below are anchored against this representation, so a
# second incoming finding cannot inherit the disposition of the first.
normalize_cran_note <- function(note) {
  note <- gsub("\033\\[[0-9;]*[[:alpha:]]", "", note)
  note <- chartr("‘’", "''", note)
  lines <- strsplit(gsub("\r\n?", "\n", note), "\n", fixed = TRUE)[[1L]]
  lines <- trimws(lines)
  if (length(lines) > 0L) {
    lines[[1L]] <- sub("^[*][[:space:]]+", "", lines[[1L]])
    lines[[1L]] <- sub(
      "[[:space:]]+\\[[0-9.]+(?:ms|s|m|h)/[0-9.]+(?:ms|s|m|h)\\](?=[[:space:]]+NOTE$)",
      "",
      lines[[1L]],
      perl = TRUE
    )
  }
  paste(lines[nzchar(lines)], collapse = "\n")
}

# Every entry carries the evidence needed to apply it and to explain it. The
# signature describes the whole normalized NOTE rather than a fragment.
cran_note_policy <- function() {
  list(
    new_submission = list(
      signature = paste0(
        "^checking CRAN incoming feasibility [.]{3} NOTE\\n",
        "Maintainer: '[^'\\n]+ <[^>\\n]+>'\\n",
        "New submission$"
      ),
      states = "unpublished",
      rationale = paste(
        "CRAN reports an initial submission before marginplyr is published."
      ),
      marker = "New submission"
    )
  )
}

# Returns the release disposition and the policy evidence for one NOTE.
classify_cran_note <- function(note, cran_status) {
  normalized <- normalize_cran_note(note)
  policies <- cran_note_policy()
  matched <- vapply(
    policies,
    function(policy) {
      cran_status %in% policy$states &&
        grepl(policy$signature, normalized, perl = TRUE)
    },
    logical(1)
  )
  if (!any(matched)) {
    return(list(
      status = "unknown-note",
      normalized = normalized,
      policy = NA_character_,
      rationale = NA_character_,
      marker = NA_character_
    ))
  }

  name <- names(policies)[which(matched)[[1L]]]
  policy <- policies[[name]]
  list(
    status = "allowed-note",
    normalized = normalized,
    policy = name,
    rationale = policy$rationale,
    marker = policy$marker
  )
}

# Routine CI reports unknown NOTEs without turning a moving R-devel signal into
# noise. A manually dispatched release run fails closed on the same result.
cran_notes_block <- function(classifications, strict) {
  isTRUE(strict) && any(vapply(
    classifications,
    function(note) identical(note$status, "unknown-note"),
    logical(1)
  ))
}

# Reads submission state from the artifact being checked, not from the checkout
# beside it. A release-matrix job therefore classifies the tarball it received.
cran_status_from_tarball <- function(tarball) {
  contents <- utils::untar(tarball, list = TRUE)
  descriptions <- contents[grepl("^[^/]+/DESCRIPTION$", contents)]
  if (length(descriptions) != 1L) {
    stop("The source tarball must contain exactly one package DESCRIPTION.")
  }
  extracted <- tempfile("marginplyr-description-")
  dir.create(extracted)
  on.exit(unlink(extracted, recursive = TRUE), add = TRUE)
  utils::untar(tarball, files = descriptions, exdir = extracted)
  description <- read.dcf(file.path(extracted, descriptions))
  field <- "Config/marginplyr/cran-status"
  if (!(field %in% colnames(description))) {
    stop("The source tarball DESCRIPTION is missing `", field, "`.")
  }
  trimws(description[[1L, field]])
}
