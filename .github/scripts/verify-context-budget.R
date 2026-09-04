# Fails when the always-loaded agent context grows past the baseline below.
#
# `CLAUDE.md` is read before every task an agent runs here, and each `@path` in
# it pulls that file in too, so the closure is loaded whether or not the task
# reaches what it says. Nothing else in the repository fails when that closure
# grows: prose costs no test and breaks no build, which is why it grew to 618
# lines before anyone measured it. A budget is the only thing that makes an
# addition cost something at the moment it is made.
#
# The baseline is a ratchet and not a target. It records what the closure
# measured when it was last deliberately set, so an addition is paid for by a
# deletion until someone decides otherwise and moves it in the same commit that
# spends it. A number chosen for how much the closure ought to be would be
# invented; this one is measured.
#
# The closure is derived from `CLAUDE.md` rather than listed, for the reason
# every other gate here derives: a list would name what it named when it was
# written, and a file added to the closure by a new `@path` would escape the
# budget it was added to. A derivation that stopped working reports a closure of
# one small file and passes, so the mechanism is asserted before the total is.

source(".github/scripts/ci-helpers.R")

# What the closure measured when it was last set, and the commit that set it.
# Moving this means saying in the same commit what was added and what paid for
# it.
baseline_bytes <- 22005L
baseline_note <- paste(
  "#418 deleting the gate's argument from *Chunks that must fail*, which",
  "`verify-must-error.R`'s header and `release-matrix.yaml`'s already held,",
  "together with the reasons `inst/vignette-hooks/must-error.R` states beside",
  "the code each is about and a claim its review found false, over #416's",
  "baseline; nothing was added"
)

entry <- "CLAUDE.md"

# Reads `@path` references the way the harness does: one per line, leading
# whitespace allowed. A reference inside a fenced block is still a reference for
# this purpose -- the budget is what the file costs, and a reader that resolves
# it costs that whether the fence was meant to quote it or not.
references <- function(path) {
  lines <- readLines(path, warn = FALSE)
  found <- regmatches(lines, regexpr("^\\s*@\\S+", lines))
  sub("^\\s*@", "", found)
}

closure <- function(root) {
  seen <- character()
  pending <- root
  while (length(pending) > 0L) {
    path <- pending[[1L]]
    pending <- pending[-1L]
    if (path %in% seen) {
      next
    }
    if (!file.exists(path)) {
      stop(call. = FALSE, sprintf(
        paste0(
          "%s names `@%s`, which no file answers. An agent reaches the ",
          "closure through these references, so one that resolves to nothing ",
          "is context the task was promised and did not get."
        ),
        entry, path
      ))
    }
    seen <- c(seen, path)
    pending <- c(pending, references(path))
  }
  seen
}

if (!file.exists(entry)) {
  stop(call. = FALSE, sprintf(
    "%s is absent, so nothing here can say what an agent loads before a task.",
    entry
  ))
}

files <- closure(entry)

# The mechanism, before the total. `CLAUDE.md` is a pointer file -- it carries
# one `@` line and no prose -- so a closure of itself alone is a reference
# reader that stopped reading, not a repository that shrank.
if (length(files) < 2L) {
  stop(call. = FALSE, sprintf(
    paste0(
      "%s resolved to %d file(s), so no `@` reference was read. The budget ",
      "below would pass on any repository. Fix the reference reader in this ",
      "script before trusting what it reports."
    ),
    entry, length(files)
  ))
}

sizes <- vapply(files, file.size, numeric(1))
total <- sum(sizes)

report <- sprintf("%8.0f  %s", sizes, files)

# The remedy is part of the message, as `verify-suite-coverage.R`'s is: a gate
# that only refuses invites the baseline to be raised instead of the argument
# being moved.
if (total > baseline_bytes) {
  stop(call. = FALSE, paste(
    c(
      sprintf(
        paste0(
          "The always-loaded context is %.0f bytes against a baseline of %d ",
          "(%s), over by %.0f."
        ),
        total, baseline_bytes, baseline_note, total - baseline_bytes
      ),
      "",
      report,
      "",
      paste(
        "An instruction belongs in this closure; the argument for it belongs",
        "to the file that owns the decision -- an ADR, a workflow comment, a",
        "verifier's header, or the ticket. Move the argument and cite it. If",
        "the addition is an instruction that has to be loaded before the task",
        "reaches it, move the baseline in this file in the same commit, and",
        "say there what was added and what paid for it."
      )
    ),
    collapse = "\n"
  ))
}

write_step_summary(c(
  "## Always-loaded agent context",
  "",
  sprintf(
    "%.0f bytes of a %d-byte baseline (%s).",
    total, baseline_bytes, baseline_note
  ),
  "",
  as_summary_block(paste(report, collapse = "\n"))
))

cat(sprintf(
  "Verified: the always-loaded context is %.0f bytes, within %d.\n",
  total, baseline_bytes
))
