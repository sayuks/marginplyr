# Confirms that the project-scoped Codex profile retains the narrow Quarto
# cache grant needed by source-tarball vignette builds, while the profile stays
# out of the source package.
#
# Run it locally with:
#
#     Rscript .github/scripts/verify-codex-config.R

expect_line <- function(lines, expected, label) {
  if (!any(lines == expected)) {
    stop(sprintf("Missing %s: %s", label, expected), call. = FALSE)
  }
}

codex_config <- readLines(".codex/config.toml", warn = FALSE)
expect_line(
  codex_config,
  'default_permissions = "marginplyr"',
  "marginplyr Codex permission profile"
)
expect_line(
  codex_config,
  'extends = ":workspace"',
  "inherited workspace safeguards"
)
expect_line(
  codex_config,
  '"~/Library/Caches/quarto" = "write"',
  "Codex Quarto cache grant"
)

build_ignore <- readLines(".Rbuildignore", warn = FALSE)
expect_line(
  build_ignore,
  "^\\.codex$",
  "source-package exclusion for Codex configuration"
)
