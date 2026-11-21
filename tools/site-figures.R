# Quarto pre-render script named by `altdoc/quarto_website.yml`. Quarto runs it
# with the site project, `_quarto/`, as the working directory.
#
# README.md, which the home page includes, and the site's navbar both show
# `man/figures/logo.png`, so the image has to exist at that path inside the
# project. altdoc copies `man/figures/` only to `help/figures/`.
dir.create("man", showWarnings = FALSE)
copied <- file.copy("../man/figures", "man", overwrite = TRUE, recursive = TRUE)
if (!all(copied)) {
  stop("Could not copy man/figures/ into the site project.", call. = FALSE)
}
