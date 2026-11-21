# Rasterises `tools/logo/logo.svg` to `man/figures/logo.png`, the image
# README.md and the site show. After editing the SVG, run from the repository
# root:
#
#   Rscript tools/logo/render-logo.R
#
# It needs rsvg, which no package code or workflow uses, so nothing declares it.
#
# The PNG is what ships because the SVG's wordmark is live text in Helvetica
# Neue, which a viewer without that face would replace; rasterise on a machine
# that has it.
rsvg::rsvg_png("tools/logo/logo.svg", "man/figures/logo.png", width = 480)
