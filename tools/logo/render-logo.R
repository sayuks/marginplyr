# Rasterises `tools/logo/logo.svg` to `man/figures/logo.png`, the image README.md
# and the site show. Run from the repository root after editing the SVG:
#
#   Rscript tools/logo/render-logo.R
#
# The PNG is what ships because the SVG's wordmark is live text in Helvetica
# Neue, which a viewer without that face would replace; rasterise on a machine
# that has it. rsvg is used rather than R's own png() devices: the macOS quartz
# device converts the colours to a Generic RGB profile, so the PNG would no
# longer hold the SVG's sRGB values.
rsvg::rsvg_png("tools/logo/logo.svg", "man/figures/logo.png", width = 480)
