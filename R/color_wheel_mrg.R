#' Metrum RG color wheel
#' 
#' @keywords export
mrg_color_wheel <- function(.n_colors) {
  hues = seq(15, 375, length = .n_colors + 1)
  hcl(h = hues, l = 65, c = 100)[1:.n_colors]
}