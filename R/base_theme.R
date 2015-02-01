#' A theme with better default values for pharmacometric plots, especially conc-time
#' @param legend_text size of legend text
#' @param axis_title_x size of X axis title
#' @param axis_title_y size of Y axis title
#' @param axis_text_x size of X axis text
#' @param axis_text_y size of Y axis text
#' @param strip_text_x size of strip text for X axis
#' @param strip_text_y size of strip text for Y axis
#' @examples 
#' \dontrun{
#' ggplot() + base_theme_obs()
#' made for viewing in the plot window and copying out
#' }
#' @export 
base_theme <- function(legend_text= 14,
                           legend_title = 16,
                           axis_title_x = 14,
                           axis_title_y = axis_title_x,
                           axis_text_x = 12,
                           axis_text_y = axis_text_x,
                           strip_text_x = 16,
                           strip_text_y = strip_text_x
                           ) { ggplot2::theme(legend.text = element_text(size = legend_text),
                                 legend.title = element_text(size = legend_title),
                                 axis.title.x = element_text(size = axis_title_x, face = "bold"),
                                 axis.title.y = element_text(size = axis_title_y, face = "bold"),
                                 axis.text.x = element_text(color = "black", size = axis_text_x),
                                 axis.text.y = element_text(color = "black", size = axis_text_y),
                                 strip.text.x = element_text(color = "black", size = strip_text_x, face = "bold"),
                                 strip.text.y = element_text(color = "black", size = strip_text_y, face = "bold")
                               )
}
