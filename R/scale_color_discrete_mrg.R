#' Apply MRG's discrete color palette to ggplots
#' 
#' @description 
#' This function ensures that variables have a consistent color mapping across 
#' multiple plots and that Metrum's preferred color palette is used for the plot. 
#' 
#' Your specification file provides the decodes for each variable which is used
#' to consistently assign each individual decode to a specific color.
#' 
#' If the same specification file is used to create multiple plots, it's suggested
#' to define a global option "mrg_color_spec" as such: `options(mrg_color_spec = YourSpec)`.
#' 
#' @param .ggplot a ggplot object with a color group already assigned
#' @param .spec specification file (preferred format is yaml)
#' 
#' @keywords export
scale_color_discrete_mrg <- function(.ggplot, .spec = getOption("mrg_color_spec")) {
  
  if (is.null(.spec)) {
    cli::cli_alert_info("If using the same spec for all plots, you can define 'options(mrg_color_spec) = spec'")
    stop("No spec defined")
  }
  
  # Extract column used to color data points
  .color_col <- rlang::quo_get_expr(.ggplot$mapping$colour)
  
  if (class(.ggplot$data[[.color_col]]) == "numeric") {
    cli::cli_alert_danger("Color variable is continuous, only use this function when coloring by discrete variables")
    stop("Non-discrete color variable")
  }
  
  # Find column in spec
  spec_col <- .spec[[gsub(pattern = "_f", "", as.character(.color_col))]]
  
  # Check is column is a factor
  is_factor <- is.factor(.ggplot$data[[.color_col]])
  
  if (is.null(spec_col$decode) | is.null(spec_col$values)) {
    paste0(spec_col, " does not have decodes or values defined in spec")
  }
  
  if(!is_factor){
    values <- spec_col$decode
  } else {
    values <- levels(.ggplot$data[[.color_col]])
  }
  
  myColors <- mrg_color_wheel(.n = length(values))
  
  names(myColors) <- values
  
  return(.ggplot + scale_color_manual(values = myColors))
  
}