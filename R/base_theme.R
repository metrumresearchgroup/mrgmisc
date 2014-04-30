
#' A theme with better default values for pharmacometric plots, especially conc-time
#' @export 
base_theme <- function() { theme(legend.text = element_text(size = 10),
                                 legend.title = element_text(size = 12),
                                 axis.title.x = element_text(size = 12, face = "bold"),
                                 axis.title.y = element_text(size = 12, face = "bold"),
                                 axis.text.x = element_text(color = "black", size = 10),
                                 axis.text.y = element_text(color = "black", size = 10),
                                 strip.text.x = element_text(color = "black", size = 10, face = "bold"))
}

#' A theme with better default values for pharmacometric plots, especially conc-time
#' 
#' made for viewing in the plot window and copying out
#' @export 
base_theme_obs <- function() { theme(legend.text = element_text(size = 14),
                                 legend.title = element_text(size = 16),
                                 axis.title.x = element_text(size = 14, face = "bold"),
                                 axis.title.y = element_text(size = 14, face = "bold"),
                                 axis.text.x = element_text(color = "black", size = 12),
                                 axis.text.y = element_text(color = "black", size = 12),
                                 strip.text.x = element_text(color = "black", size = 14, face = "bold"))
}
