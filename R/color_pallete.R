#' design-quality color palletes to use in ggplot2
#' @param pallete pallete number or name
#' @examples \dontrun{
#' library(PKPDdatasets)
#' library(PKPDmisc)
#' library(ggplot2)
#' library(dplyr)
#' ggplot(sd_oral_richpk %>% filter(ID < 20), 
#' aes(x = Time,
#' y= Conc, 
#' group = ID, 
#' color = Gender)) + 
#'   geom_line(size = 1.5) + scale_color_manual(values =color_pallete(1)) +
#'   base_theme()
#' } 
#' @export
color_pallete <- function(pallete) {
  palletes <- list(s1 = c("#359075",
                          "#CA4FCC",
                          "#CD4E3C",
                          "#617FAE",
                          "#BF5084")
                   )
  return(palletes[[pallete]])
}