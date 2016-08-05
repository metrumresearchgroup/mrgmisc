#' create a list of plots cleanly with extra pdf functionality
#' @param .ggplot_list list of ggplot plots
#' @param .start_page_number pdf-only starting page number for plots
#' @details 
#' Especially for pdf, this can allow the generation of clean pdf pages
#' with only plots, no code, warnings, etc. for all pages related to the plots. 
#' In addition, by controlling the start number, you can further trim the pdf
#' to slice out the extra pages generated from the output but keep a nicely
#' numbered plot appendix
#' @examples \dontrun{
#' library(dplyr)
#' library(PKPDmisc)
#' library(PKPDdatasets)
#' 
#' # given we may only plot a subset of individuals per plot
#' # and generate multiple plots, lets split the dataframe 
#' list_of_ids <- sd_oral_richpk %>% capitalize_names() %>%
#' mutate(plotnum = ids_per_plot(ID)) %>% # default 9 per plot
#' split(.$plotnum)
#' 
#' # now we want to plot each subplot
#' plot_list <- list_of_ids %>%
#' lapply(function(df) {
#'  df %>%
#'    ggplot(aes(x = TIME, y = CONC, group = ID)) +
#'    geom_line() + facet_wrap(~ID)
#' })
#' 
#' # to print these out (with one plot per page on pdf)
#' print_plots(plot_list)
#' }
print_plots <- function(.ggplot_list, .start_page_number = 1) {
  .last <- length(.ggplot_list)
  .ggplot_list %>% seq_along() %>% lapply(function(p) {
    # make sure new page before first plot for pdf
    if (knitr::opts_knit$get("rmarkdown.pandoc.to") == "latex") {
      if (p == 1) {
        cat("\\newpage")
        cat(paste0("\\setcounter{page}{", .start_page_number, "}"))
      }
    }
    cat('\r\n\r\n')
    suppressWarnings(suppressMessages(print(.ggplot_list[[p]])))
    if (opts_knit$get("rmarkdown.pandoc.to") == "latex") {
      # want after last page to do a full page break
      if (p == .last) {
        cat("\\newpage")
      }
    }
    return(NULL)
  })
}