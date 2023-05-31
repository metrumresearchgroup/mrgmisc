#' Search for value in a data.frame
#'
#' @description 
#' \code{search_df} counts the number of occurrences of a specified value in 
#' each column of a given data.frame. The output will show the count for each
#' column that had at least one value matching the search criteria.
#' 
#' @param .df a data.frame for the count of a value in each column
#' @param .value numeric or character value to search
#' 
#' @examples
#' search_df(Theoph, 1)
#' 
#' @export
search_df <- function(.df, .value) {
  .df %>% 
    dplyr::summarize(dplyr::across(dplyr::everything(), ~ sum(. == .value)), .groups = "keep") %>% 
    tidyr::pivot_longer(-dplyr::group_cols(), values_to = paste0("n_", as.character(.value))) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(.data[[paste0("n_", as.character(.value))]] > 0)
}