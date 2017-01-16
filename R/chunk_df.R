#' chunk dataframes so easy to split for parallel processing
#' @param .gdf (grouped) data frame
#' @param ... grouping variables, either a character vector or NSE-style column names
#' @param .nchunks set number of chunks
#' @examples 
#' library(dplyr)
#' Theoph %>% chunk_df()
#' 
#' Theoph %>% group_by(Subject) %>% chunk_df()
#' Theoph %>% chunk_df(Subject)
#' Theoph %>% chunk_df(c("Subject"))
#' 
#' Theoph %>% chunk_df(Subject, .nchunks = 3)
#' @export
chunk_df <- function(.gdf, ..., .nchunks = NULL) {
  if (is.null(.nchunks)) {
    .nchunks <- parallel::detectCores()
  }
  .dots <- dplyr:::dots(...)
  if (length(.dots) > 0) {
    # handle NSE
    if (class(.dots[[1]]) == "name") {
      .gdf <- dplyr::group_by(.gdf, ...)
    } else {
      # expect a named vector
      dot_cols <- eval(.dots[[1]])
      if (!is.character(dot_cols)) {
        stop("grouping vars passed to dots must be a single character vector or unquoted column names")
      }
      .gdf <- dplyr::group_by_(.gdf, .dots = dot_cols)
    }
  }
  .gdf$indices__ <- dplyr::group_indices(.gdf)
  indices <- unique(.gdf$indices__)
  # if not grouped will all be 1, in that case treat this 'by row'
  if (length(indices) == 1) {
    .gdf$indices__ <- 1:nrow(.gdf)
    indices <- 1:nrow(.gdf)
  }
  num_indices <- length(indices)
  per_chunk <- num_indices %/% .nchunks 
  remainder <- num_indices %% .nchunks
  
  if (remainder > 0) {
    chunk_indices <- c(rep(1:.nchunks, per_chunk), 1:remainder)
  } else {
    chunk_indices <- rep(1:.nchunks, per_chunk)
  }
  
  chunk_indices <- chunk_indices[order(chunk_indices)]
  indices_df <- tibble::tibble(
    indices__ = indices, 
    sim_chunk = chunk_indices
  )
  return(
    dplyr::select(dplyr::left_join(.gdf, indices_df, by="indices__"), -indices__) %>%
      purrr::slice_rows("sim_chunk")
  )
}