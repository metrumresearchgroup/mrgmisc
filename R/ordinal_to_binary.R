#' convert a column of categorical covariates into a number of 
#' columns with a binary flag for each category
#' @param df data frame
#' @param col_name column name to split into multiple binary flags
#' @param prefix string name if want to specify a prefix to label 
#'    the binary flag columns other than the original col_name
#' @param overwrite overwrite any existing columns if the newly generated columns
#'    share the same name
#' @rdname oridinal_to_binary
#' @examples \donttest{
#' library(dplyr)
#' df <- data_frame(OCC = c(1, 1, 2, 3))
#' df %>% ordinal_to_binary_("OCC")
#' df %>% ordinal_to_binary_("OCC", prefix = "OCCASION")
#' 
#' df2 <- data_frame(OCC = c(1, 1, 2, 3), OCC1 = 999)
#' df2 %>% ordinal_to_binary_("OCC")
#' df2 %>% ordinal_to_binary_("OCC", overwrite=T)
#' }
#' @export
ordinal_to_binary_ <- function(df, 
                               col_name, 
                               prefix = NULL, 
                               overwrite = FALSE) {
  if(!is.numeric(df[[col_name]])) {
    stop("Currently, column to convert must be numeric")
  }
  unique_categories <- unique(df[[col_name]])
  cols <- lapply(unique_categories, function(x, df, col_name) {
    result <- ifelse(df[[col_name]] == x, 1, 0)
    result <- setNames(dplyr::data_frame(nm = result), paste0(col_name, x))
    return(result)
  }, df, col_name)
  #df_cols <- do.call(dplyr::data_frame, cols)
  df_cols <- do.call(cbind, cols)
  if(!is.null(prefix)) {
    names(df_cols) <- stringr::str_replace(names(df_cols), col_name, prefix)
  } 
  
  # check if any column names exist in both
  same_names <- dplyr::intersect(names(df_cols), names(df))
  
  if (overwrite) {
    for (i in seq_along(same_names)) {
      df[[same_names[i]]] <- NULL 
    }
  }
  #make sure no intersecting column names
  if(length(same_names) > 0 & !overwrite) {
    warning("detected existing columns with same name sequence, 
            prepending \"_\" to all generated columns.")
    names(df_cols) <- paste0("_", names(df_cols))
  }
  return(cbind(df, df_cols))
}


