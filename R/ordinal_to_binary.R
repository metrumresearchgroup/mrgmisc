#' convert a column of categorical covariates into a number of 
#' columns with a binary flag for each category
#' @rdname oridinal_to_binary
ordinal_to_binary_ <- function(df, 
                               col_name, 
                               id = NULL, 
                               prefix = NULL, 
                               overwrite = F) {
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
  
  #make sure no intersecting column names
  if(length(setdiff(names(df_cols), names(df)) > 0 & !overwrite) {
    warning("detected existing columns with same name sequence, 
            prepending \"_\" to all generated columns.")
    names(df_cols) <- paste0("_", names(df_cols))
  }
  return(cbind(df, df_cols))
}

test_df <- data_frame(OCC = c(1, 1, 2, 3))

ordinal_to_binary_(test_df, "OCC")
