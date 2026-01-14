#' Show column labels
#' 
#' Gives column names and labels
#' 
#' If a column has a vector of labels then the first label will be returned.
#' 
#' @param df data frame
#' @param ... columns of interest. If none are specified then all columns will
#'   be used. Accepts:
#'   \itemize{
#'    \item unquoted column names (e.g., \code{ID})
#'    \item character column names (e.g., \code{"ID"})
#'    \item numeric indices (e.g., \code{1:3})
#'    \item tidyselect helpers (e.g., \code{starts_with("A")})
#'    \item vectors of any of the above
#'   }
#'
#' @return A tibble with the names of \code{df} in the first column and the
#'   labels of \code{df} in the second column
#' @seealso \link[tidyselect]{starts_with}
#' @export
#' 
#' @examples
#' id <- 1:4
#' age <- c(83, 29, 64, 40)
#' bwt <- c(71, 66, 70, 81)
#' attr(id, "label") <- "Subject ID"
#' attr(age, "label") <- "Age (years)"
#' attr(bwt, "label") <- "Weight (kg)"
#' df <- tibble::tibble(ID = id, AGE = age, BWT = bwt)
#' show_labels(df)
#' show_labels(df, "AGE", BWT)
#' show_labels(df, 2)
#' show_labels(df, c("BWT", "ID"))
#' show_labels(df, tidyselect::starts_with("A"))
show_labels <- function(df, ...) {
  if (!is.data.frame(df)) stop("df must be a data frame.")
  
  # work with a 0-row slice to avoid carrying large amounts of data
  df_meta <- df[0, , drop = FALSE]
  
  # if dots (...) are present, select specific columns
  if (!rlang::is_empty(rlang::enquos(...))) {
    tryCatch(
      df_meta <- dplyr::select(df_meta, ...),
      error = function(e) {
        stop(
          "Invalid column selection in `...`.\n",
          " Arguments must be column names (quoted or unquoted), indices,",
          " tidyselect helpers, or vectors of these.",
          call. = FALSE
        )
      }
    )
  }
  
  # ensure strict character output
  labels <- purrr::map_chr(df_meta, function(col) {
    val <- attr(col, "label", exact = TRUE)
    if (is.null(val)) return(NA_character_)
    # take first element if label is a vector
    as.character(val)[1] 
  })
  
  # return
  tibble::tibble(
    name = names(df_meta),
    label = stats::setNames(labels, NULL)
  )
}
