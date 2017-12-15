#' split IDs into groups to use for subsequent plotting
#' @param id vector of ids (eg id column)
#' @param id_per_plot number of ids per plot. Default to 9
#' @details
#' works very well with hadley wickham's purrr package to create a column
#' to split on then subsequently plot, see \code{vignette("Multiplot")} for details
#' @rdname chunk
#'@export
ids_per_plot <- function(id, id_per_plot = 9) {
  if(!is.vector(id)) {
    stop("chunking requires a vector")
  }
  uid <- unique(id)
  mod <- length(uid)%/%id_per_plot
  remainder <- length(uid)%%id_per_plot
  bin_number <- c(rep(1:mod, each= id_per_plot),
                  rep(mod + 1, times = remainder ))
  bin_number <- sort(bin_number)
  if(length(bin_number) != length(uid)) stop("something went wrong in bin_number calculation")
  bin_number[match(id, uid)]
}

#' chunk
#' @param .x vector of values
#' @param .nchunk number of chunks to identify
#' @rdname chunk
#' @details 
#' chunk by group, unique values, and return as a vector or a list with elememts
#' @export
chunk_grp <- function(.x, .nchunk) {
  ids_per_plot(.x, .nchunk)
}

#' @rdname chunk
#' @export
chunk <- function(.x, .nchunk) {
  mod <- length(.x)%/%.nchunk
  remainder <- length(.x)%%.nchunk
  bin_number <- c(rep(1:mod, each= .nchunk),
                  rep(mod + 1, times = remainder ))
  bin_number <- sort(bin_number)
  if(length(bin_number) != length(uid)) stop("something went wrong in bin_number calculation")
  return(bin_number)
}

#' @rdname chunk
#' @export
chunk_list <- function(.x, .nchunk) {
  chunk <- chunk_grp(.x, .nchunk)
  split(chunk, chunk)
}

#' @rdname chunk
#' @export
chunk_grp_list <- function(.x, .nchunk) {
  chunk <- chunk_grp(.x, .nchunk)
  split(chunk, chunk)
}