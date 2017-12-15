#' split IDs into groups to use for subsequent plotting
#' @param id vector of ids (eg id column)
#' @param id_per_plot number of ids per plot. Default to 9
#' @details
#' works very well with hadley wickham's purrr package to create a column
#' to split on then subsequently plot, see \code{vignette("Multiplot")} for details
#' @examples 
#' #chunking will provide the chunk index by splitting the data as evenly as possible
#' # into the number chunks specified
#' letters[1:9]
#' 
#' chunk(letters[1:9], 3)
#' 
#' letters[c(1, 1, 1:7)]
#' chunk(letters[c(1, 1, 1:7)], 3)
#' 
#' # sometimes you want to evenly chunk by unique values rather than purely balancing
#' chunk_grp(c(1, 1, 1:7), 3)
#' 
#' # a next step after chunking is splitting into a list, so this does thus for you
#' 
#' # chunk list will both split the data and keep the original values
#' chunk_list(letters[1:9], 3)
#' 
#' chunk_list(c(letters[1], letters[1], letters[1:7]), 3)
#' 
#' # in this case ragged arrays will be created to keep the number of 
#' # unique elements consistent as possible between chunks
#' chunk_grp_list(c(letters[1], letters[1], letters[1:7]), 3)
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
chunk <- function(.x, .nchunk = parallel::detectCores()) {
  mod <- length(.x)%/%.nchunk
  bin_number <- sort(rep(1:.nchunk, each = mod, length.out = length(.x)))
  return(bin_number)
}

#' @rdname chunk
#' @export
chunk_grp <- function(.x, .nchunk = parallel::detectCores()) {
  .c <- chunk(unique(.x), .nchunk)
  .c[match(.x, unique(.x))]
}

#' @rdname chunk
#' @export
chunk_list <- function(.x, .nchunk = parallel::detectCores()) {
  .c <- chunk(.x, .nchunk)
  unname(split(.x, .c))
}

#' @rdname chunk
#' @export
chunk_grp_list <- function(.x, .nchunk = parallel::detectCores()) {
  .c <- chunk_grp(.x, .nchunk)
  unname(split(.x, .c))
}