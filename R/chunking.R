
#' Chunking
#' 
#' Create chunks by group, unique values, and return as a vector or a list with elements. 
#'  
#' @examples 
#' # Chunking will provide the chunk index by splitting the data 
#' # as evenly as possible into the number chunks specified
#' 
#' chunk(letters[1:9], 3) 
#' 
#' # When the vector length isn't divisible by the number of chunks, 
#' # some chunks will be filled with 1 more element than others. 
#' 
#' chunk(letters[c(1, 1, 2, 1:7)], 3)
#' 
#' # If interested in evenly chunking by unique values rather than balancing,
#' # notice how the first chunk contains many more elements since there are 3, 1's.
#' 
#' chunk_grp(c(1, 1, 1:7), 3)
#' 
#' # A potential next step after chunking is splitting the output into a list.
#' 
#' chunk_list(letters[1:9], 3)
#' 
#' # If we want to keep unique elements consistent as possible between chunks
#' 
#' chunk_grp_list(c(letters[1], letters[1], letters[1:7]), 3)
#' 
#' @param .x vector of values
#' @param .nchunk number of chunks to identify
#' @export
chunk <- function(.x, .nchunk = parallel::detectCores()) {
  if (length(.x) < .nchunk) stop('# of chunks must be less than or equal to length of vector')
  mod <- length(.x)%/%.nchunk
  bin_number <- sort(rep(1:.nchunk, each = mod, length.out = length(.x)))
  return(bin_number)
}


#' @describeIn chunk split IDs into groups to use for subsequent plotting
#' works very well with hadley wickham's purrr package to create a column
#' to split on then subsequently plot, see \code{vignette("Multiplot")} for details
#' @param id vector of ids (eg id column)
#' @param id_per_plot number of ids per plot. Default to 9
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



#' @describeIn chunk used when desirable to have unique elements in same chunk
#' @export
chunk_grp <- function(.x, .nchunk = parallel::detectCores()) {
  .c <- chunk(unique(.x), .nchunk)
  .c[match(.x, unique(.x))]
}

#' @describeIn chunk used when desirable to have output be a list
#' @export
chunk_list <- function(.x, .nchunk = parallel::detectCores()) {
  if (class(.x) == "list") warning("It's recommended to input a vector as this function converts input to list")
  .c <- chunk(.x, .nchunk)
  unname(split(.x, .c))
}

#' @describeIn chunk used when desirable to have output be a list with unique
#' elements in the same chunk
#' @export
chunk_grp_list <- function(.x, .nchunk = parallel::detectCores()) {
  .c <- chunk_grp(.x, .nchunk)
  unname(split(.x, .c))
}