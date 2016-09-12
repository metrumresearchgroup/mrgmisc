#' split IDs into groups to use for subsequent plotting
#' @param id vector of ids (eg id column)
#' @param id_per_plot number of ids per plot. Default to 9
#' @details
#' works very well with hadley wickham's purrr package to create a column
#' to split on then subsequently plot, see \code{vignette("Multiplot")} for details
#'@export
ids_per_plot <- function(id, id_per_plot = 9) {
  if(!is.vector(id))stop('id must be a vector')
  uid <- unique(id)
  mod <- length(uid)%/%id_per_plot
  remainder <- length(uid)%%id_per_plot
  bin_number <- c(rep(1:mod, each= id_per_plot),
                  rep(mod + 1, times = remainder ))
  if(length(bin_number) != length(uid)) stop("something went wrong in bin_number calculation")
  bin_number[match(id, uid)]
}


# generate_data <- function(num_inds, samples_per_id) {
#   expand.grid(ID = 1:num_inds, SAMPLE = 1:samples_per_id) %>% arrange(ID, SAMPLE)
# }
# dat100_3 <- generate_data(100, 3) 
# 
# dat100_8 <- generate_data(100, 8)
# 
# idpp <- function(dat, idpp) {
#   dat %>% mutate(IDPP = ids_per_plot(ID, idpp))
# }
# sum_n <- function(dat) {
#   dat %>% filter(!duplicated(ID)) %>% group_by(IDPP) %>% summarize(n = n())
# }
# idpp(dat100_3, 16) %>% sum_n()
# idpp(dat100_3, 9) %>% sum_n()
# idpp(dat100_3, 5) %>% sum_n()
# idpp(dat100_8, 16) %>% sum_n()
# idpp(dat100_8, 9) %>% sum_n()
# idpp(dat100_8, 5) %>% sum_n()
