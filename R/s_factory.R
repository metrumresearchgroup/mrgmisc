# s_factory_ <- function(fun) {
#   func <- function(df, DV, na.rm=T, name = "CMAX") {
#     dots = list(lazyeval::interp(~ fun(var, na.rm = na.rm), 
#                                  var = as.name(DV),
#                                  fun = as.name(fun)))
#     df %>% 
#       dplyr::summarize_(.dots = setNames(dots, paste0(name)))
#   }
#  return(func)
# }
# 
# 
# ## still doesn't work
# s_factory <- function(fun) {
#   func <- interp(~func, func =lazyeval::lazy(fun))
#   browser()
#   s_factory_(func)
# }
# 
# 
# test_smax_ <- s_factory_("max")
# test_smax <- s_factory(max)
# library(PKPDdatasets)
# pr_id <- dplyr::group_by(pain_relief, ID)
# s1 <- s_cmax(pr_id, "CONC") 
# s2 <- test_smax_(pr_id, "CONC")
# s3 <- test_smax(pr_id, "CONC")
# 
# identical(s1, s2)
# 
