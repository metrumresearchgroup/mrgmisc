# return either quoted character or lazy evaluation expression whether a param is a function or not
check_lazy_value <- function(x) {
  if(is.function(eval(substitute(x)))) {
    return(deparse(substitute(x)))
  } else {
    return(lazyeval::lazy(x))
  }
}