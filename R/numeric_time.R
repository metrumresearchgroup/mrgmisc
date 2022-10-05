#' Convert clock time to decimal equivalent
#' 
#' @param .col column of type character or POSIXct to extract clocktimes from 
#' to convert to decimal values
#' 
#' @export
numeric_time <- function(.col) {
  ## if DATETIME format, strip clocktime to character
  if(lubridate::is.POSIXct(.col)){
    .col <- as.character(hms::as_hms(.col))
  }
  
  sapply(strsplit(.col,":"),
         function(x) {
           x <- as.numeric(x)
           round(x[1]+x[2]/60,2)
         }
  )
}