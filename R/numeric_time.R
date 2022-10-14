#' Convert clock time to decimal equivalent
#' 
#' @param .col column of type character or POSIXct to extract clocktimes from 
#' to convert to decimal values
#' 
#' @examples 
#' 
#' timedf <- dplyr::tibble(ID = c(1, 1, 2),
#' DATETIME = c("2022-01-23T12:34:00", "2022-01-28T04:32:00", "2022-01-21T18:00:00"))
#'
#' times <- lubridate::ymd_hms(timedf$DATETIME)
#' numeric_time(times)
#' 
#' @author Samuel P Callisto, PhD
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