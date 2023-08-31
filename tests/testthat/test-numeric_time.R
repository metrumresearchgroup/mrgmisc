test_that("numeric_time accurately calculates the decimal time from type POSIXct columns", {
  timeDF <- dplyr::tibble(ID = c(1, 1, 2),
                     DATETIME = c("2022-01-23T12:34:00", "2022-01-28T04:32:00", "2022-01-21T18:00:00"))
  times <- lubridate::ymd_hms(timeDF$DATETIME)
  
  expect_equal(numeric_time(times)[1], 12.57)
  expect_equal(numeric_time(times)[2], 4.53)
})
