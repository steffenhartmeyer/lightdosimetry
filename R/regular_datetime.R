#' Regularise datetime vector
#'
#' This function fills in gaps in a datetime vector and returns an equally spaced
#' datetime vector.
#'
#' @param dtVar Vector containing the time data. Can be POSIXct or numeric.
#' @param sampling_int Numeric. Sampling interval (in seconds if `dtVar` is
#'    POSIXct).
#'
#' @return Regularised time data.
#' @export
#'
#' @examples
regular_datetime <- function(dtVar,
                             sampling_int) {
  start <- as.numeric(dtVar)[1]
  end <- as.numeric(dtVar)[length(dtVar)]
  datetime <- seq(start, end, sampling_int)
  if (lubridate::is.POSIXct(dtVar)) {
    datetime <- lubridate::as_datetime(datetime, tz = lubridate::tz(dtVar))
  }

  return(datetime)
}
