#' Regularise datetime vector
#'
#' This function fills in gaps in a datetime vector and returns an equally spaced
#' datetime vector.
#'
#' @param timeVar Vector containing the time data. Can be POSIXct or numeric.
#' @param sampling_int Numeric. Sampling interval (in seconds if `timeVar` is
#'    POSIXct).
#'
#' @return Regularised time data.
#' @export
#'
#' @examples
regularise_time <- function(timeVar,
                                sampling_int) {

  start <- as.numeric(timeVar)[1]
  end <- as.numeric(timeVar)[length(timeVar)]
  new_time <- seq(start, end, sampling_int)
  if (lubridate::is.POSIXct(timeVar)) {
    new_time <- lubridate::as_datetime(new_time, tz = lubridate::tz(timeVar))
  }

  return(new_time)
}
