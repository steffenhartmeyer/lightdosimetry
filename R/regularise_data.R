#' Regularise (date)time vector
#'
#' This function fills in gaps in a data frame containing time series data.
#'
#' @param df (Grouped) data frame to be regularised.
#' @param timeVar Column containing the time data. Can be POSIXct or numeric.
#' @param sampling_int Numeric. Sampling interval (in seconds if `timeVar` is
#'    POSIXct).
#' @param startVar Column indicating the start of the interval to be regularised.
#'    Can be POSIXct or numeric. If not specified, the first element of the
#'     `timeVar` vector will be taken (default).
#' @param endVar Column indicating the end of the interval to be regularised.
#'    Can be POSIXct or numeric. If not specified, the last element of the
#'     `timeVar` vector will be taken (default).
#'
#' @return Regularised time data.
#' @export
#'
#' @examples
regularise_data <- function(df, timeVar, sampling_int, startVar = NULL, endVar = NULL) {
  df %>%
    dplyr::summarise(
      "{{ timeVar }}" := list(regularise_time(
        {{ timeVar }},
        sampling_int,
        {{ startVar }},
        {{ endVar }}
      ))
    ) %>%
    tidyr::unnest(cols = c({{ timeVar }})) %>%
    dplyr::left_join(df)
}

# Helper function
regularise_time <- function(timeVar,
                            sampling_int,
                            start = NULL,
                            end = NULL) {
  if (is.null(start)) {
    start <- as.numeric(timeVar)[1]
  } else {
    start <- as.numeric(start)[1]
  }

  if (is.null(end)) {
    end <- as.numeric(timeVar)[length(timeVar)]
  } else {
    end <- as.numeric(end)[1]
  }

  # Make new time
  new_time <- seq(start, end, sampling_int)
  if (lubridate::is.POSIXct(timeVar)) {
    new_time <- lubridate::as_datetime(new_time, tz = lubridate::tz(timeVar))
  }

  return(new_time)
}
