#' Brightest or darkest continuous period
#'
#' This function finds the brightest or darkest continuous period of a given
#' timespan and calculates its \code{mean} light level, \code{onset},
#' \code{midpoint}, and \code{offset}. Defined as the period with the maximum
#' or minimum mean light level.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param timeVar Vector containing the time data. Can be POSIXct or numeric.
#' @param period_type String indicating the type of period. Must be "bright" or
#'    "dark".
#' @param timespan Single string or vector of strings with the timespan(s).
#'    Timespans must be in the format "[numeric] [unit]", with possible units
#'    ("seconds","minutes","hours","days"). Units can be abbreviated.
#'    See \code{\link{parse_timeunit_tosecs}}.
#' @param sampling_int Numeric. Sampling interval in seconds.
#' @param loop Logical. Should the data be looped? Defaults to FALSE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of timespan and calculated values.
#'    If wide is TRUE then variable names will be concatenated with the timespan.
#'
#' @details Assumes regular 24h light data. Otherwise, results may not be
#'    meaningful. Looping the data is recommended for finding the darkest period.
#'    Missing light values will be removed by default.
#'
#' @export
#'
#' @examples
bright_dark_period <- function(lightVar,
                               timeVar,
                               period_type,
                               timespan,
                               sampling_int,
                               loop = FALSE,
                               as_df = TRUE,
                               wide = TRUE) {

  # Check whether time series is regularly spaced
  if (length(unique(diff(timeVar))) > 1) {
    warning("Time variable is not regularly spaced. Calculated results may be incorrect!")
  }

  # Parse period
  max <- switch(period_type,
    "bright" = TRUE,
    "dark" = FALSE,
    stop("Wrong period type specification! Must be 'bright' or 'dark'.")
  )

  # Loop data
  if (loop) {
    lightVar <- c(lightVar, lightVar)
    span <- timeVar[length(timeVar)] - timeVar[1]
    timeVar <- c(timeVar, timeVar + span + sampling_int)
  }

  df <- tibble::tibble(
    timespan = numeric(),
    mean = numeric(),
    midpoint = numeric(),
    onset = numeric(),
    offset = numeric()
  )

  for (ts in timespan) {
    # Parse time unit
    parsed_ts <- parse_timeunit_tosecs(ts)

    # Calculate window size
    window <- floor(parsed_ts$secs / sampling_int)
    if (window %% 2 != 0) window <- window + 1

    # Calculate rolling means
    means <- zoo::rollapply(lightVar, window, mean,
      na.rm = TRUE,
      partial = FALSE, fill = NA
    )

    # Find maximum/minimum mean value
    if (max) {
      center <- which(means == max(means, na.rm = TRUE))[1]
    } else {
      center <- which(means == min(means, na.rm = TRUE))[1]
    }

    df <- df %>% tibble::add_row(
      timespan = as.numeric(parsed_ts$time),
      mean = means[center],
      midpoint = as.numeric(timeVar[center]),
      onset = as.numeric(timeVar[center - (window / 2) + 1]),
      offset = as.numeric(timeVar[center + (window / 2)])
    )
  }

  # Convert to POSIXct
  if (lubridate::is.POSIXct(timeVar)) {
    df <- df %>% dplyr::mutate_at(dplyr::vars(midpoint:offset),
      lubridate::as_datetime,
      tz = lubridate::tz(timeVar)
    )
  }

  # Rename
  if (max) {
    names(df)[-1] <- paste0("bright_period_", names(df)[-1])
  } else {
    names(df)[-1] <- paste0("dark_period_", names(df)[-1])
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>% tidyr::pivot_wider(
      names_from = timespan,
      values_from = names(df)[-1],
      names_sep = "."
    )
  }

  # Return as data frame or numeric matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}
