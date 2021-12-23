#' Maximum or minimum continuous period
#'
#' This function finds the brightest or darkest continuous period of a given
#' timespan and calculates its \code{mean} light level, \code{onset},
#' \code{midpoint}, and \code{offset}. Defined as the period with the maximum
#' or minimum mean light level.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param dtVar Vector containing the time data. Can be POSIXct or numeric.
#' @param timespan Single string or vector of strings with the timespan(s).
#'    Timespans must be in the format "[numeric] [unit]", with possible units
#'    ("seconds","minutes","hours","days"). Units can be abbreviated.
#'    See \code{\link{parse_timeunit_tosecs}}.
#' @param period_type String indicating the type of period. Must be "max" or "min".
#' @param sampling_int Numeric. Sampling interval in seconds. Defaults to 60.
#' @param loop Logical. Should the data be looped? Defaults to TRUE
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of timespan and calculated values.
#'    If wide is TRUE then variable names will be concatenated with the timespan.
#'
#' @details Assumes 24h light data. Otherwise, results may not be meaningful.
#'    Looping the data is recommended for finding the minimum (darkest) period.
#'
#' @export
#'
#' @examples
max_min_period <- function(lightVar,
                           dtVar,
                           timespan,
                           period_type,
                           sampling_int = 60,
                           loop = TRUE,
                           na.rm = TRUE,
                           as_df = TRUE,
                           wide = TRUE) {

  # Parse period
  max <- switch(period_type,
    "max" = TRUE,
    "min" = FALSE,
    stop("Wrong period specification! Must be 'max' or 'min'.")
  )

  # Loop data
  if (loop) {
    lightVar <- c(lightVar, lightVar)
    dtVar <- c(dtVar, dtVar)
    # dt = as.numeric(dtVar)
    # dt = c(dt[1:length(dt)-1], dt+(dt[length(dt)]-dt[1]))
    # if(lubridate::is.POSIXct(dtVar))
    #   dtVar = dt %>% lubridate::as_datetime(tz = lubridate::tz(dtVar))
    # else
    #   dtVar = dt
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
    window <- (parsed_ts$secs / sample_int) %>% floor()
    if (window %% 2 != 0) window <- window + 1

    # Calculate rolling means
    means <- zoo::rollapply(lightVar, window, mean,
      na.rm = na.rm,
      partial = FALSE, fill = NA
    )

    # Find maximum/minimum mean value
    if (max) {
      center <- which(means == max(means, na.rm = TRUE))[1]
    } else {
      center <- which(means == min(means, na.rm = TRUE))[1]
    }

    df <- df %>% tidyr::add_row(
      timespan = as.numeric(parsed_ts$time),
      mean = means[center],
      midpoint = as.numeric(dtVar[center]),
      onset = as.numeric(dtVar[center - (window / 2) + 1]),
      offset = as.numeric(dtVar[center + (window / 2)])
    )
  }

  # Convert to POSIXct
  if (lubridate::is.POSIXct(dtVar)) {
    df <- df %>% mutate_at(vars(midpoint:offset),
      lubridate::as_datetime,
      tz = lubridate::tz(dtVar)
    )
  }

  # Rename
  if (max) {
    names(df)[-1] <- paste0("max_period_", names(df)[-1])
  } else {
    names(df)[-1] <- paste0("min_period_", names(df)[-1])
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
