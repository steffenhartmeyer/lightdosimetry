#' Find threshold at timespan
#'
#' This function finds the threshold of light for a given timespan.
#' This function is the inverse of \code{\link{tat}}.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param timespan Single value or vector specifying the timespan(s).
#'    Can be numeric or string in the format "[numeric] [unit]", with possible units ("seconds","minutes",
#'    "hours","days"). Units can be abbreviated, see
#'    \code{\link{parse_timeunit_tosecs}}. If numeric, no time scaling will be performed.
#' @param above Logical. Should the threshold of the timespan at which the light
#'    is above the threshold be calculated? Defaults to TRUE. If FALSE, the
#'    threshold of the timespan at which the light is below the threshold is calculated.
#' @param sampling_int Numeric. Sampling interval in seconds. If not specified
#'    (default), no time scaling will be performed.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of timespans and calculated values.
#'    If wide is TRUE, then variable names will be concatenated with the timespan.
#' @export
#'
#' @examples
threshold_at_timespan <- function(lightVar,
                                  timespan,
                                  sampling_int = NULL,
                                  above = TRUE,
                                  as_df = TRUE,
                                  wide = TRUE) {

  # Check whether sampling interval and output unit specified
  if (is.null(sampling_int) | is.numeric(timespan)) {
    warning("No sampling interval and/or timespan unit specified. Returning raw output.")
    sampling_int <- 1
    timespan <- paste(timespan, "secs")
  }

  df <- tibble::tibble(
    timespan = numeric(),
    threshold = numeric()
  )
  for (ts in timespan) {
    parsed_ts <- parse_timeunit_tosecs(ts)
    idx <- floor(parsed_ts$secs / sampling_int)
    sorted <- sort(lightVar, decreasing = above)
    threshold <- sorted[idx]
    df <- df %>% tibble::add_row(
      timespan = as.numeric(parsed_ts$time),
      threshold = threshold
    )
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = timespan,
        values_from = threshold,
        names_prefix = "threshold."
      )
  }

  # Return as data frame or numeric
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}
