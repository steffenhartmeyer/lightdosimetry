#' Phase angle of first/last timing above/below threshold.
#'
#' These functions calculate the phase angle (i.e., timespan) between the first/last
#' timepoint where light levels are above or below a given threshold intensity
#' and the start/end of the given interval.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param dtVar Vector containing the time data. Can be POSIXct or numeric.
#' @param threshold Single numeric value or vector specifying threshold
#'    intensities. The sign indicates above/below (see \code{\link{threshold}}).
#' @param unit_out String indicating the time unit of the output value.
#'    Possible values are ("seconds","minutes","hours","days"). Units can be
#'    abbreviated. Is only used if `dtVar` is POSIXct, otherwise no conversion
#'    will be performed. Defaults to "minutes".
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of threshold and calculated values.
#'    If `wide` is TRUE then variable names will be concatenated with the threshold.
#'
#' @details The phase angle is relative to the start/end of the given time series.
#'    Therefore, a time interval of interest should be provided (e.g.,
#'    the light exposure between waking up and going to bed).
#'
#' @name xlit_angle
NULL

# FLiT Angle --------------------------------------------------------------

#' @rdname xlit_angle
#'
#' @details `flit_angle()` calculates the phase angle between the first timing of
#'    light above/below threshold and the start of the given interval.
#'
#' @export
#'
flit_angle <- function(lightVar,
                       dtVar,
                       threshold,
                       unit_out = "mins",
                       as_df = TRUE,
                       wide = TRUE) {
  df <- tibble::tibble(threshold = numeric(), flit_angle = numeric())

  # Calculate FLiT Angle
  for (c in threshold) {
    flit <- dtVar[threshold(lightVar, c)][1]
    flit_angle <- (flit - dtVar[1])
    if (lubridate::is.POSIXct(dtVar)) {
      flit_angle <- flit_angle %>% as.numeric(units = unit_out)
    }
    df <- df %>% tidyr::add_row(threshold = c, flit_angle = flit_angle)
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = threshold,
        values_from = flit_angle,
        names_prefix = "flit_angle.")
  }

  # Return data frame or numeric matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}


# LLiT Angle --------------------------------------------------------------

#' @rdname xlit_angle
#'
#' @details `llit_angle()` calculates the phase angle between the last timing of
#'    light above/below threshold and the end of the given interval.
#'
#' @export
#'
llit_angle <- function(lightVar,
                       dtVar,
                       threshold,
                       unit_out = "mins",
                       as_df = TRUE,
                       wide = TRUE) {
  df <- tibble::tibble(threshold = numeric(), llit_angle = numeric())

  # Calculate LLiT Angle
  for (c in threshold) {
    llit <- dtVar[threshold(lightVar, c)] %>% dplyr::last()
    llit_angle <- (dtVar[length(dtVar)] - llit)
    if (lubridate::is.POSIXct(dtVar)) {
      llit_angle <- llit_angle %>% as.numeric(units = unit_out)
    }
    df <- df %>% tidyr::add_row(threshold = c, llit_angle = llit_angle)
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = threshold,
        values_from = llit_angle,
        names_prefix = "llit_angle.")
  }

  # Return data frame or numeric matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}
