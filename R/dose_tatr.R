#' Dose within threshold range
#'
#' This function calculates the light dose within a specified intensity range.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param lower Single numeric value or vector specifying the lower bound of the
#'    threshold intensity range. The sign indicates above/below
#'    (see \code{\link{threshold}}). Must be same length as upper bound.
#' @param upper Single numeric value or vector specifying the upper bound of the
#'    threshold intensity range. The sign indicates above/below
#'    (see \code{\link{threshold}}). Must be same length as lower bound.
#' @param sampling_int Numeric. Sampling interval in seconds. If not specified
#'    (default), no time scaling will be performed.
#' @param unit_out Character. Time unit of output. Possible values are
#'    ("seconds", "minutes", "hours", "days"), which can be abbreviated.
#'    If not specified (default), no time scaling will be performed.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of threshold and calculated values.
#'    If `wide` is TRUE then variable names will be concatenated with the
#'    threshold.
#' @export
#'
#' @examples
dose_tatr <- function(lightVar,
                      lower,
                      upper,
                      sampling_int = NULL,
                      unit_out = NULL,
                      as_df = TRUE,
                      wide = TRUE) {

  # Check that lower and upper bounds are same length
  if (length(lower) != length(upper)) {
    stop("Lower and upper bounds must be same length.")
  }

  if (is.null(sampling_int) | is.null(unit_out)) {
    warning("No sampling interval and/or output unit specified. Returning raw output.")
    sampling_int <- 1
    unit_out <- "secs"
  }

  df <- tibble::tibble(
    threshold_min = numeric(),
    threshold_max = numeric(),
    dose_tat = numeric()
  )
  for (i in 1:length(lower)) {
    cmin <- lower[i]
    cmax <- upper[i]
    dose_tat <- (sum(dplyr::between(lightVar, cmin, cmax), na.rm = TRUE) * sampling_int) %>%
      from.secs(unit_out) * ((cmax - cmin) / 2)
    df <- df %>% tibble::add_row(
      threshold_min = cmin,
      threshold_max = cmax,
      dose_tat = dose_tat
    )
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::unite(threshold, threshold_min, threshold_max) %>%
      tidyr::pivot_wider(
        names_from = threshold, values_from = dose_tat,
        names_prefix = "dose_tat."
      )
  }

  # Return as data frame or matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}
