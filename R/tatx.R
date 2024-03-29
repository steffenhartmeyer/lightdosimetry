#' Time above/below threshold or within threshold range
#'
#' These functions calculate the duration above/below a specified threshold
#' intensity or within a specified intensity range, within the time series provided.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param threshold Single numeric value or vector specifying threshold
#'    intensities. The sign indicates above/below (see \code{\link{threshold}}).
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
#'    If wide is TRUE then variable names will be concatenated with the threshold.
#'
#' @name tatx
NULL

#' @rdname tatx
#'
#' @details `tat()` calculates the time above/below threshold.
#'
#' @export
#'
tat <- function(lightVar,
                threshold,
                sampling_int = NULL,
                unit_out = NULL,
                as_df = TRUE,
                wide = TRUE) {

  # Check whether sampling interval and output unit specified
  if (is.null(sampling_int) | is.null(unit_out)) {
    warning("No sampling interval and/or output unit specified. Returning raw output.")
    sampling_int <- 1
    unit_out <- "secs"
  }

  df <- tibble::tibble(
    threshold = numeric(),
    tat = numeric()
  )
  for (c in threshold) {
    tat <- (sum(threshold(lightVar, c)) * sampling_int) %>%
      from.secs(unit_out)
    df <- df %>% tibble::add_row(threshold = c, tat = tat)
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>% tidyr::pivot_wider(
      names_from = threshold,
      values_from = tat,
      names_prefix = "tat."
    )
  }
  # Return as df or numeric matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}


#' @rdname tatx
#'
#' @details `tatr()` calculates the time within threshold range.
#'
#' @export
#'
tatr <- function(lightVar,
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

  # Check whether sampling interval and output unit specified
  if (is.null(sampling_int) | is.null(unit_out)) {
    warning("No sampling interval and/or output unit specified. Returning raw output.")
    sampling_int <- 1
    unit_out <- "secs"
  }

  df <- tibble::tibble(
    threshold_min = numeric(),
    threshold_max = numeric(),
    tat = numeric()
  )
  for (i in 1:length(lower)) {
    cmin <- lower[i]
    cmax <- upper[i]
    tat <- (sum(dplyr::between(lightVar, cmin, cmax)) * sampling_int) %>%
      from.secs(unit_out)
    df <- df %>%
      tibble::add_row(
        threshold_min = cmin,
        threshold_max = cmax,
        tat = tat
      )
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::unite(threshold, threshold_min, threshold_max) %>%
      tidyr::pivot_wider(
        names_from = threshold,
        values_from = tat,
        names_prefix = "tat."
      )
  }

  # Return as data frame or matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}
