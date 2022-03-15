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
#' @param sampling_int Numeric. Sampling interval in seconds. Defaults to 60.
#' @param unit_out Character. Time unit of output. Possible values are
#'    ("secs", "mins", "hours", "days"). Can be abbreviated. Defaults to "mins".
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of threshold and calculated values.
#'    If wide is TRUE then variable names will be concatenated with the threshold.
#' @export
#'
#' @examples
#' @name tat
NULL
#> NULL

#' @rdname tat
tat <- function(lightVar,
                threshold,
                sampling_int = 60,
                unit_out = "mins",
                na.rm = TRUE,
                as_df = TRUE,
                wide = TRUE) {
  df <- tibble::tibble(
    threshold = numeric(),
    tat = numeric()
  )
  for (c in threshold) {
    val <- (sum(threshold(lightVar, c), na.rm = na.rm) * sampling_int) %>%
      from.secs(unit_out)
    df <- df %>% tibble::add_row(threshold = c, tat = val)
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>% tidyr::pivot_wider(names_from = threshold, values_from = tat,
                                    names_sep = ".")
    if (ncol(df) == 1) {
      names(df) <- paste0("tat.", names(df))
    }
  }
  # Return as df or numeric matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}


#' @rdname tat
tatr <- function(lightVar,
                 lower,
                 upper,
                 sampling_int = 60,
                 unit_out = "mins",
                 na.rm = TRUE,
                 as_df = TRUE,
                 wide = TRUE) {

  # Check that lower and upper bounds are same length
  if (length(lower) != length(upper)) {
    stop("Lower and upper bounds must be same length.")
  }

  df <- tibble::tibble(
    threshold_min = numeric(),
    threshold_max = numeric(),
    tat = numeric()
  )
  for (i in 1:length(lower)) {
    cmin <- lower[i]
    cmax <- upper[i]
    val <- (sum(between(lightVar, cmin, cmax), na.rm = na.rm) * sampling_int) %>%
      from.secs(unit_out)
    df <- df %>% tibble::add_row(threshold_min = cmin, threshold_max = cmax, tat = val)
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::unite(threshold, threshold_min, threshold_max) %>%
      tidyr::pivot_wider(names_from = threshold, values_from = tat,
                         names_sep = ".")
    if (ncol(df) == 1) names(df) <- paste0("tat.", names(df))
  }

  # Return as data frame or matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}
