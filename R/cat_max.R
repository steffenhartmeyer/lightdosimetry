#' Length of longest continuous cluster above/below threshold
#'
#' @param lightVar Numeric vector containing the light data.
#' @param threshold Single numeric value or vector specifying threshold
#'    intensities. The sign indicates above/below (see \code{\link{threshold}}).
#' @param sampling_int Numeric. Sampling interval in seconds. If not specified
#'    (default), no time scaling will be performed.
#' @param unit_out Character. Time unit of output. Possible values are
#'    ("seconds", "minutes", "hours", "days"), which can be abbreviated.
#'    If not specified (default), no time scaling will be performed.
#' @param loop Logical. Should the data be looped? Defaults to FALSE.
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
cat_max <- function(lightVar,
                    threshold,
                    sampling_int = NULL,
                    unit_out = NULL,
                    loop = FALSE,
                    as_df = TRUE,
                    wide = TRUE) {

  # Check whether sampling interval and output unit specified
  if (is.null(sampling_int) | is.null(unit_out)) {
    warning("No sampling interval and/or output unit specified. Returning raw output.")
    sampling_int <- 1
    unit_out <- "secs"
  }

  # Loop data
  if (loop) {
    lightVar <- c(lightVar, lightVar)
  }

  # Function to find longest cluster
  max_clust <- function(x) {
    x[is.na(x)] <- 0
    z <- c(x, 0)
    z <- (cumsum(z) * c(diff(z) < 0, 0))
    max(diff(c(0, 0, z[z != 0])))
  }

  df <- tibble::tibble(
    threshold = numeric(),
    cat_max = numeric()
  )
  for (c in threshold) {
    cat_max <- (max_clust(threshold(lightVar, c)) * sampling_int) %>%
      from.secs(unit_out)
    df <- df %>% tibble::add_row(
      threshold = c,
      cat_max = cat_max
    )
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = threshold,
        values_from = cat_max,
        names_prefix = "cat_max."
      )
  }

  # Return as data frame or numeric
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}
