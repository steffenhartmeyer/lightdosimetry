#' Mean/first/last timing above/below threshold.
#'
#' These functions calculate the mean/first/last timepoint where light levels are
#' above or below a given threshold intensity within the given interval.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param timeVar Vector containing the time data. Can be POSIXct or numeric.
#' @param threshold Single numeric value or vector specifying threshold
#'    intensities. The sign indicates above/below (see \code{\link{threshold}}).
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of threshold and calculated values.
#'    If wide is TRUE then variable names will be concatenated with the threshold.
#'
#' @name xlit
NULL

# MLiT --------------------------------------------------------------------

#' @rdname xlit
#'
#' @details `mlit()` calculates the mean timing of light above/below threshold.
#'
#' @references Reid, K. J., Santostasi, G., Baron, K. G., Wilson, J., Kang, J.,
#'    & Zee, P. C. (2014). Timing and Intensity of Light Correlate with Body Weight
#'     in Adults. \emph{PLOS ONE}, 9(4), e92251.
#'      \url{https://doi.org/10.1371/journal.pone.0092251}
#'
#' @export
#'
mlit <- function(lightVar,
                 timeVar,
                 threshold,
                 as_df = TRUE,
                 wide = TRUE) {
  df <- tibble::tibble(threshold = numeric(), mlit = numeric())

  # Calculate MLiT
  for (c in threshold) {
    mlit <- timeVar[threshold(lightVar, c)] %>%
      as.numeric() %>%
      mean()
    df <- df %>% tibble::add_row(threshold = c, mlit = mlit)
  }

  # Convert to POSIXct
  if (lubridate::is.POSIXct(timeVar)) {
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(mlit), round) %>%
      dplyr::mutate_at(dplyr::vars(mlit), lubridate::as_datetime,
        tz = lubridate::tz(timeVar)
      )
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = threshold,
        values_from = mlit,
        names_prefix = "mlit.")
  }

  # Return data frame or matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}

# FLiT --------------------------------------------------------------------

#' @rdname xlit
#'
#' @details `flit()` calculates the first timing of light above/below threshold.
#'
#' @export
#'
flit <- function(lightVar,
                 timeVar,
                 threshold,
                 as_df = TRUE,
                 wide = TRUE) {
  df <- tibble::tibble(threshold = numeric(), flit = numeric())

  # Calculate FLiT
  for (c in threshold) {
    flit <- timeVar[threshold(lightVar, c)][1] %>% as.numeric()
    df <- df %>% tibble::add_row(threshold = c, flit = flit)
  }

  # Convert to POSIXct
  if (lubridate::is.POSIXct(timeVar)) {
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(flit), round) %>%
      dplyr::mutate_at(dplyr::vars(flit), lubridate::as_datetime,
        tz = lubridate::tz(timeVar)
      )
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = threshold,
        values_from = flit,
        names_prefix = "flit.")
  }

  # Return data frame or matrix
  if (as_df) {
    return(df)
  }
}

# LLiT --------------------------------------------------------------------

#' @rdname xlit
#'
#' @details `llit()` calculates the last timing of light above/below threshold.
#'
#' @export
#'
llit <- function(lightVar,
                 timeVar,
                 threshold,
                 as_df = TRUE,
                 wide = TRUE) {
  df <- tibble::tibble(threshold = numeric(), llit = numeric())

  # Calculate LLiT
  for (c in threshold) {
    llit <- timeVar[threshold(lightVar, c)] %>%
      dplyr::last() %>%
      as.numeric()
    df <- df %>% tibble::add_row(threshold = c, llit = llit)
  }

  # Convert to POSIXct
  if (lubridate::is.POSIXct(timeVar)) {
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(llit), round) %>%
      dplyr::mutate_at(dplyr::vars(llit), lubridate::as_datetime,
        tz = lubridate::tz(timeVar)
      )
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = threshold,
        values_from = llit,
        names_prefix = "llit.")
  }

  # Return data frame or matrix
  if (as_df) {
    return(df)
  }
}
