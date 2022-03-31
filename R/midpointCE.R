#' Midpoint of cumulative light exposure.
#'
#' This function calculates the timing corresponding to half of the cumulative
#' light exposure within the given time series.
#'
#' @param lightVar Numeric vector containing the light data. Missing values are
#'    replaced with 0.
#' @param timeVar Vector containing the time data. Can be POSIXct or numeric.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Single column data frame or vector.
#' @export
#'
#' @references Shochat, T., Santhi, N., Herer, P., Flavell, S. A., Skeldon, A. C.,
#'   & Dijk, D.-J. (2019). Sleep Timing in Late Autumn and Late Spring Associates
#'  With Light Exposure Rather Than Sun Time in College Students.
#'  \emph{Frontiers in Neuroscience}, 13. \url{https://doi.org/10.3389/fnins.2019.00882}
#'
#' @examples
midpointCE <- function(lightVar,
                       timeVar,
                       as_df = TRUE) {

  # Replace missing values with 0
  lightVar[is.na(lightVar)] <- 0

  # Find midpoint of CE
  cumsum <- cumsum(lightVar)
  halfSum <- cumsum[length(cumsum)] / 2
  midpoint <- which.min(abs(cumsum - halfSum))

  # Return as data frame or numeric vector
  if (as_df) {
    return(tibble::tibble(midpointCE = timeVar[midpoint]))
  } else {
    return(timeVar[midpoint])
  }
}
