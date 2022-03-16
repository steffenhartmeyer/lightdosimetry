#' Cumulative exposure (CE)
#'
#' This function calculates the integral of light exposure across the given
#' interval.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Single column data frame or vector.
#' @export
#'
#' @examples
cumulative_exposure <- function(lightVar,
                                as_df = TRUE) {
  ce <- sum(lightVar, na.rm = TRUE)
  if (as_df) {
    return(tibble::tibble(CE = ce))
  } else {
    return(ce)
  }
}
