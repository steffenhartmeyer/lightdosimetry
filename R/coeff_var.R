#' Coefficient of variation
#'
#' This function calculates the coefficient of variation, defined as the standard
#' deviation divided by the mean.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Data frame or numeric vector.
#' @export
#'
#' @examples
coeff_var <- function(lightVar,
                      na.rm = TRUE,
                      as_df = TRUE) {
  cv <- sd(lightVar, na.rm = na.rm) / mean(lightVar, na.rm = na.rm)
  if (as_df) {
    return(tibble::tibble(coeff_var = cv))
  } else {
    return(cv)
  }
}
