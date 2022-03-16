#' Coefficient of variation
#'
#' This function calculates the coefficient of variation, defined as the standard
#' deviation divided by the mean.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param na_rm Logical. Should missing light values be removed? Defaults to TRUE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Data frame or numeric vector.
#' @export
#'
#' @examples
coeff_var <- function(lightVar,
                      na_rm = TRUE,
                      as_df = TRUE) {

  # Remove NAs
  if (na_rm) {
    lightVar <- na.omit(lightVar)
  }

  cv <- sd(lightVar) / mean(lightVar)

  if (as_df) {
    return(tibble::tibble(coeff_var = cv))
  } else {
    return(cv)
  }
}
