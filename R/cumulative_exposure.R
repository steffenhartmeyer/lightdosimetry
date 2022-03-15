#' Cumulative exposure (CE)
#'
#' This function calculates the integral of light exposure across the given
#' interval.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Single column data frame or vector.
#' @export
#'
#' @references Price, L. L. A. (2014). On the Role of Exponential Smoothing in
#'    Circadian Dosimetry. \emph{Photochemistry and Photobiology}, 90(5),
#'    \url{1184–1192. https://doi.org/10.1111/php.12282}
#'
#' @examples
cumulative_exposure = function(lightVar,
                               na.rm = TRUE,
                               as_df=TRUE){
  ce = sum(lightVar, na.rm = na.rm)
  if(as_df){
    return(tibble::tibble(CE = ce))
  }
  else{
    return(ce)
  }
}
