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
