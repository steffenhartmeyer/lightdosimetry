#' Midpoint of cumulative light exposure.
#'
#' This function calculates the timing corresponding to half of the cumulative
#' light exposure within the given time series.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param dtVar Vector containing the time data. Can be POSIXct or numeric.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Single column data frame or vector.
#' @export
#'
#' @examples
midpointCE = function(lightVar, dtVar, as_df = TRUE){
  lightVar[is.na(lightVar)] = 0
  cumsum = cumsum(lightVar)
  halfSum = cumsum[length(cumsum)]/2
  midpoint = which.min(abs(cumsum-halfSum))

  if(as_df) return(tibble::tibble(midpointCE = dtVar[midpoint]))
  else return(dtVar[midpoint])
}
