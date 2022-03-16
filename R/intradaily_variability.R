#' Intradaily variability (IV)
#'
#' This function calculates the variability of consecutive light levels within
#' a 24h day. Calculated as the ratio of the variance of the differences between
#' consecutive light levels to the total variance across the day. Calculated for
#' mean hourly light levels. Higher values indicate more fragmentation.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param datetimeVar Vector containing the time data. Must be POSIXct.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Single column data frame or vector.
#' @export
#'
#' @references Van Someren, E. J. W., Swaab, D. F., Colenda, C. C., Cohen, W.,
#'    McCall, W. V., & Rosenquist, P. B. (1999). Bright Light Therapy: Improved
#'    Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer Patients
#'    by Application of Nonparametric Methods. \emph{Chronobiology International},
#'    16(4), 505–518. \url{https://doi.org/10.3109/07420529908998724}
#'
#' @examples
intradaily_variability <- function(lightVar,
                                   datetimeVar,
                                   as_df = TRUE) {

  if(!lubridate::is.POSIXct(datetimeVar)){
    stop("Datetime variable must be POSIXct!")
  }

  # Hourly averages for each day
  total_hourly <-
    tibble::tibble(light = lightVar, datetime = datetimeVar) %>%
    dplyr::group_by(floor_date(datetime, unit = "1 hour")) %>%
    dplyr::summarise(light = mean(light, na.rm = TRUE))

  # Variance of consecutive hourly differences
  var_hourly_diff <- sum(diff(total_hourly$light, 1)^2) / (length(total_hourly$light) - 1)

  # Variance of consecutive differences / variance across all days
  iv <- var_hourly_diff / var(total_hourly$light)

  # Return data frame or numeric vector
  if (as_df) {
    return(tibble::tibble(IV = iv))
  } else {
    return(iv)
  }
}
