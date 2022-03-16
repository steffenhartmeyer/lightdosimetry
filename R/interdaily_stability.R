#' Interdaily stability (IS)
#'
#' This function calculates the variability of 24h light exposure patterns across
#' multiple days. Calculated as the ratio of the variance of the average daily
#' pattern to the total variance across all days. Calculated for mean hourly
#' light levels. Ranges between 0 (Gaussian noise) and 1 (Perfect Stability).
#'
#' @param lightVar Numeric vector containing the light data.
#' @param datetimeVar Vector containing the datetime data. Must be POSIXct.
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
interdaily_stability <- function(lightVar,
                                 datetimeVar,
                                 as_df = TRUE) {

  if(!lubridate::is.POSIXct(datetimeVar)){
    stop("Datetime variable must be POSIXct!")
  }

  # Hourly averages for each day
  total_hourly <- tibble::tibble(light = lightVar,
                                 datetime = datetimeVar) %>%
    dplyr::group_by(floor_date(datetime, unit = "1 hour")) %>%
    dplyr::summarise(light = mean(light, na.rm = TRUE))

  # Hourly average across all days
  avg_hourly <- total_hourly %>%
    dplyr::group_by(hour = lubridate::hour(datetime)) %>%
    dplyr::summarise(light = mean(light, na.rm = TRUE))

  # Variance across average day / variance across all days
  is <- var(avg_hourly$light) / var(total_hourly$light)

  # Return data frame or numeric matrix
  if (as_df) {
    return(tibble::tibble(IS = is))
  } else {
    return(is)
  }
}
