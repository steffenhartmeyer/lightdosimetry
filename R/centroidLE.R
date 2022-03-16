#' Centroid of light exposure
#'
#' This function calculates the centroid of light exposure as the mean of the
#' time vector weighted in proportion to the corresponding light intensity.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param timeVar Vector containing the time data. Can be POSIXct or numeric.
#' @param bin_size Numeric value or character specifying size of bins to average
#'    light data. If timeVar is POSIXct it must be a string with the time followed
#'    by its unit (e.g., "1 hour", "30 mins"). If NULL then no binning will be
#'    performed. Defaults to NULL.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Single column data frame or vector.
#' @export
#'
#' @references Phillips, A. J. K., Clerx, W. M., O’Brien, C. S., Sano, A., Barger,
#'    L. K., Picard, R. W., Lockley, S. W., Klerman, E. B., & Czeisler, C. A. (2017).
#'    Irregular sleep/wake patterns are associated with poorer academic performance
#'    and delayed circadian and sleep/wake timing. \emph{Scientific Reports},
#'    7(1), 3216. \url{https://doi.org/10.1038/s41598-017-03171-4}

#'
#' @examples
centroidLE <- function(lightVar,
                       timeVar,
                       bin_size = NULL,
                       as_df = TRUE) {
  df <- tibble::tibble(
    light = lightVar,
    time = timeVar
  )

  if (!is.null(bin_size)) {
    # Check whether bin size specification is correct
    if ((is.character(bin_size) & !lubridate::is.POSIXct(timeVar)) |
        (!is.character(bin_size) & lubridate::is.POSIXct(timeVar))) {
      stop("Bin size specification not compatible with type of time variable!")
    }
    # Average into bins
    if(lubridate::is.POSIXct(timeVar)){
      df <- df %>%
        dplyr::group_by(time = lubridate::floor_date(time, unit = bin_size)) %>%
        dplyr::summarise(light = mean(light, na.rm = TRUE))
    }
    else{
      df <- df %>%
        dplyr::group_by(time = cut(time, breaks = bin_size)) %>%
        dplyr::summarise(light = mean(light, na.rm = TRUE))
    }
  }

  # Calculate weighted mean
  weights <- (df$light / sum(df$light, na.rm = TRUE))
  centroidLE <- sum(as.numeric(df$time) * weights, na.rm = TRUE)

  # Convert back to POSIXct
  if (lubridate::is.POSIXct(timeVar)) {
    centroidLE <- centroidLE %>%
      round() %>%
      lubridate::as_datetime(tz = lubridate::tz(timeVar))
  }

  # Return data frame or numeric vector
  if (as_df) {
    return(tibble::tibble(centroidLE = centroidLE))
  } else {
    return(centroidLE)
  }
}
