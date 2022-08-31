#' Example light data.
#'
#' A dataset containing personal light exposure data measured across 24 hours
#' with a recording interval of 60 seconds
#'
#' @format A data frame with 1440 rows and 3 variables:
#' \describe{
#'   \item{datetime}{datetime, as yy/mm/dd HH:MM:SS}
#'   \item{light}{illuminance, in lux}
#'   \item{awake}{logical, whether subject was awake}
#' }
"example_light"
