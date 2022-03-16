#' Exponential moving average filter (EMA)
#'
#' This function smoothes the data using an exponential moving average filter
#' with a specified decay.
#'
#' @param lightVar Numeric vector containing the light data. Missing values are
#'    replaced by 0.
#' @param decay Numeric value or character in the format "[numeric] [unit]", with
#'    possible units ("seconds","minutes","hours","days"); units can be
#'    abbreviated, see \code{\link{parse_timeunit_tosecs}}.
#' @param beta Logical. Is `decay` refering directly to beta value? If
#'    TRUE, `decay` must be a numeric value.
#' @param sampling_int Numeric. Sampling interval in seconds. Defaults to 60.
#'
#' @return Numeric vector of filtered light data
#' @export
#'
#' @examples
ema <- function(lightVar,
                decay,
                beta = FALSE,
                sampling_int = 60) {

  # Replace missing values with 0
  lightVar[is.na(lightVar)] <- 0

  # Parse decay half-life
  if (!beta) {
    if (!is.numeric(decay)) {
      decay <- parse_timeunit_tosecs(decay) / sampling_int
    }
    beta <- log(2) / decay
  } else {
    if (!is.numeric(decay)) {
      stop("Beta must be a numeric value!")
    }
  }

  # EMA filter
  D <- replicate(length(lightVar), 0)
  for (idx in 1:length(lightVar)) {
    if (idx == 1) {
      D[idx] <- beta * (lightVar[idx])
    } else {
      D[idx] <- D[idx - 1] + beta * (lightVar[idx] - D[idx - 1])
    }
  }

  # Return numeric vector of EMA light values
  return(D)
}
