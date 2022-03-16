#' Light Quality Index
#'
#' This function calculates the light quality index as described in Martinez-Nicolas
#' et al. (2011).
#'
#' @param lightVar Numeric vector containing the light data.
#' @param upper Single numeric value specifying the threshold of the bright light
#'    level. Defaults to 500 lx as described in Martinez-Nicolas et al. (2011).
#' @param lower Single numeric value specifying the threshold of the dark light
#'    level. Defaults to 10 lx as described in Martinez-Nicolas et al. (2011).
#' @param sampling_int Numeric. Sampling interval in seconds. Defaults to 60.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Data frame or numeric vector.
#' @export
#'
#' @details Should be calculated for the period from waking up to going to bed.
#'    Default threshold values are provided in photopic illuminance as per the
#'    original paper. To calculate the original metric make sure that
#'    `lightVar` is photopic illuminance.
#'
#' @references Martinez-Nicolas, A., Ortiz-Tudela, E., Madrid, J. A., & Rol, M.
#'    A. (2011). Crosstalk Between Environmental Light and Internal Time in Humans.
#'    \emph{Chronobiology International}, 28(7), 617–629.
#'    \url{https://doi.org/10.3109/07420528.2011.593278}
#'
#'
#' @examples
lqi <- function(lightVar,
                upper = 500,
                lower = 10,
                sampling_int = 60,
                as_df = TRUE) {

  high <- tat(lightVar, upper, sampling_int)[[1]]
  low <- tat(lightVar, -1 * lower, sampling_int)[[1]]
  lqi <- (high - low) / (high + low)
  if (as_df) {
    return(tibble::tibble(LQI = lqi))
  } else {
    return(lqi)
  }
}
