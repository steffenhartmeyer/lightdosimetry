
#' Disparity index
#'
#' This function calculates the continuous disparity index as described in
#' Fernández-Martínez et al. (2018).
#'
#' @param lightVar Numeric vector containing the light data.
#' @param na_rm Logical. Should missing values be removed? Defaults to TRUE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Data frame or numeric vector.
#' @export
#'
#' @references Fernández-Martínez, M., Vicca, S., Janssens, I. A., Carnicer, J.,
#'   Martín-Vide, J., & Peñuelas, J. (2018).
#'   The consecutive disparity index, D: A measure of temporal variability in
#'   ecological studies. \emph{Ecosphere}, 9(12), e02527.
#'   \url{https://doi.org/10.1002/ecs2.2527}
#'
#' @examples
disparity_index <- function(lightVar,
                            na_rm = TRUE,
                            as_df = TRUE) {

  # Remove NAs
  if (na_rm){
    lightVar <- na.omit(lightVar)
  }

  # Calculate disparity index
  fractions <- (lightVar[2:length(lightVar)] + 1) /
               (lightVar[1:length(lightVar)-1] + 1)
  di <- 1 / (length(lightVar) - 1) * sum(abs(log(fractions)))

  # Return as data frame or numeric vector
  if (as_df) {
    return(tibble::tibble(disparity_index = di))
  } else {
    return(di)
  }
}
