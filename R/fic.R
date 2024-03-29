#' Frequency of intensity changes
#'
#' This functions calculates the times a given threshold
#' intensity is crossed.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param threshold Single numeric value or vector specifying threshold
#'    intensities. The sign indicates above/below (see \code{\link{threshold}}).
#' @param na_rm Logical. Should missing light values be removed? Defaults to FALSE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of threshold and calculated values.
#'    If `wide` is TRUE then variable names will be concatenated with the threshold.
#' @export
#'
#' @references Alvarez, A. A., & Wildsoet, C. F. (2013). Quantifying light
#'    exposure patterns in young adult students. \emph{Journal of Modern Optics},
#'    60(14), 1200–1208. \url{https://doi.org/10.1080/09500340.2013.845700}
#'
#' @examples
fic <- function(lightVar,
                threshold,
                na_rm = FALSE,
                as_df = TRUE,
                wide = TRUE) {

  # Remove missing values
  if (na_rm) {
    lightVar <- na.omit(lightVar)
  }

  df <- tibble::tibble(threshold = numeric(), fic = numeric())

  # Calculate FIC
  for (c in threshold) {
    fic <- sum(abs(diff(threshold(lightVar, c))))
    df <- df %>% tibble::add_row(threshold = c, fic = fic)
  }

  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = threshold,
        values_from = fic,
        names_prefix = "fic."
      )
  }

  # Return data frame or numeric matrix
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}
