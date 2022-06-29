#' Light above or below threshold?
#'
#' Convenience function to determine whether light is above or below threshold.
#'
#' @param lightVar Numeric value or vector of light data.
#' @param threshold Single numeric value specifying threshold
#'    intensity. The sign indicates above/below (negative values = below,
#'    positive value = above). This operation is always including the threshold
#'    value.
#'
#' @return Logical vector.
#' @export
#'
#' @examples
threshold <- function(lightVar, threshold) {
  if (threshold < 0) {
    out <- lightVar <= abs(threshold)
  } else {
    out <- lightVar >= threshold
  }
  tidyr::replace_na(out, FALSE)
}
