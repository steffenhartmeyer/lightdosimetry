#' Interval to data frame rows
#'
#' This function reshapes a data frame from a given interval to continuously
#' spaced rows.
#' @param df The data frame to be reshaped.
#' @param rowvar Name of the new variable holding the row indices.
#' @param from Start of interval. Numeric or POSIXct.
#' @param to End of interval. Numeric or POSIXct.
#' @param by Numeric. Spacing between individual rows. Needs to be in same unit
#'    as interval variables (e.g., seconds for POSIXct)
#' @param removeBounds Logical. Should start and end variables be removed?
#'    Defaults to TRUE.
#'
#' @return Reshaped data frame.
#' @export
#'
#' @examples
intervalToRows <- function(df, rowvar, from, to, by, removeBounds = TRUE) {
  df <- df %>%
    rowwise() %>%
    mutate({{ rowvar }} := ifelse(is.na({{ from }}) | is.na({{ to }}),
      list(NA),
      list(seq({{ from }}, {{ to }}, by))
    )) %>%
    unnest(cols = c({{ rowvar }})) %>%
    ungroup()
  if (removeBounds) df <- df %>% select(!c({{ from }}, {{ to }}))
  return(df)
}
