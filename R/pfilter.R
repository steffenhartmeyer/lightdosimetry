#' Filter and print
#'
#' This function filters data (like dplyr's filter) and prints the number of
#' rows filtered by each condition.
#'
#' @param df Data frame to be filtered.
#' @param ... Filter conditions. See \code{\link[dplyr]{filter}}
#'
#' @return Filtered data frame
#' @export
#'
#' @examples
pfilter <- function(df, ...) {
  vars <- as.list(substitute(list(...)))[-1L]
  for (arg in vars) {
    df_new <- df %>% filter(!!arg)
    rows_filtered <- nrow(df) - nrow(df_new)
    percent_filtered <- rows_filtered / nrow(df)*100
    cat(sprintf(
      "Filtered out %s rows (%.2f%%) using: %s\n",
      rows_filtered, percent_filtered, deparse(arg)
    ))
    df <- df_new
  }
  return(df)
}
