#' Title
#'
#' @param lightVar
#' @param dtVar
#' @param threshold
#' @param na.rm
#' @param as_df
#' @param wide
#'
#' @return
#' @export
#'
#' @examples
mlit = function(lightVar,
                dtVar,
                threshold,
                na.rm = FALSE,
                as_df = TRUE,
                wide = TRUE){

  df = tibble::tibble(threshold = numeric(),
                      mlit = numeric())
  # Calculate MLIT
  for(c in threshold){
    mlit = mean(as.numeric(dtVar[threshold(lightVar,c)]), na.rm = na.rm)
    df = df %>% tidyr::add_row(threshold = c, mlit = mlit)
  }

  # Convert to POSIXct
  if(lubridate::is.POSIXct(dtVar))
    df = df %>%
      dplyr::mutate_at(dyplr::vars(mlit), round) %>%
      dyplr::mutate_at(dyplr::vars(mlit), lubridate::as_datetime,
                       tz = lubridate::tz(dtVar))

  # Reshape to wide format
  if(wide){
    df = df %>% tidyr::pivot_wider(names_from = threshold, values_from = mlit)
    if(ncol(df) == 1)
      names(df) = paste0("mlit.", names(df))
  }

  # Return data frame or matrix
  if(as_df) return(df)
  else return(as.numeric(df))
}
