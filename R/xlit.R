#' Mean/first/last timing above/below threshold.
#'
#' These functions calculate the mean/first/last timepoint where light levels are
#' above or below a given threshold intensity within the given interval.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param dtVar Vector containing the time data. Can be POSIXct or numeric.
#' @param threshold Single numeric value or vector specifying threshold
#'    intensities. The sign indicates above/below (see \code{\link{threshold}}).
#' @param na.rm Logical. Should missing values be removed? Defaults to TRUE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of threshold and calculated values.
#'    If wide is TRUE then variable names will be concatenated with the threshold.
#'
#' @name xlit
NULL

# MLiT --------------------------------------------------------------------

#' @rdname xlit
#'
#' @details `mlit()` calculates the mean timing of light above/below threshold.
#'
#' @references Reid, K. J., Santostasi, G., Baron, K. G., Wilson, J., Kang, J.,
#'    & Zee, P. C. (2014). Timing and Intensity of Light Correlate with Body Weight
#'     in Adults. \emph{PLOS ONE}, 9(4), e92251.
#'      \url{https://doi.org/10.1371/journal.pone.0092251}

#'
#' @export
#'
mlit = function(lightVar,
                dtVar,
                threshold,
                as_df = TRUE,
                wide = TRUE){

  df = tibble::tibble(threshold = numeric(), mlit = numeric())

  # Calculate MLiT
  for(c in threshold){
    mlit = dtVar[threshold(lightVar,c)] %>% as.numeric() %>% mean()
    df = df %>% tidyr::add_row(threshold = c, mlit = mlit)
  }

  # Convert to POSIXct
  if(lubridate::is.POSIXct(dtVar)){
    df = df %>%
      dplyr::mutate_at(dyplr::vars(mlit), round) %>%
      dyplr::mutate_at(dyplr::vars(mlit), lubridate::as_datetime,
                       tz = lubridate::tz(dtVar))
  }

  # Reshape to wide format
  if(wide){
    df = df %>% tidyr::pivot_wider(names_from = threshold, values_from = mlit)
    if(ncol(df) == 1) names(df) = paste0("mlit.", names(df))
  }

  # Return data frame or matrix
  if(as_df) return(df)
  else return(as.numeric(df))
}

# FLiT --------------------------------------------------------------------

#' @rdname xlit
#'
#' @details `flit()` calculates the first timing of light above/below threshold.
#'
#' @export
#'
flit = function(lightVar,
                dtVar,
                threshold,
                as_df = TRUE,
                wide = TRUE){

  df = tibble::tibble(threshold = numeric(), flit = numeric())

  # Calculate FLiT
  for(c in threshold){
    flit = dtVar[threshold(lightVar,c)][1] %>% as.numeric()
    df = df %>% tidyr::add_row(threshold = c, flit = flit)
  }

  # Convert to POSIXct
  if(lubridate::is.POSIXct(dtVar)){
    df = df %>%
      dplyr::mutate_at(dyplr::vars(flit), round) %>%
      dyplr::mutate_at(dyplr::vars(flit), lubridate::as_datetime,
                       tz = lubridate::tz(dtVar))
  }

  # Reshape to wide format
  if(wide){
    df = df %>% tidyr::pivot_wider(names_from = threshold, values_from = flit)
    if(ncol(df) == 1) names(df) = paste0("flit.", names(df))
  }

  # Return data frame or matrix
  if(as_df) return(df)
}

# LLiT --------------------------------------------------------------------

#' @rdname xlit
#'
#' @details `llit()` calculates the last timing of light above/below threshold.
#'
#' @export
#'
llit = function(lightVar,
                dtVar,
                threshold,
                as_df = TRUE,
                wide = TRUE){

  df = tibble::tibble(threshold = numeric(), llit = numeric())

  # Calculate LLiT
  for(c in threshold){
    llit = dtVar[threshold(lightVar,c)] %>% dplyr::last() %>% as.numeric()
    df = df %>% tidyr::add_row(threshold = c, llit = llit)
  }

  # Convert to POSIXct
  if(lubridate::is.POSIXct(dtVar)){
    df = df %>%
      dplyr::mutate_at(dyplr::vars(llit), round) %>%
      dyplr::mutate_at(dyplr::vars(llit), lubridate::as_datetime,
                       tz = lubridate::tz(dtVar))
  }

  # Reshape to wide format
  if(wide){
    df = df %>% tidyr::pivot_wider(names_from = threshold, values_from = llit)
    if(ncol(df) == 1) names(df) = paste0("llit.", names(df))
  }

  # Return data frame or matrix
  if(as_df) return(df)
}
