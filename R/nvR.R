
# Response functions ------------------------------------------------------

#' Non-visual direct response
#'
#' @param irr_mel Numeric vector. Melanopic irradiance values.
#' @param lux Numeric vector. Photopic illuminance values.
#' @param sampling_int Numeric. Sampling interval in seconds.
#' @param shiftModel Logical. Should the spectral shift model be used? Default
#'    is FALSE.
#'
#' @return Numeric vector with direct response values.
#'
#' @details  The timeseries is assumed to be regular. Missing values in the
#'    light data will be replaced by 0.
#'
#' @references Amundadottir, M.L. (2016). Light-driven model for identifying
#'    indicators of non-visual health potential in the built environment.
#'    [Doctoral dissertation, EPFL]. EPFL infoscience.
#'    \url{http://dx.doi.org/10.5075/epfl-thesis-7146}
#'
#' @export
#'
#' @examples
nvR_directResponse <- function(irr_mel,
                               lux,
                               sampling_int,
                               shiftModel = FALSE) {

  # Replace missing values
  if (any(is.na(irr_mel) | is.na(lux))) {
    warning("Light data contains missing values! They are replaced by 0.")
    irr_mel[is.na(irr_mel)] <- 0
    lux[is.na(lux)] <- 0
  }

  # Spectral sensitivity
  zeff <- 310 / 683 / 118.5 / 1.42 * 0.91 # from lux to melanopic eff. irr (F11)

  # Intensity response
  EI50 <- 106 # [lx]
  slope <- 3.55

  # Sampling interval in hours
  delta <- to.hours(sampling_int, "s")

  # Convert to effective irradiance
  Ieff <- nvR_getIeff(irr_mel, lux, delta, shiftModel)

  # Filter L2
  dFL2 <- round(2.3 / delta)

  # Filter L1
  dFL1 <- round(0.3 / delta)

  # Filter LH
  dFLH <- round(1.7 / delta)

  # MODEL
  u <- nvR_filterSMA(dFL1, Ieff)
  v <- nvR_adaptiveResponse(u, lux, EI50 * zeff, slope, dFLH, 0)
  RD <- nvR_filterEMA(dFL2, v)
  RD_p <- rep(0, length(RD))
  RD_p[Ieff > 0] <- RD[Ieff > 0] # return the same vector size as Ieff

  return(RD)
}

#' Non-visual circadian response
#'
#' This function calculates the non-visual circadian response
#'
#' @param irr_mel Numeric vector. Melanopic irradiance values.
#' @param lux Numeric vector. Photopic illuminance values.
#' @param sampling_int Numeric. Sampling interval in seconds.
#' @param datetime POSIXct vector. Datetime values. Will only be used if
#'    \code{sleep_onset} is specified. Default is NULL.
#' @param sleep_onset Character. Time of habitual sleep onset in the format
#'    "HH:MM:SS". Will only be used if \code{datetime} is specified. Default is
#'    NULL.
#' @param shiftModel Logical. Should the spectral shift model be used? Default
#'    is FALSE.
#'
#' @return Numeric vector with circadian response values.
#'
#' @details If \code{datetime} and \code{sleep_onset} are not specified, the
#'    start of the timeseries is assumed to be at habitual sleep onset. The
#'    timeseries is assumed to be regular. Missing values in the light data will
#'    be replaced by 0.
#'
#' @references Amundadottir, M.L. (2016). Light-driven model for identifying
#'    indicators of non-visual health potential in the built environment.
#'    [Doctoral dissertation, EPFL]. EPFL infoscience.
#'    \url{http://dx.doi.org/10.5075/epfl-thesis-7146}
#'
#' @export
#'
#' @examples
nvR_circadianResponse <- function(irr_mel,
                                  lux,
                                  sampling_int,
                                  datetime = NULL,
                                  sleep_onset = NULL,
                                  shiftModel = FALSE) {

  # Replace missing values
  if (any(is.na(irr_mel) | is.na(lux))) {
    warning("Light data contains missing values! They are replaced by 0.")
    irr_mel[is.na(irr_mel)] <- 0
    lux[is.na(lux)] <- 0
  }

  # Spectral sensitivity
  zeff <- 310 / 683 / 118.5 / 1.42 * 0.91 # from lux to melanopic eff. irr (F11)

  # Intensity response
  EI50 <- 119 # [lx]
  slope <- 1.42
  delta <- to.hours(sampling_int, "s")

  # Convert to effective irradiance
  Ieff <- nvR_getIeff(irr_mel, lux, delta, shiftModel)

  # Check input
  if (xor(!is.null(datetime), !is.null(sleep_onset))) {
    warning("Only one of datetime or sleep_onset specified. Will be ignored.")
  }

  # Align circadian sensitivity curve with sleep onset
  if (!is.null(datetime) & !is.null(sleep_onset)) {
    bt <- as.numeric(lubridate::hms(sleep_onset))
    dt <- as.numeric(datetime) - as.numeric(datetime)[1] +
      as.numeric(hms::as_hms(datetime[1]))

    index <- 1:length(dt)
    index_bt <- approx(dt, index, bt)$y %>% ceiling()
    if (is.na(index_bt)) index_bt <- 1

    post <- c(bt, dt[index_bt:length(dt)]) - bt
    pre <- dt[1:index_bt - 1] - bt
    t <- to.hours(c(pre, post), "s")
    if (length(t) != length(dt)) t <- t[-1]
  } else {
    # Check whether first three hours are darkness --> timeseries should start at
    # habitual sleep onset.
    if (mean(lux[1:(3 / delta)]) > 10) {
      warning("Average lux across the first three hours is higher than 10lx.
              Does the timeseries start at habitual sleep onset?")
    }
    t <- seq(0, (length(Ieff) * delta) - delta, delta)
  }

  # Get circadian modulation
  Rmax <- nvR_circadianModulator(t)

  # Filter L2
  dFL2 <- round(5.7 / delta)

  # Filter L1
  dFL1 <- round(0.6 / delta)

  # Filter LH
  dFLH <- round(3.7 / delta)

  # MODEL
  u <- nvR_filterSMA(dFL1, Ieff)
  v <- nvR_adaptiveResponse(u, lux, EI50 * zeff, slope, dFLH, 0) * Rmax
  RC <- nvR_filterEMA(dFL2, v)
  RC_p <- rep(0, length(RC))
  RC_p[Ieff > 0] <- RC[Ieff > 0]

  return(RC)
}

# Direct response metrics -------------------------------------------------

#' Performance metrics for direct response
#'
#' @param RD Time series of direct response (see \code{\link{directResponse}}.
#' @param sampling_int Numeric. Sampling interval in seconds.
#' @param unit_out Character. Time unit of output. Possible values are
#'    ("seconds", "minutes", "hours", "days"), which can be abbreviated.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Single column data frame or vector.
#'
#' @references Amundadottir, M.L. (2016). Light-driven model for identifying
#'    indicators of non-visual health potential in the built environment.
#'    [Doctoral dissertation, EPFL]. EPFL infoscience.
#'    \url{http://dx.doi.org/10.5075/epfl-thesis-7146}
#'
#' @name nvR_direct_metrics
NULL

#' @rdname nvR_direct_metrics
#'
#' @details `nvR_cumulativeResponse()` calculates the cumulative direct response.
#'
#' @export
#'
nvR_cumulativeResponse <- function(RD,
                                   sampling_int,
                                   unit_out = "hours",
                                   as_df = TRUE) {

  rd <- (sum(RD) * sampling_int) %>% from.secs(unit_out)

  if (as_df) {
    return(tibble::tibble(nvR_RD = rd))
  } else {
    return(rd)
  }
}

# Circadian response metrics ----------------------------------------------

#' Performance metrics for circadian response
#'
#' @param RC Time series of circadian response (see \code{\link{circadianResponse}}.
#'    Length must be 24h.
#' @param RC_ref Time series of circadian response (see \code{\link{circadianResponse}}
#'    for reference light exposure pattern. Length must be 24h.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return
#'
#' @references Amundadottir, M.L. (2016). Light-driven model for identifying
#'    indicators of non-visual health potential in the built environment.
#'    [Doctoral dissertation, EPFL]. EPFL infoscience.
#'    \url{http://dx.doi.org/10.5075/epfl-thesis-7146}
#'
#' @name nvR_circadian_metrics
NULL

#' @rdname nvR_circadian_metrics
#'
#' @details `nvR_circadianDisturbance()` calculates the circadian disturbance (CD).
#'
#' @export
#'
nvR_circadianDisturbance <- function(RC, RC_ref, as_df = TRUE) {
  if (length(RC) != length(RC_ref)) {
    stop("Periods must have same length!")
  }
  cd <- sum(abs(RC - RC_ref)) / sum(RC_ref > 0)

  if (as_df) {
    return(tibble::tibble(nvR_CD = cd))
  } else {
    return(cd)
  }
}

#' @rdname nvR_circadian_metrics
#'
#' @details `nvR_circadianBias()` calculates the circadian bias (CB).
#'
#' @export
#'
nvR_circadianBias <- function(RC, RC_ref, as_df = TRUE) {
  if (length(RC) != length(RC_ref)) {
    stop("Periods must have same length!")
  }
  cb <- sum(RC - RC_ref) / sum(RC_ref > 0)

  if (as_df) {
    return(tibble::tibble(nvR_CB = cb))
  } else {
    return(cb)
  }
}

#' @rdname nvR_circadian_metrics
#'
#' @details `nvR_relativeAmplitudeError()` calculates the relative amplitude error (RAE).
#'
#' @export
#'
nvR_relativeAmplitudeError <- function(RC, RC_ref, as_df = TRUE) {
  if (length(RC) != length(RC_ref)) {
    stop("Periods must have same length!")
  }
  rae <- max(RC) - max(RC_ref)

  if (as_df) {
    return(tibble::tibble(nvR_RAE = rae))
  } else {
    return(rae)
  }
}

# Helper Functions --------------------------------------------------------

# Adaptive response: adaptation of sensitivity due to prior light history
nvR_adaptiveResponse <- function(u, lux, sigma, n, dFLH, FLOOR) {
  H <- nvR_filterSMA(dFLH, logp1(lux))
  H[H < 0] <- 0
  if (FLOOR == 1) {
    H[H > 0] <- floor(H[H > 0])
  }
  sigma <- sigma * 2^(H - 1)
  R <- nvR_relativeResponse(u, sigma, n)
  return(R)
}

# Relative response: intensity-response curve based on melatonin suppression
nvR_relativeResponse <- function(u, sigma, n) {
  1 / (1 + (sigma / u)^n)
}

# Circadian modulator
nvR_circadianModulator <- function(t) {
  DAY <- 24
  phi_XcX <- -2.98 # rad - redefinition of CBTmin
  theta <- pi + phi_XcX / 2
  Cm <- (1 - 0.4 * cos(2 * pi / DAY * t + theta)) *
    (1 - 0.4 * -sin(2 * pi / DAY * t + theta))
  return(Cm)
}

# EMA filter
nvR_filterEMA <- function(d, I) {
  alpha <- 2 / (d + 1) # calculate smoothing factor "alpha"
  coefficient <- rep(1 - alpha, d)^(d:1) # note 1-alpha
  I <- c(rep(0, d - 1), I)
  R <- zoo::rollapplyr(I, d, function(x) sum(x * coefficient) / sum(coefficient))
  R[R < 0] <- 0
  return(R)
}

# SMA filter
nvR_filterSMA <- function(d, I) {
  I <- c(rep(0, d - 1), I)
  R <- zoo::rollapplyr(I, d, mean)
  R[R < 0] <- 0
  return(R)
}

# Get effective irradiance for non-visual responses
nvR_getIeff <- function(irr_mel, lux, delta, shiftModel) {
  A_mel <- 97.07
  A_v <- 118.5

  Ieff_mel <- (irr_mel / A_mel) * 310
  Ieff_v <- (lux / 683 / A_v) * 310

  if (shiftModel) {
    #   ON = which(I>0)
    #   Duration = length(I[ON[1]:length(I)]) * delta - delta
    #
    #   Ieff = rep(0,length(I))
    #   Ieff[ON[1]:length(I)] =
    #     I[ON[1]:length(I)] * RSE_e_v * exp(seq(0,Duration,delta) * a) +
    #     I[ON[1]:length(I)] * RSE_e_ipRGC * (1-exp(seq(0,Duration,delta) * a))
    Ieff <- Ieff_mel
  } else {
    Ieff <- Ieff_mel
  }

  return(Ieff)
}
