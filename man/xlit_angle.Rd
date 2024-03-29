% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlit_angle.R
\name{xlit_angle}
\alias{xlit_angle}
\alias{flit_angle}
\alias{llit_angle}
\title{Phase angle of first/last timing above/below threshold.}
\usage{
flit_angle(
  lightVar,
  timeVar,
  threshold,
  unit_out = "mins",
  as_df = TRUE,
  wide = TRUE
)

llit_angle(
  lightVar,
  timeVar,
  threshold,
  unit_out = "mins",
  as_df = TRUE,
  wide = TRUE
)
}
\arguments{
\item{lightVar}{Numeric vector containing the light data.}

\item{timeVar}{Vector containing the time data. Can be POSIXct or numeric.}

\item{threshold}{Single numeric value or vector specifying threshold
intensities. The sign indicates above/below (see \code{\link{threshold}}).}

\item{unit_out}{String indicating the time unit of the output value.
Possible values are ("seconds","minutes","hours","days"). Units can be
abbreviated. Is only used if `timeVar` is POSIXct, otherwise no conversion
will be performed. Defaults to "minutes".}

\item{as_df}{Logical. Should the output be returned as a data frame? Defaults
to TRUE.}

\item{wide}{Logical. Should the output be returned in wide format? Defaults to
TRUE.}
}
\value{
Data frame or matrix with pairs of threshold and calculated values.
   If `wide` is TRUE then variable names will be concatenated with the threshold.
}
\description{
These functions calculate the phase angle (i.e., timespan) between the first/last
timepoint where light levels are above or below a given threshold intensity
and the start/end of the given interval.
}
\details{
The phase angle is relative to the start/end of the given time series.
   Therefore, a time interval of interest should be provided (e.g.,
   the light exposure between waking up and going to bed).

`flit_angle()` calculates the phase angle between the first timing of
   light above/below threshold and the start of the given interval.

`llit_angle()` calculates the phase angle between the last timing of
   light above/below threshold and the end of the given interval.
}
