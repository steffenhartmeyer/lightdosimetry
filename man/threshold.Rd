% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/threshold.R
\name{threshold}
\alias{threshold}
\title{Light above or below threshold?}
\usage{
threshold(lightVar, threshold)
}
\arguments{
\item{lightVar}{Numeric value or vector of light data.}

\item{threshold}{Single numeric value specifying threshold
intensity. The sign indicates above/below (negative values = below,
positive value = above). This operation is always including the threshold
value.}
}
\value{
Logical vector.
}
\description{
Convenience function to determine whether light is above or below threshold.
}
