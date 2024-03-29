% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_conversion.R
\name{time_conversion}
\alias{time_conversion}
\alias{to.secs}
\alias{to.mins}
\alias{to.hours}
\alias{to.days}
\alias{from.secs}
\alias{from.mins}
\alias{from.hours}
\alias{from.days}
\title{Convert time between units.}
\usage{
to.secs(t, unit)

to.mins(t, unit)

to.hours(t, unit)

to.days(t, unit)

from.secs(t, unit)

from.mins(t, unit)

from.hours(t, unit)

from.days(t, unit)
}
\arguments{
\item{t}{Numeric time value.}

\item{unit}{Time unit as string. Possible values are ("seconds","minutes",
"hours","days"). Units can be abbreviated.}
}
\value{

}
\description{
Convenience functions for converting between time units.
}
\details{
`to.secs()` converts time `t` with unit `unit` to seconds.

`to.mins()` converts time `t` with unit `unit` to minutes.

`to.hours()` converts time `t` with unit `unit` to hours.

`to.days()` converts time `t` with unit `unit` to days.

`from.secs()` converts time `t` in seconds to unit `unit`.

`from.mins()` converts time `t` in minutes to unit `unit`.

`from.hours()` converts time `t` in hours to unit `unit`.

`from.days()` converts time `t` in days to unit `unit`.
}
