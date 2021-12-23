#' Convert time between units.
#'
#' Convenience functions for converting between time units.
#'
#' @param t Numeric time value.
#' @param unit Time unit as string. Possible values are ("seconds","minutes",
#' "hours","days"). Units can be abbreviated.
#'
#' @return
#'
#' @name time_conversion
NULL


#' @rdname time_conversion
#'
#' @details `to.secs()` converts time `t` with unit `unit` to seconds.
#'
#' @export
#'
to.secs = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit,
         "m" = t * 60,
         "h" = t * 3600,
         "d" = t * 86400,
         stop("Invalid unit. Possible values are ['m', 'h', 'd']!"))
}

#' @rdname time_conversion
#'
#' @details `to.mins()` converts time `t` with unit `unit` to minutes.
#'
#' @export
#'
to.mins = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit,
         "s" = t / 60,
         "h" = t * 60,
         "d" = t * 1440,
         stop("Invalid unit. Possible values are ['s', 'h', 'd']!"))
}

#' @rdname time_conversion
#'
#' @details `to.hours()` converts time `t` with unit `unit` to hours.
#'
#' @export
#'
to.hours = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,
          "s" = t / 3600,
          "m" = t / 60,
          "d" = t * 24,
          stop("Invalid unit. Possible values are ['s', 'm', 'd']!"))
}

#' @rdname time_conversion
#'
#' @details `to.days()` converts time `t` with unit `unit` to days.
#'
#' @export
#'
to.days = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,
          "s" = t / 86400,
          "m" = t / 1440,
          "h" = t / 24,
          stop("Invalid unit. Possible values are ['s', 'm', 'h']!"))
}

#' @rdname time_conversion
#'
#' @details `from.secs()` converts time `t` in seconds to unit `unit`.
#'
#' @export
#'
from.secs = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit,
         "m" = t / 60,
         "h" = t / 3600,
         "d" = t / 86400,
         stop("Invalid unit. Possible values are ['m', 'h', 'd']!"))
}

#' @rdname time_conversion
#'
#' @details `from.mins()` converts time `t` in minutes to unit `unit`.
#'
#' @export
#'
from.mins = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit,
         "s" = t * 60,
         "h" = t / 60,
         "d" = t / 1440,
         stop("Invalid unit. Possible values are ['s', 'h', 'd']!"))
}

#' @rdname time_conversion
#'
#' @details `from.hours()` converts time `t` in hours to unit `unit`.
#'
#' @export
#'
from.hours = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,
          "s" = t * 3600,
          "m" = t * 60,
          "d" = t / 24,
          stop("Invalid unit. Possible values are ['s', 'm', 'd']!"))
}

#' @rdname time_conversion
#'
#' @details `from.days()` converts time `t` in days to unit `unit`.
#'
#' @export
#'
from.days = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,
          "s" = t * 86400,
          "m" = t * 1440,
          "h" = t * 24,
          stop("Invalid unit. Possible values are ['s', 'm', 'h']!"))
}
