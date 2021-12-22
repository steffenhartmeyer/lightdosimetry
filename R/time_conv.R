#' Convert time to/from other unit.
#'
#' Convenience functions for converting between time units.
#'
#' @name time_conv
#' @param t Numeric time value.
#' @param unit Character time unit. Possible values are ("secs", "mins", "hours",
#'    "days"). Can be abbreviated.
#'
#' @return
#' @export
#'
#' @examples
NULL
#> NULL

#' @rdname time_conv
to.secs = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit,"m" = t * 60,"h" = t * 3600,"d" = t * 86400)
}

#' @rdname time_conv
from.secs = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit, "m" = t / 60, "h" = t / 3600, "d" = t / 86400)
}

#' @rdname time_conv
to.mins = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit,"s" = t / 60,"h" = t * 60,"d" = t * 1440)
}

#' @rdname time_conv
from.mins = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit, "s" = t * 60, "h" = t / 60, "d" = t / 1440)
}

#' @rdname time_conv
to.hours = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,"s" = t / 3600,"m" = t / 60,"d" = t * 24)
}

#' @rdname time_conv
from.hours = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,"s" = t * 3600,"m" = t * 60,"d" = t / 24)
}

#' @rdname time_conv
to.days = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,"s" = t / 86400,"m" = t / 1440,"h" = t / 24)
}

#' @rdname time_conv
from.days = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,"s" = t * 86400,"m" = t * 1440,"h" = t * 24)
}
