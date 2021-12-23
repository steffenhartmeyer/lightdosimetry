#' Parse time and unit and convert to seconds
#'
#' This is a convenience function for parsing a time together with its unit given
#' as a string and converting it to seconds.
#'
#' @param input String to be parsed. Must be in the format "[numeric] [unit]",
#'    with possible units ("seconds","minutes","hours","days"). Units can be
#'    abbreviated.
#'
#' @return List with converted time in seconds, the input time and unit as strings.
#' @export
#'
#' @examples
#' parse_timeunit_tosecs("30 minutes")
#' parse_timeunit_tosecs("0.5 hrs")
#' parse_timeunit_tosecs("1 d")
#' parse_timeunit_tosecs("500 secs")
#' \dontrun{
#' parse_timeunit_tosecs("30min")
#' }
parse_timeunit_tosecs <- function(input) {
  if (!stringr::str_detect(input, "\\d+ (s|m|h|d)")) {
    stop("Wrong time unit specification! Must be '[numeric] ['s','m','h','d']'.")
  }
  parsed <- stringr::str_split(input, " ")[[1]]
  list(
    secs = to.secs(as.numeric(parsed[1]), parsed[2]),
    time = parsed[1],
    unit = parsed[2]
  )
}
