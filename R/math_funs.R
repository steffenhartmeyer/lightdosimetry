#' Mathematical helper functions
#'
#' @param x Numeric vector.
#' @param trim The fraction (0 to 0.5) of observations to be trimmed from each
#'    end of x before the mean is computed. Values of trim outside that range
#'    are taken as the nearest endpoint. See \code{\link[base]{mean}}
#' @param na.rm Logical. Should missing values be removed?
#' @param base A positive or complex number: the base with respect to which
#'    logarithms are computed. Defaults to `exp(1)`.
#'
#' @return
#'
#' @name math_funs
#'
NULL

#' @rdname math_funs
#'
#' @details `geomean` calculates the geometric mean.
#'
#' @export
#'
geomean <- function(x, trim = 0, na.rm = FALSE, base = exp(1)) {
  x[x == 0] <- 1
  exp(mean(log(x, base = exp(1)), trim = trim, na.rm = na.rm))
}

#' @rdname math_funs
#'
#' @details `geosd` calculates the geometric standard deviation
#'
#' @export
#'
geosd <- function(x, na.rm = FALSE, base = exp(1)) {
  x[x == 0] <- 1
  exp(sd(log(x, base = exp(1)), na.rm = na.rm))
}

#' @rdname math_funs
#'
#' @details `se` calculates the standard error
#'
#' @export
#'
se <- function(x, na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x) / length(x))
}

#' @rdname math_funs
#'
#' @details `logp1` calculates log(x + 1) for any base. The default base is 10.
#'
#' @export
#'
logp1 <- function(x, base = 10) {
  log(x + 1, base)
}
