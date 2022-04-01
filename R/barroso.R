#' Metrics from Barroso et al.
#'
#' @param lightVar Numeric vector containing the light data.
#' @param sampling_int Numeric. Sampling interval in seconds.
#' @param unit_out_clength String. Time unit of the bright/dark cluster length.
#'    Possible values are ("seconds", "minutes", "hours", "days"), which can be $
#'    abbreviated.
#' @param loop Logical. Should the data be looped? Defaults to FALSE.
#' @param na_rm Logical. Should missing values be removed? Defaults to TRUE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return Data frame or numeric.
#' @export
#'
#' @examples
barroso <- function(lightVar,
                    sampling_int,
                    unit_out_clength,
                    loop = FALSE,
                    na_rm = TRUE,
                    as_df = TRUE) {

  # Bright/dark thresholds
  tB <- threshold_at_timespan(lightVar, "6 h",
    sampling_int = sampling_int,
    above = TRUE, as_df = FALSE
  )[1]

  tD <- threshold_at_timespan(lightVar, "8 h",
    sampling_int = sampling_int,
    above = FALSE, as_df = FALSE
  )[1]

  # Bright/dark mean level
  mB <- mean(lightVar[lightVar >= tB], trim = 0.2, na.rm = na_rm)
  mD <- mean(lightVar[lightVar <= tD], trim = 0.2, na.rm = na_rm)

  # Bright/dark cluster
  cB <- cat_max(lightVar, tB,
    sampling_int = sampling_int,
    unit_out = unit_out, loop = loop, as_df = FALSE
  )[1]
  cD <- cat_max(lightVar, -1 * (tD + 0.01),
    sampling_int = sampling_int,
    unit_out = unit_out, loop = loop, as_df = FALSE
  )[1]

  # Circadian variation
  civ <- coeff_var(lightVar, na.rm = na_rm, as_df = FALSE)

  # Store in dataframe
  df <- tibble::tibble(
    bright_threshold = tB,
    dark_threshold = tD,
    bright_mean_level = mB,
    dark_mean_level = mD,
    bright_cluster = cB,
    dark_cluster = cD,
    circadian_variation = civ
  )

  # Return as data frame or numeric
  if (as_df) {
    return(df)
  } else {
    return(as.numeric(df))
  }
}
