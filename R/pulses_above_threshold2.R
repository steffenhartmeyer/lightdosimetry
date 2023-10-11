#' Pulses above threshold
#'
#' This function clusters the light data into continuous clusters (pulses) of
#' light above/below a given threshold. Clustering may be fine-tuned by setting
#' the minimum length of the clusters and by allowing brief interruptions to be
#' included in a single cluster, with a specified maximum length of interruption
#' episodes and proportion of total amount of interruptions to light above
#' threshold.
#'
#' @param lightVar Numeric vector containing the light data. Missing values are
#'    replaced by 0.
#' @param timeVar Vector containing the time data. Can be POSIXct or numeric.
#'    Must be regularly spaced.
#' @param threshold Single numeric value or vector specifying threshold
#'    intensities. The sign indicates above/below (see \code{\link{threshold}}).
#' @param min_length Minimum length of each cluster. Can be numeric or string
#'    in the format "[numeric] [unit]", with possible units ("seconds","minutes",
#'    "hours","days"). Units can be abbreviated, see
#'    \code{\link{parse_timeunit_tosecs}}. Defaults to 0.
#' @param max_interrupt Maximum length of each episode of interruptions. Can be
#'    numeric or string in the format "[numeric] [unit]", with possible units
#'    ("seconds","minutes","hours","days"). Units can be abbreviated, see
#'    \code{\link{parse_timeunit_tosecs}}. Defaults to 0.
#' @param prop_interrupt Single numeric value between [0, 1] specifying the
#'    maximum proportion of the total number of interruptions to light above
#'    threshold. Defaults to 0.
#' @param unit_pulse_length Character. Time unit of pulse length metric.
#'    Possible values are ("seconds", "minutes", "hours", "days"), which can be
#'    abbreviated. Defaults to "minutes".
#' @param return_indices Logical. Should the cluster indices be returned? Defaults
#'    to FALSE.
#' @param loop Logical. Should the data be looped? Defaults to FALSE.
#' @param as_df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#' @param wide Logical. Should the output be returned in wide format? Defaults to
#'    TRUE.
#'
#' @return Data frame or matrix with pairs of threshold and calculated values.
#'    If `wide` is TRUE then variable names will be concatenated with the
#'    threshold. If `return_indices` is TRUE then a list with the data frame
#'    and a list of cluster indices will be returned.
#' @export
#'
#' @references Wilson, J., Reid, K. J., Braun, R. I., Abbott, S. M., & Zee, P. C.
#'    (2018). Habitual light exposure relative to circadian timing in delayed
#'    sleep-wake phase disorder. \emph{Sleep}, 41(11).
#'    \url{https://doi.org/10.1093/sleep/zsy166}
#'
#' @examples
#'
pulses_above_threshold2 <- function(
    lightVar,
    timeVar,
    threshold,
    min_length = "10 mins",
    max_interrupt = "2 mins",
    prop_interrupt = 0.25,
    unit_pulse_length = "mins",
    return_indices = FALSE,
    loop = FALSE,
    as_df = TRUE,
    wide = TRUE) {

  # Detect epoch
  epoch = abs(diff(as.numeric(timeVar)))
  if(length(unique(epoch)) > 1){
    warning("Data not regularly spaced. Selecting shortest epoch.")
    epoch = sort(epoch)
  }
  epoch = epoch[1]

  # Parse time units
  min_length = parse_timeunit_tosecs(min_length)$secs
  max_interrupt = parse_timeunit_tosecs(max_interrupt)$secs

  # Check whether parameters are longer than epoch
  if(any(c(min_length, max_interrupt) < epoch)){
    stop("Time parameters must be equal to or longer than the epoch.")
  }

  # Convert to sample counts
  min_length = round(min_length / epoch)
  max_interrupt = round(max_interrupt / epoch)

  # Loop data
  if (loop) {
    lightVar <- c(lightVar, lightVar)
    span <- timeVar[length(timeVar)] - timeVar[1]
    timeVar <- c(timeVar, timeVar + span + epoch)
  }

  # Pre-allocate containers
  df <- tibble::tibble(
    threshold = numeric(),
    number_pulses = numeric(),
    mean_pulse_intensity = numeric(),
    geomean_pulse_intensity = numeric(),
    mean_pulse_length = numeric(),
    total_pulse_time = numeric(),
    mean_pulse_midpoint = numeric(),
    mean_pulse_onset = numeric(),
    mean_pulse_offset = numeric()
  )
  index_list <- list()

  data = tibble(idx = 1:length(lightVar), light = lightVar, time = timeVar)

  data.clusters = list()
  for (i in 1:length(threshold)) {
    c = threshold[i]
    abovethreshold = lightVar > c
    clusters = find_clusters2(abovethreshold, min_length, max_interrupt, prop_interrupt)

    data.clusters[[i]]  = data %>%
      left_join(clusters, by = "idx") %>%
      filter(is_cluster) %>%
      add_column(threshold = c)
  }
  data.clusters = bind_rows(data.clusters)
  options(dplyr.summarise.inform = FALSE)

  df = data.clusters %>%
    group_by(threshold, cluster_idx) %>%
    summarise(
      mean = mean(light, na.rm = TRUE),
      geomean = geomean(light, na.rm = TRUE),
      length = length(light)*epoch,
      on = first(time),
      off = last(time),
      mid = mean(time)
    ) %>%
    group_by(threshold) %>%
    summarise(
      number_pulses = length(mean),
      mean_pulse_intensity = mean(mean, na.rm = TRUE),
      geomean_pulse_intensity = mean(geomean, na.rm = TRUE),
      mean_pulse_length = from.secs(mean(length, na.rm = TRUE), unit_pulse_length),
      total_pulse_time = mean_pulse_length*number_pulses,
      mean_pulse_onset = mean(on, na.rm = TRUE),
      mean_pulse_midpoint = mean(off, na.rm = TRUE),
      mean_pulse_offset = mean(mid, na.rm = TRUE),
    )

  # Convert to POSIXct
  if (lubridate::is.POSIXct(timeVar)) {
    df <- df %>%
      dplyr::mutate_at(
        dplyr::vars(mean_pulse_midpoint:mean_pulse_offset), round
      ) %>%
      dplyr::mutate_at(
        dplyr::vars(mean_pulse_midpoint:mean_pulse_offset),
        lubridate::as_datetime,
        tz = lubridate::tz(timeVar)
      )
  }
  # Reshape to wide format
  if (wide) {
    df <- df %>%
      tidyr::pivot_wider(
        names_from = threshold,
        values_from = names(df)[-1],
        names_sep = "."
      )
  }

  # Return
  if (!as_df) df <- as.numeric(df)
  if (return_indices) {
    return(list(metrics = df, indices = index_list))
  } else {
    return(df)
  }
}
