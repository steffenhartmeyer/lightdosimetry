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
#'    Should be a vector of regularly spaced timestamps, otherwise the calculations
#'    may be misleading.
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
#' @param sampling_int Numeric. Sampling interval in seconds. Defaults to 60.
#' @param unit_pulse_length Character. Time unit of pulse length metric.
#'    Possible values are ("secs", "mins", "hours", "days"). Can be abbreviated.
#'    Defaults to "mins".
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
pulses_above_threshold <- function(lightVar,
                                   timeVar,
                                   threshold,
                                   min_length = 0,
                                   max_interrupt = 0,
                                   prop_interrupt = 0,
                                   sampling_int = 60,
                                   unit_pulse_length = "mins",
                                   return_indices = FALSE,
                                   loop = FALSE,
                                   as_df = TRUE,
                                   wide = TRUE) {

  # Check whether time series is regularly spaced
  if (length(unique(diff(timeVar))) > 1) {
    warning("Time variable is not regularly spaced. Calculated results may be
            incorrect!")
  }

  # Loop data
  if (loop) {
    lightVar <- c(lightVar, lightVar)
    span <- timeVar[length(timeVar)] - timeVar[1]
    timeVar <- c(timeVar, timeVar + span + sampling_int)
  }

  # Parse minimum cluster length
  if (is.numeric(min_length)) {
    minlen <- min_length
  } else {
    minlen <- parse.timeunit.tosecs(min_length)$secs / sampling_int
  }

  # Parse maximum number of interruptions
  if (is.numeric(max_interrupt)) {
    maxint <- max_interrupt
  } else {
    maxint <- parse.timeunit.tosecs(max_interrupt)$secs / sampling_int
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

  for (c in threshold) {
    clustering <- FALSE # Flag indicating whether to consider values for cluster
    count.int <- 0 # Counter of interruptions
    temp.clusters <- list() # Temporary list of uninterrupted clusters
    current.cluster <- c(NULL) # Current uninterrupted cluster
    temp.int <- c(NULL) # Temporary list of interruptions
    clusters.index <- list() # Cluster list with indices
    clusters.light <- list() # Cluster list with light values
    clusters.time <- list() # Cluster list with datetime values

    for (idx in c(1:length(lightVar))) {
      if (threshold(lightVar[idx], c)) {
        # Light above threshold
        # --> Add to current cluster and set flag and interruption counter
        current.cluster <- append(current.cluster, idx)
        clustering <- TRUE
        count.int <- 0
      } else {
        # Light below threshold
        if (clustering) {
          if (count.int == 0) {
            # No interruptions counted yet
            # --> Add current cluster to temporary list and reset current cluster
            temp.clusters <- append(temp.clusters, list(current.cluster))
            current.cluster <- c(NULL)
          }

          # Increase interruption counter and add to interruption list
          # if maximum number of interruptions not yet reached
          count.int <- count.int + 1
          if (count.int <= maxint) {
            temp.int <- append(temp.int, idx)
          } else {
            # Max interruption length reached
            # --> Remove last interruptions from interruption list
            temp.int <- temp.int[1:(length(temp.int) - maxint)]

            if (1 - (length(temp.int) / length(unlist(temp.clusters))) <
              prop_interrupt) {
              # Proportion of interruptions lower than maximum proportion
              # --> Merge interruptions and temp clusters into one cluster
              cluster <- c(unlist(temp.clusters), temp.int) %>% sort()
              temp.light <- list(lightVar[cluster])
              temp.time <- list(as.numeric(timeVar[cluster]))
              temp.clusters <- list(cluster)
            } else {
              # Proportion of interruptions higher than maximum proportion
              # --> Extract clusters that satisfy minimum length argument
              temp.clusters <- temp.clusters[lengths(temp.clusters) >= minlen]
              temp.light <- list()
              temp.time <- list()
              for (cluster in temp.clusters) {
                temp.light <- append(temp.light, list(lightVar[cluster]))
                temp.time <- append(temp.time, list(as.numeric(timeVar[cluster])))
              }
            }
            # Add clusters to final cluster lists
            clusters.light <- append(clusters.light, temp.light)
            clusters.time <- append(clusters.time, temp.time)
            if (return_indices) clusters.index <- append(index, temp.clusters)

            # Reset everything
            clustering <- FALSE
            current.cluster <- c(NULL)
            count.int <- 0
            temp.int <- c(NULL)
            temp.clusters <- list()
          }
        }
      }
    }
    # Finished with clustering
    # --> Calculate metrics and add to data frame
    p_n <- length(clusters.light)
    p_mean <- unlist(lapply(clusters.light, mean)) %>% mean(na.rm = TRUE)
    p_geomean <- unlist(lapply(clusters.light, geomean)) %>% mean(na.rm = TRUE)
    p_len <- (unlist(lapply(clusters.light, length)) * sampling_int) %>%
      from.secs(unit_pulse_length) %>%
      mean()
    p_time <- p_n * p_len
    p_on <- unlist(lapply(clusters.time, first)) %>% mean(na.rm = TRUE)
    p_off <- unlist(lapply(clusters.time, last)) %>% mean(na.rm = TRUE)
    p_mid <- unlist(lapply(clusters.time, mean)) %>% mean(na.rm = TRUE)
    df <- df %>%
      tibble::add_row(
        threshold = c,
        number_pulses = p_n,
        mean_pulse_intensity = p_mean,
        geomean_pulse_intensity = p_geomean,
        mean_pulse_length = p_len,
        total_pulse_time = p_time,
        mean_pulse_onset = p_on,
        mean_pulse_midpoint = p_mid,
        mean_pulse_offset = p_off
      )
    # Add indices to index list
    if (return_indices) index_list <- append(index_list, list(index))
  }
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
