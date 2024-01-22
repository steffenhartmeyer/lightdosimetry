# DEPRECATED

This repository is no longer maintained as the package development has been merged with 
the development of the LightLogR package, which can be found here: https://github.com/tscnlab/LightLogR

All functions included in this package will be available in the LightLogR package, which enables
import, processing and analysis pipelines for light-dosimetry data. Please check it out!
.
.
.
.
.
.
.
.
.
.
.

## Installation

You can install the released version of the lightdosimetry package using:

``` r
remotes::install_github("steffenhartmeyer/lightdosimetry")
```

## Example

This is a basic example of how the functions in this package can be used:

``` r
library(lightdosimetry)

# Subset of data where subject is awake
example_light.awake = example_light %>% dplyr::filter(awake)

# Duration metrics
duration =
  example_light.awake %>%
  dplyr::summarise(
    tat(light, 1000, sampling_int = 60, unit_out = "mins"),
    tatr(light, 100, 1000, sampling_int = 60, unit_out = "mins")
  )

# Timing metrics
timing =
  example_light.awake %>%
  dplyr::summarise(
    midpointCE(logp1(light),datetime),
    centroidLE(light, datetime, bin_size = "1 hour"),
    mlit(light, datetime, 1000),
    flit(light, datetime, 500),
    llit(light, datetime, 500),
    flit_angle(light, datetime, 500),
    llit_angle(light, datetime, 500),
  )

# Timing metrics (24h data)
timing.24h =
  example_light %>%
  dplyr::summarise(
    bright_dark_period(light, datetime, "bright", "10 h", sampling_int = 60),
    bright_dark_period(light, datetime, "dark", "5 h", sampling_int = 60, loop=TRUE),
  )

# Temporal dynamics metrics
pattern =
  example_light.awake %>%
  dplyr::summarise(
    pulses_above_threshold(light, datetime, 1000, min_length = "10 min",
                           max_interrupt = "2 min", prop_interrupt = 0.25),
    fic(light, 1000),
    disparity_index(light),
    lqi(light)
  )

# Temporal dynamics metrics (24h data)
pattern.24h =
  example_light.awake %>%
  dplyr::summarise(
    interdaily_stability(logp1(light), datetime),
    intradaily_variability(logp1(light), datetime),
  )


# Prior exposure history metrics
history =
  example_light.awake %>%
  dplyr::summarise(
    cumulative_exposure(light),
    dose_tatr(light, 100, 500, sampling_int = 60, unit_out = "mins")
  )
  
# Exponential smoothing
history.ema =
  example_light.awake %>%
  dplyr::mutate(
    ema = ema(light, "90 mins", sampling_int = 60)
  )

#Metrics from Barrosso et al.
barrosso =
  example_light %>%
  dplyr::summarise(
    barroso(light, sampling_int = 60, unit_out_clength = "mins", loop = TRUE)
  )
```
