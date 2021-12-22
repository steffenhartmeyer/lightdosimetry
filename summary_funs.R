## ------------------------------------------------------------------------
##
## Script name: Light exposure quantification metrics 
##
## Purpose of script: Collection of functions to transform, subset, 
## and summarize time-series light exposure data
##
## Author: Steffen Hartmeyer
##
## Date Created: 19-Nov-2020
## Last Updated: 16-Nov-2021
##
## ------------------------------------------------------------------------
##
## List of functions:
##
##
## ------------------------------------------------------------------------

require(glue)
require(tidyverse)
require(lubridate)
require(rlang)
require(zoo)
require(hms)
require(circacompare)

source("./Functions/helper_funs.R")


# Intensity ---------------------------------------------------------------

trimmed_mean = function(lightVar, trim, na.rm = TRUE){
  if(na.rm) lightVar = na.omit(lightVar)
  sorted = sort(lightVar)
  trim_window = floor(trim * length(sorted))
  mean(sorted[trim_window+1:length(sorted)-trim_window])
}

# Duration ----------------------------------------------------------------

#Time above/below threshold
tat = function(lightVar, threshold, sampling_int = 60, unit_out = "mins", na.rm=TRUE, 
               as_df = TRUE, wide=TRUE){
  
  df = tibble(threshold=numeric(), tat=numeric()) 
  for(c in threshold){
    val = (sum(threshold(lightVar, c), na.rm=na.rm) * sampling_int) %>% from.secs(unit_out) 
    df = df %>% add_row(threshold=c, tat=val)
  }
  
  if(wide){
    df = df %>% pivot_wider(names_from=threshold, values_from=tat)
    if(ncol(df)==1) names(df) = paste0("tat.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

tat_sn = function(lightVar, solarVar, threshold, sampling_int = 60, unit_out = "mins", na.rm=TRUE, 
                  as_df = TRUE, wide=TRUE){
  
  df = tibble(threshold=numeric(), tat_sn=numeric()) 
  for(c in threshold){
    tat = (sum(threshold(lightVar, c), na.rm=na.rm) * sampling_int) %>% from.secs(unit_out) 
    solar = (sum(solarVar > 0, na.rm=na.rm) * sampling_int) %>% from.secs(unit_out) 
    tat_sn = (tat/solar*100) %>% round(2)
    df = df %>% add_row(threshold=c, tat_sn = tat_sn)
  }
  
  if(wide){
    df = df %>% pivot_wider(names_from=threshold, values_from=tat_sn)
    if(ncol(df)==1) names(df) = paste0("tat_sn.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

#Time within threshold range
tatr = function(lightVar, lower, upper, sampling_int = 60, unit_out = "mins", na.rm=TRUE,
                as_df = TRUE, wide=TRUE){
  if(length(lower) != length(upper)) stop("lower and upper bounds must be same length")
  
  df = tibble(threshold_min=numeric(), threshold_max=numeric(), tat=numeric()) 
  for(i in 1:length(lower)){
    cmin = lower[i]
    cmax = upper[i]
    val = (sum(between(lightVar, cmin, cmax), na.rm=na.rm) * sampling_int) %>% 
      from.secs(unit_out)
    df = df %>% add_row(threshold_min=cmin, threshold_max=cmax, tat=val)
  }
  
  if(wide){
    df = df %>% unite(threshold, threshold_min, threshold_max) %>%
      pivot_wider(names_from=threshold, values_from=tat)
    if(ncol(df)==1) names(df) = paste0("tat.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

# Timing ------------------------------------------------------------------

#Midpoint of CE
midpointCE = function(lightVar, dtVar, as_df = TRUE){
  lightVar[is.na(lightVar)] = 0
  cumsum = cumsum(lightVar)
  halfSum = cumsum[length(cumsum)]/2
  midpoint = which.min(abs(cumsum-halfSum))
  
  if(as_df) return(tibble(midpointCE = dtVar[midpoint]))
  else return(dtVar[midpoint])
}

#Centroid of LE
centroidLE = function(lightVar, dtVar, na.rm = TRUE, as_df = TRUE){
  df = tibble(light = lightVar, datetime = dtVar) %>%  
    group_by(datetime_hour = floor_date(datetime, "hour")) %>%
    summarise(light = mean(light, na.rm = na.rm))
  weights = (df$light/sum(df$light))
  val = sum(as.numeric(df$datetime_hour)*weights) 
  
  if("POSIXct" %in% class(dtVar)) val = val %>% round() %>% as_datetime(tz = tz(dtVar))
  
  if(as_df) return(tibble(centroidLE = val))
  else return(val)
}

#Mean light timing above threshold
mlit = function(lightVar, dtVar, threshold, na.rm=FALSE, as_df=TRUE, wide=TRUE){
  df = tibble(threshold=numeric(), mlit=numeric()) 
  for(c in threshold){
    val = mean(as.numeric(dtVar[threshold(lightVar,c)]), na.rm=na.rm)
    df = df %>% add_row(threshold=c, mlit=val)
  }
  if("POSIXct" %in% class(dtVar)) 
    df = df %>% mutate_at(vars(mlit),round) %>% 
      mutate_at(vars(mlit), as_datetime, tz=tz(dtVar))
  
  if(wide){
    df = df %>% pivot_wider(names_from=threshold, values_from=mlit)
    if(ncol(df)==1) names(df) = paste0("mlit.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

#First clock time above threshold
flit = function(lightVar, dtVar, threshold, as_df = TRUE, wide=TRUE){
  df = tibble(threshold=numeric(), flit=numeric()) 
  for(c in threshold){
    val = dtVar[threshold(lightVar,c)][1] %>% as.numeric()
    df = df %>% add_row(threshold=c, flit=val)
  }
  if("POSIXct" %in% class(dtVar)) 
    df = df %>% mutate_at(vars(flit), as_datetime, tz=tz(dtVar))
  
  if(wide){
    df = df %>% pivot_wider(names_from=threshold, values_from=flit)
    if(ncol(df)==1) names(df) = paste0("flit.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

#Last clock time above threshold
llit = function(lightVar, dtVar, threshold, as_df = TRUE, wide=TRUE){
  df = tibble(threshold=numeric(), llit=numeric())
  for(c in threshold){
    val = dtVar[threshold(lightVar,c)] %>% last() %>% as.numeric()
    df = df %>% add_row(threshold=c, llit=val)
  }
  if("POSIXct" %in% class(dtVar)) 
    df = df %>% mutate_at(vars(llit), as_datetime, tz=tz(dtVar))
  
  if(wide){
    df = df %>% pivot_wider(names_from=threshold, values_from=llit)
    if(ncol(df)==1) names(df) = paste0("llit.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

#Phase angle between FLIT and start of interval
flit_angle = function(lightVar, dtVar, threshold, unit_out="mins", as_df=TRUE, wide=TRUE){
  df = tibble(threshold=numeric(), flit_angle=numeric())
  for(c in threshold){
    flit = dtVar[threshold(lightVar,c)][1]
    val = (flit - dtVar[1]) %>% as.numeric(units = unit_out)
    df = df %>% add_row(threshold=c, flit_angle=val)
  }
  if(wide){
    df = df %>% pivot_wider(names_from=threshold, values_from=flit_angle)
    if(ncol(df)==1) names(df) = paste0("flit_angle.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
} 

#Phase angle between LLIT and end of interval
llit_angle = function(lightVar, dtVar, threshold, unit_out="mins", as_df = TRUE, 
                      wide=TRUE){
  df = tibble(threshold=numeric(), llit_angle=numeric()) 
  for(c in threshold){
    light = {{lightVar}}
    llit = dtVar[threshold(light,c)] %>% last()
    val = (dtVar[length(dtVar)] - llit) %>% as.numeric(units = unit_out)
    df = df %>% add_row(threshold=c, llit_angle=val)
  }
  if(wide){
    df = df %>% pivot_wider(names_from=threshold, values_from=llit_angle)
    if(ncol(df)==1) names(df) = paste0("llit_angle.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

#Maximum/minimum continuous 
max_min_period = function(lightVar, dtVar, timespan, max=TRUE, sample_int=60, 
                          loop=FALSE, na.rm=TRUE, as_df=TRUE, wide=TRUE){
  if(loop){
    #Loop data
    lightVar = c(lightVar, lightVar)
    dt = as.numeric(dtVar)
    dt = c(dt[1:length(dt)-1], dt + (dt[length(dt)]-dt[1]))
    if("POSIXct" %in% class(dtVar)) dtVar = dt %>% as_datetime(tz = tz(dtVar))
    else dtVar = dt
  }
  
  df = tibble(timespan=numeric(), mean=numeric(), midpoint=numeric(),
              onset=numeric(), offset=numeric())
  for(ts in timespan){
    parsed_ts = parse.timeunit.tosecs(ts)
    window = parsed_ts$secs/sample_int %>% floor()
    if(window %% 2 != 0) window = window + 1
    
    means = rollapply(lightVar, window, mean, na.rm=na.rm, partial=FALSE, fill=NA)
    if(max) center = which(means==max(means,na.rm=TRUE))[1]
    else center = which(means==min(means,na.rm=TRUE))[1]
    
    df = df %>% add_row(timespan = as.numeric(parsed_ts$time),
                        mean = means[center], 
                        midpoint = as.numeric(dtVar[center]), 
                        onset = as.numeric(dtVar[center-(window/2)+1]), 
                        offset = as.numeric(dtVar[center+(window/2)]))
  }
  if("POSIXct" %in% class(dtVar))
    df = df %>% mutate_at(vars(midpoint:offset), as_datetime, tz = tz(dtVar))
  
  if(max) names(df)[-1] = paste0("max_period_", names(df)[-1])
  else names(df)[-1] = paste0("min_period_", names(df)[-1])
  if(wide) df = df %>% pivot_wider(names_from=timespan, values_from=names(df)[-1], 
                                   names_sep = ".")
  if(as_df) return(df)
  else return(as.numeric(df))
}

# Temporal Pattern --------------------------------------------------------

#Frequency of intensity changes
fic = function(lightVar, threshold=1000, na.rm=TRUE, as_df = TRUE, wide=TRUE){
  if(na.rm) lightVar = na.omit(lightVar)
  
  df = tibble(threshold=numeric(), fic=numeric()) 
  for(c in threshold){
    val = sum(abs(diff(lightVar >= c)))
    df = df %>% add_row(threshold=c, fic=val)
  }
  
  if(wide){
    df = df %>% pivot_wider(names_from=threshold, values_from=fic)
    if(ncol(df)==1) names(df) = paste0("fic.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

#Disparity index
disparity_index = function(lightVar, na.rm=TRUE, as_df=TRUE){
  if(na.rm) lightVar = na.omit(lightVar)
  
  fractions = (lightVar[2:length(lightVar)]+1)/(lightVar[1:length(lightVar)-1]+1)
  di = 1/(length(lightVar)-1) * sum(abs(log(fractions)))
  
  if(as_df) return(tibble(disparity_index = di))
  else return(di)
} 

#Coefficient of variation
coeff_var = function(lightVar, na.rm=TRUE, as_df=TRUE){
  cv = sd(lightVar, na.rm=na.rm)/mean(lightVar, na.rm=na.rm)
  if(as_df) return(tibble(coeff_var = cv))
  else return(cv)
}

#Light quality index
#Defaults from Martinez-Nicolas et al. 2011
lqi = function(lightVar, upper=500, lower=10, sampling_int=60, na.rm=TRUE, as_df=TRUE){
  high = tat(lightVar, upper, sampling_int, na.rm=na.rm)[[1]]
  low = tat(lightVar, -1*lower, sampling_int, na.rm=na.rm)[[1]]
  lqi = (high - low) / (high + low)
  if(as_df) return(tibble(LQI = lqi))
  else return(lqi)
}

#Inter-daily stability:
#Variance of average 24h pattern / total variance across measurement period
is = function(lightVar, dtVar, na.rm=TRUE, as_df=TRUE){
  total = tibble(light = lightVar, datetime = dtVar) %>%  
    group_by(datetime_hour = floor_date(datetime, "hour")) %>%
    summarise(light = mean(light, na.rm = na.rm))
  daily = total %>% group_by(hour = hour(datetime_hour)) %>% 
    summarise(light = mean(light, na.rm = na.rm))
  is = var(daily$light) / var(total$light)
  
  if(as_df) return(tibble(IS = is))
  else return(is)
}

#Intra-daily variability: 
#Variance of consecutive hourly differences / total variance across measurement period
iv = function(lightVar, dtVar, na.rm = TRUE, as_df = TRUE){
  total = tibble(light = lightVar, datetime = dtVar) %>%  
    group_by(datetime_hour = floor_date(datetime, "hour")) %>%
    summarise(light = mean(light, na.rm = na.rm))
  var_hourly_diff = sum(diff(total$light, 1)^2) / (length(total$light) - 1)
  iv = var_hourly_diff / var(total$light)
  
  if(as_df) return(tibble(IV = iv))
  else return(iv)
}

#Combined function for IS and IV
is_iv = function(lightVar, dtVar, na.rm = TRUE, as_df = TRUE){
  is = is(lightVar, dtVar, na.rm, as_df)
  iv = iv(lightVar, dtVar, na.rm, as_df)
  
  if(as_df) return(add_column(is, iv))
  else return(c(is,iv))
}

#Relative amplitude
ra = function(lightVar, dtVar, max="10 h", min="5 h", sample_int=60, loop=FALSE, na.rm=TRUE, as_df=TRUE){
  m10 = max_min_period(lightVar, dtVar, max, sample_int = sample_int, loop = loop, na.rm = na.rm)
  l5 = max_min_period(lightVar, dtVar, min, max=FALSE, sample_int = sample_int, loop = loop, na.rm = na.rm)
  ra = (m10$max_period_mean.10 - l5$min_period_mean.5) / (m10$max_period_mean.10 + l5$min_period_mean.5)
  
  if(as_df) return(tibble(RA = ra))
  else return(ra)
}

#Pulses above/below threshold
#Defaults from Wilson et al. 2018
pulses = function(lightVar, dtVar, threshold=200, min_length="0 min", max_interrupt="8 min",
                  at_proportion=0.75, sampling_int=60, unit_out_plength="mins", loop=FALSE,
                  na.rm=TRUE, as_df=TRUE, wide=TRUE, indices=FALSE){
  #Loop data
  if(loop){
    lightVar = c(lightVar, lightVar)
    dt = as.numeric(dtVar)
    dt = c(dt[1:length(dt)-1], dt + (dt[length(dt)]-dt[1]))
    if("POSIXct" %in% class(dtVar)) dtVar = dt %>% as_datetime(tz = tz(dtVar))
    else dtVar = dt
  }
  #Parse time unit input
  minlen = parse.timeunit.tosecs(min_length)$secs/sampling_int
  maxint = parse.timeunit.tosecs(max_interrupt)$secs/sampling_int
  
  df = tibble(threshold=numeric(), 
              number_pulses=numeric(),
              mean_pulse_intensity=numeric(),
              geomean_pulse_intensity=numeric(),
              mean_pulse_length=numeric(),
              total_pulse_time=numeric(),
              mean_pulse_midpoint=numeric(),
              mean_pulse_onset=numeric(),
              mean_pulse_offset=numeric())
  i_list = list()
  for(c in threshold){
    clust = FALSE
    icount = 0
    temp = c(NULL)
    tempi = c(NULL)
    index = list()
    clusters.dt = list()
    clusters.light = list()
    for(i in c(1:length(lightVar))){
      if(threshold(lightVar[i],c)){
        #Light above threshold
        if(icount > 0){
          #Add brief interruptions to cluster and reset
          temp = append(temp, tempi)
          icount = 0
          tempi = c(NULL)
        }
        #Add light above threshold to cluster
        clust = TRUE
        temp = append(temp, i)
      } 
      else if(clust){
        # Clustering is still on: counting interruptions
        icount = icount+1
        if(icount <= maxint) 
          # Add to container while max interruption length not reached
          tempi = append(tempi, i)
        else{
          # Max interruption length reached
          if(1-(sum(!threshold(temp,c))/sum(threshold(temp,c))) >= at_proportion &
             length(temp) >= minlen){
            # Add previous cluster to cluster list if conditions fulfilled
            clusters.light = append(clusters.light, list(lightVar[temp]))
            clusters.dt = append(clusters.dt, list(as.numeric(dtVar[temp])))
            if(indices) index = append(index, list(temp))
          } 
          #Reset everything
          clust = FALSE
          icount = 0
          temp = c(NULL)
          tempi = c(NULL)
        }
      }
    }
    #Calculate metrics and add to tibble
    p_n = length(clusters.light)
    p_mean = mean(unlist(lapply(clusters.light, mean, na.rm=TRUE)))
    p_geomean = mean(unlist(lapply(clusters.light, geomean, na.rm=TRUE)))
    p_len = (unlist(lapply(clusters.light, length)) * sampling_int) %>% 
      from.secs(unit_out_plength) %>% mean(na.rm=T)
    p_time = p_n*p_len
    p_on = unlist(lapply(clusters.dt, first)) %>% mean(na.rm=T)
    p_off = unlist(lapply(clusters.dt, last)) %>% mean(na.rm=T)
    p_mid = unlist(lapply(clusters.dt, mean)) %>% mean(na.rm=T)
    df = df %>% add_row(threshold=c, number_pulses=p_n, mean_pulse_intensity=p_mean,
                        geomean_pulse_intensity=p_geomean,
                        mean_pulse_length=p_len, total_pulse_time=p_time,
                        mean_pulse_onset=p_on, mean_pulse_midpoint=p_mid, 
                        mean_pulse_offset=p_off)
    if(indices) i_list = append(i_list, list(index))
  }
  if("POSIXct" %in% class(dtVar))
    df = df %>% mutate_at(vars(mean_pulse_midpoint:mean_pulse_offset), round) %>% 
    mutate_at(vars(mean_pulse_midpoint:mean_pulse_offset), as_datetime, tz = tz(dtVar))
  #Reshape to wide format
  if(wide) df = df %>% pivot_wider(names_from=threshold, values_from=names(df)[-1], 
                                   names_sep = ".")
  #if(indices) return(list(df = df, idx = i_list))
  if(as_df) return(df)
  else return(as.numeric(df))
} 

#Inverse of TAT: max or min threshold of TAT == timespan
#Barroso et al
timespan_threshold = function(lightVar, timespan, above, sampling_int=60, as_df=TRUE, 
                            wide = TRUE){
  
  df = tibble(timespan=numeric(), threshold=numeric()) 
  for(ts in timespan){
    parsed_ts = parse.timeunit.tosecs(ts)
    idx = (parsed_ts$secs/sampling_int) %>% floor()
    sorted = sort(lightVar, decreasing = above)
    val = sorted[idx]
    df = df %>% add_row(timespan = as.numeric(parsed_ts$time), threshold=val)
  }
  
  if(wide){
    df = df %>% pivot_wider(names_from=timespan, values_from=threshold)
    if(ncol(df)==1) names(df) = paste0("threshold.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

#Length of longest continuous cluster above/below threshold
#Barroso et al
cat_max = function(lightVar, threshold, sampling_int=60, unit_out="mins", loop=FALSE, 
                   as_df=TRUE, wide=TRUE){
  max_clust = function(x){
    x[is.na(x)] = 0
    z = c(x, 0)
    z = (cumsum(z) * c(diff(z)<0, 0))
    max(diff(z[z != 0]))
  }
  
  #Loop data
  if(loop) lightVar = c(lightVar, lightVar)
  
  df = tibble(threshold=numeric(), cat_max=numeric())
  for(c in threshold){
    val = (max_clust(threshold(lightVar, c)) * sampling_int) %>% from.secs(unit_out)
    df = df %>% add_row(threshold=c, cat_max=val)
  }
  # Reshape to wide format
  if(wide){
    df = df %>% pivot_wider(names_from=threshold, values_from=cat_max)
    if(ncol(df)==1) names(df) = paste0("cat_max.",names(df)) 
  }
  if(as_df) return(df)
  else return(as.numeric(df))
}

#Metrics by barroso et al.
barroso = function(lightVar, dtVar, trim=0.2, sampling_int=60, loop=FALSE, na.rm=TRUE, as_df=TRUE, wide=TRUE){
  # Bright/dark thresholds
  tB = timespan_threshold(lightVar, "6 h", above=TRUE, sampling_int=sampling_int, as_df=FALSE)[1]
  tD = timespan_threshold(lightVar, "8 h", above=FALSE, sampling_int=sampling_int, as_df=FALSE)[1]
  
  # Bright/dark mean level
  mB = trimmed_mean(lightVar[lightVar >= tB], trim=trim, na.rm=na.rm)
  mD = trimmed_mean(lightVar[lightVar <= tD], trim=trim, na.rm=na.rm)
  
  # Bright/dark cluster
  cB = cat_max(lightVar, tB, loop=loop, as_df=FALSE)[1]
  cD = cat_max(lightVar, -1*(tD+0.01), loop=loop, as_df=FALSE)[1]
  
  #Circadian variation
  civ = coeff_var(lightVar, na.rm=na.rm, as_df=FALSE)
  
  df = tibble(bright_threshold = tB,
              dark_threshold = tD,
              bright_mean_level = mB,
              dark_mean_level = mD,
              bright_cluster = cB,
              dark_cluster = cD,
              circadian_variation = civ)
  if(as_df) return(df)
  else return(as.numeric(df))
}

#Wavelet
wts = function(lightVar, solarVar=NULL, sampling_int=60, unit_out="min", min.scale=NULL,
               max.scale=NULL, mother="morlet"){
  require(biwavelet)
  
  dt = from.secs(sampling_int, unit_out)
  time = seq(0, dt*(length(lightVar)-1), dt)
  data = cbind(time, lightVar, solarVar)
  if(is.null(min.scale)) min.scale = 2*dt
  if(is.null(max.scale)) max.scale = max(time)
  
  # Wavelet transform
  wt = wt(data[,1:2], dt = dt, s0 = min.scale, 
          max.scale = max.scale, mother = mother)
  global_ws = rowMeans(wt$power)
  
  # Wavelet transform for solar radiation
  if(!is.null(solarVar)){
    wt_solar = wt(data[,c(1,3)], dt = dt, s0 = min.scale, 
            max.scale = max.scale, mother = mother)
    global_ws_solar = rowMeans(wt_solar$power)
    # Superposition
    global_ws_super = sqrt(global_ws^2-global_ws_solar^2)
    drop = which(is.nan(global_ws_super))[1]
    # Find peaks in personal data until superposition drops off
    peaks = amax(global_ws[1:drop])
  }
  else{
    # Find peaks in personal data
    peaks = amax(global_ws)
  }
  peak_periods = wt$period[peaks]
  # Fit linear model to personal data
  lm = lm(log10(global_ws) ~ log10(wt$period))
  slope = lm$coefficients[2]
  
  list(periods = peak_periods, slope = slope)
} 

# Exposure History --------------------------------------------------------

#Cumulative sum
ce = function(lightVar, as_df=TRUE){
  ce = sum(lightVar)
  if(as_df) return(tibble(CE = ce))
  else return(ce)
}

ce_sn = function(lightVar, solarVar, as_df=TRUE){
  ce_sn = (sum(lightVar)/sum(solarVar)*100) %>% round(2)
  if(as_df) return(tibble(CE_sn = ce_sn))
  else return(ce_sn)
}

#Dose of light exposure within range
tatr_dose = function(lightVar, lower, upper, sampling_int=60, unit_out="mins", as_df=TRUE,
                     wide=TRUE){
  if(length(lower) != length(upper)) stop("lower and upper bounds must be same length")
  
  df = tibble(threshold_min=numeric(), threshold_max=numeric(), tat_dose=numeric()) 
  for(i in 1:length(lower)){
    cmin = lower[i]
    cmax = upper[i]
    val = (sum(between(lightVar, cmin, cmax)) * sampling_int) %>% 
      from.secs(unit_out) * (cmax - cmin)/2
    df = df %>% add_row(threshold_min=cmin, threshold_max=cmax, tat_dose=val)
  }
  #Reshape to wide format
  if(wide){
    df = df %>% unite(threshold, threshold_min, threshold_max) %>%
      pivot_wider(names_from=threshold, values_from=tat_dose)
    if(ncol(df)==1) names(df) = paste0("tat_dose.",names(df)) 
  }
  
  if(as_df) return(df)
  else return(as.numeric(df))
}

#Exponential moving average
ema = function(lightVar, beta){
  D = replicate(length(lightVar),0)
  for(t in 1:length(lightVar)){
    if(t == 1) D[t] = beta*(lightVar[t]) 
    else D[t] = D[t-1] + beta*(lightVar[t]-D[t-1])
  }
  return(D)
}

#Simple moving average
sma = function(lightVar, window_size, align = "center", partial = TRUE){
  rollapply(lightVar, window_size, mean, align = align, partial = partial)
}


# Cosinor Analysis --------------------------------------------------------

#Cosinor function
cosinor = function(lightVar, dtVar, unit_in = "hours"){
  df = tibble(light = log10p1(lightVar), datetime = dtVar) 
  if(is.POSIXct(dtVar)) 
    df = df %>% mutate(time = as.numeric(as_hms(datetime)) %>% to.hours("s"))
  else
    df = df %>% mutate(time = datetime %>% to.hours(unit_in))
  circa_single(df, "time", "light")
}

#Summary of cosinor coefficients
cosinor_summary = function(lightVar, dtVar, unit_in = "secs", as_df = TRUE){
  cosinor = cosinor({{lightVar}}, {{dtVar}}, unit_in)
  df = cosinor$summary
  df = df %>% pivot_wider(names_from = "parameter", values_from = "value")
  
  if(is.POSIXct(dtVar))
    df = df %>% mutate(acrophase = (to.secs(peak_time_hours,"h")+min(as.numeric(dtVar))) %>%
                         as_datetime(tz=tz(dtVar)))
    
  #Add R2
  y = log10p1(lightVar)
  predicted = predict(cosinor$fit)
  R2=1-sum((y-predicted)^2)/(length(y)*var(y))
  df = df %>% mutate(rsquared = R2)

  if(as_df) return(df)
  else return(as.numeric(df))
}

#Plot cosinor fit
cosinor.plot = function(lightVar, dtVar, unit_in = "secs", point_colour = "black", 
                        line_colour = "red"){
  cosinor = cosinor(lightVar, dtVar, unit_in)
  cosinor.plot = cosinor$plot
  cosinor.plot$layers[[1]]$aes_params$size = 0.5
  cosinor.plot$layers[[1]]$aes_params$colour = point_colour
  cosinor.plot$layers[[2]]$aes_params$colour = line_colour
  return(cosinor.plot)
}


# Deprecated --------------------------------------------------------------

#' #' Simple moving average
#' #'
#' #' @param data A data frame
#' #' @param group_vars Grouping variables (must be passed within vars() expression)
#' #' @param summary_vars Variables to be summarized (must be passed within vars() expression)
#' #' @param window_size Window size of moving average (numeric)
#' #' @return A data frame
#' #' 
#' transform.sma = function(data, group_vars, summary_vars, window_size){
#'   data %>% 
#'     group_by(!!!group_vars) %>% 
#'     mutate_at(summary_vars, list(sma = ~sma(.x, window_size)))
#' }
#' 
#' #' Exponential moving average
#' #'
#' #' @param data A data frame
#' #' @param group_vars Grouping variables (must be passed within vars() expression) 
#' #' @param summary_vars Variables to be summarized (must be passed within vars() expression)
#' #' @param half_life Half-life of decay (numeric)
#' #' @return A data frame
#' #'
#' transform.ema = function(data, group_vars, summary_vars, half_life){
#'   data %>% 
#'     group_by(!!!group_vars) %>%
#'     mutate_at(summary_vars, list(ema = ~ema(., log(2)/half_life)))
#' }

#' #' Title
#' #'
#' #' @param data 
#' #' @param time_var 
#' #' @param interval 
#' #' @param group_vars 
#' #' @param summary_vars 
#' #' @param summary_funs 
#' #' @return
#' #'
#' summarise.interval = function(data, time_var, interval, group_vars, summary_vars, summary_funs){
#'   data %>% 
#'     subset.interval({{time_var}}, interval) %>% 
#'     summarise.by(group_vars, summary_vars, summary_funs) %>% 
#'     return()
#' }
#' 
#' 
#' #' Function to calculate time above threshold (TAT)
#' #'
#' #' @param data A data frame 
#' #' @param reference_var A light variable of interest
#' #' @param sign A logical operator to filter the light data relative to the threshold (string)
#' #' @param threshold A threshold value (numeric) 
#' #' @param group_vars Grouping variables (must be passed within vars() expression)
#' #' @param sampling_rate Sampling rate of the data in minutes. Default is 1
#' #' @return A data frame
#' #' 
#' summarise.tat = function(data, reference_var, sign, threshold, group_vars, sampling_rate = 1){
#'   filter = glue("{quo_name(enquo(reference_var))} {sign} {threshold}")
#'   data %>% 
#'     mutate(over = eval(parse(text = filter))) %>% 
#'     group_by(!!!group_vars) %>%
#'     summarise(tat = sum(over) * sampling_rate) %>% 
#'     return()
#' }
#' 
#' #' Function to calculate time in threshold range (TIT)
#' #'
#' #' @param data A data frame 
#' #' @param reference_var A light variable of interest
#' #' @param sign A logical operator to filter the light data relative to the threshold (string)
#' #' @param range A range of two threshold values (numeric) 
#' #' @param group_vars Grouping variables (must be passed within vars() expression)
#' #' @param sampling_rate Sampling rate of the data in minutes. Default is 1
#' #' @return A data frame
#' #' 
#' summarise.tit = function(data, reference_var, range, group_vars, sampling_rate = 1){
#'   data %>% 
#'     mutate(withinrange = between({{reference_var}}, range[1], range[2])) %>% 
#'     group_by(!!!group_vars) %>%
#'     summarise(tit = sum(withinrange) * sampling_rate) %>% 
#'     return()
#' }
#' 
#' #' Function to calculate mean light timing above threshold (MLiT) and its standard deviation (MLiT_sd)
#' #'
#' #' @param data A data frame
#' #' @param reference_var A light variable of interest
#' #' @param sign A logical operator to filter the light data relative to the threshold (string)
#' #' @param threshold A threshold value (numeric) 
#' #' @param group_vars Grouping variables (must be passed within vars() expression)
#' #' @param time_var A datetime variable
#' #' @param sampling_rate The sampling rate of the data in minutes. Default is 1
#' #' @return A data frame
#' #' 
#' summarise.mlit = function(data, reference_var, sign, threshold, group_vars, time_var, sampling_rate = 1){
#'   filter = glue("{quo_name(enquo(reference_var))} {sign} {threshold}")
#'   data %>% 
#'     group_by(!!!group_vars) %>%
#'     mutate(over = eval(parse(text = filter)),
#'            timing_over = over * (as_hms({{time_var}}) / 60) %>% as.numeric()) %>% 
#'     summarise(mlit = (sum(timing_over) / sum(over)),
#'               mlit_sd = sd(timing_over)) %>% 
#'     return()
#' }
#' 
#' 
#' #' Function to calculate light quality index (TAT_high - TAT_low) / (TAT_high + TAT_low)
#' #'
#' #' @param data 
#' #' @param reference_var 
#' #' @param group_vars 
#' #' @param thresholds 
#' #' @param sampling_rate 
#' #' @return
#' #' 
#' summarise.lqi = function(data, reference_var, group_vars, thresholds = c(10,500), sampling_rate = 1){
#'   DF.low = summarise.tat(data, {{reference_var}}, "<", thresholds[1], group_vars, sampling_rate) %>% 
#'     rename(tat_low = tat)
#'   DF.high = summarise.tat(data, {{reference_var}}, ">", thresholds[2], group_vars, sampling_rate) %>% 
#'     rename(tat_high = tat) 
#'   DF = left_join(DF.low, DF.high) %>%
#'     mutate(lqi = (tat_high - tat_low) / (tat_high + tat_low)) %>%
#'     return()
#' }
#' 
#' 
#' summarise.is = function(data, reference_var, group_vars, day_var){
#'   data %>% 
#'     group_by(!!!group_vars) %>%
#'     mutate(total_variance = var({{reference_var}})) %>% 
#'     group_by({{day_var}}, .add = TRUE) %>% 
#'     mutate(daily_variance = var({{reference_var}}),
#'            is = daily_variance / total_variance)
#' }


