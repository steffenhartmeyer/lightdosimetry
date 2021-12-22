require(writexl)
require(rlang)
require(tidyverse)

# Time unit conversion ----------------------------------------------------


as_datetime_hms = function(x){
  as_datetime(as_hms(x))
}

#Convert to/from seconds
to.secs = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit,"m" = t * 60,"h" = t * 3600,"d" = t * 86400)
}
from.secs = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit, "m" = t / 60, "h" = t / 3600, "d" = t / 86400)
}

#Convert to/from minutes
to.mins = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit,"s" = t / 60,"h" = t * 60,"d" = t * 1440)
}
from.mins = function(t, unit){
  unit = substr(unit,1,1)
  switch(unit, "s" = t * 60, "h" = t / 60, "d" = t / 1440)
}

#Convert to/from hours
to.hours = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,"s" = t / 3600,"m" = t / 60,"d" = t * 24)
}
from.hours = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,"s" = t * 3600,"m" = t * 60,"d" = t / 24)
}

#Convert to/from days
to.days = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,"s" = t / 86400,"m" = t / 1440,"h" = t / 24)
}
from.days = function(t, unit){
  unit = substr(unit,1,1)
  switch (unit,"s" = t * 86400,"m" = t * 1440,"h" = t * 24)
}

#Change time of datetime variable
shift.datetime = function(datetime, hours, minutes, seconds){
  update(datetime, hour = hours, minute = minutes, second = seconds)
}

#Convert time 
parse.timeunit.tosecs= function(input){
  if(!str_detect(input, "\\d+ (sec|min|h)")) 
    stop("Wrong time unit specification!")
  parsed = str_split(input, " ")[[1]]
  list(secs = to.secs(as.numeric(parsed[1]), parsed[2]), 
       time = parsed[1],
       unit = parsed[2])
}

# Calculation -------------------------------------------------------------

#Geometric mean
geomean <- function(x, na.rm = FALSE, trim = 0, ...){
  x[x == 0] = 1
  exp(mean(log(x, ...), na.rm = na.rm, trim = trim, ...))
}

#Geometric standard deviation
geosd <- function(x, na.rm = FALSE, ...){
  x[x == 0] = 1
  exp(sd(log(x, ...), na.rm = na.rm, ...))
}

#Standard error
se = function(x, na.rm = FALSE) {
  if(na.rm) x = na.omit(x)
  sqrt(var(x)/length(x))
}

#Log10(x+1)
log10p1 = function(x){
  log10(x + 1)
}

# Plot helper -------------------------------------------------------------

# Custom ggplot theme
theme_custom = function(base_size = 14, base_family = "Arial"){
  library(ggsci)
  (theme_light(base_size = base_size, base_family = base_family)) +
    theme(#strip.background=element_rect(colour="black",fill="#f0f0f0"),
          #strip.text = element_text(colour="black", face="bold"),
          strip.background=element_rect(colour="grey20",
                                        fill="grey20"),
          strip.text = element_text(colour="white", 
                                    face="bold",
                                    margin = margin(0.08, 0, 0.08, 0, "cm"), 
                                    size = 12),
          axis.line = element_line(colour="black"),
          axis.ticks = element_line(colour="black"),
          axis.text = element_text(colour = "black", size = 12),
          legend.spacing.x = unit(0.1, "cm"), 
          legend.key.size = unit(0.5, "cm"),
          panel.border = element_rect(colour = "grey20"),
          )
}

theme_Publication <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face="bold", size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line.x = element_line(colour="black"),
            axis.line.y = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
}

scale_colour_branded <- function(
  primary = "blue", 
  other = "grey", 
  direction = 1, 
  ...
) {
  ggplot2::discrete_scale(
    "colour", "branded", 
    branded_pal(primary, other, direction), 
    ...
  )
}

branded_pal <- function(
  primary = "blue", 
  other = "grey", 
  direction = 1
) {
  branded_colors <- list(
    "blue"   = "#00798c",
    "red"    = "#d1495b",
    "yellow" = "#edae49",
    "green"  = "#66a182",
    "navy"   = "#2e4057", 
    "grey"   = "#8d96a3"
  )
  stopifnot(primary %in% names(branded_colors))
  
  function(n) {
    if (n > 6) warning("Branded Color Palette only has 6 colors.")
    
    if (n == 2) {
      other <- if (!other %in% names(branded_colors)) {
        other
      } else {
        branded_colors[other]
      }
      color_list <- c(other, branded_colors[primary])
    } else {
      color_list <- branded_colors[1:n]
    }
    
    color_list <- unname(unlist(color_list))
    if (direction >= 0) color_list else rev(color_list)
  }
}


# Misc --------------------------------------------------------------------

normalize_minmax = function(x){
  (x-min(x))/(max(x)-min(x))
}

#Return evenly spaced datetime vector
regular_datetime = function(dtVar, epoch=60){
  start = as.numeric(dtVar)[1] 
  end = as.numeric(dtVar)[length(dtVar)]
  datetime = seq(start, end, epoch)
  if(is.POSIXct(dtVar)) datetime = as_datetime(datetime, tz=tz(dtVar))
  return(datetime)
}

#Find peaks
amax <- function(x)
{
  a1 <- c(0,x,0)
  a2 <- c(x,0,0)
  a3 <- c(0,0,x)
  e <- which((a1 >= a2 & a1 > a3)[2:(length(x))])
  if(!is.na(e[1] == 1))
    if(e[1]==1)
      e <- e[-1]
  if(length(e) == 0) e <- NaN
  return (e)
}


#Count valid observations 
count.obs = function(x){
  sum(!is.na(x))
}

#Write dataframe to file
writeDF = function(x, dir, type = "txt"){
  dfname = deparse(substitute(x)) %>% str_replace("\\.","_")
  path = paste0(dir, dfname, ".", type)
  switch(type, 
         "txt" = write.table(x, path), 
         "xlsx" = write_xlsx(x, path))
}

#Decide whether above or below threshold
threshold = function(light, threshold){
  if(threshold < 0) out = light <= abs(threshold)
  else out = light >= threshold
  replace_na(out, FALSE)
}

#Mean while preserving counting for count variables
mean.and.count = function(x, count_suffix = "_n", na.rm = TRUE){
  varname = deparse(substitute(x))
  if(str_detect(varname, paste0(".", count_suffix))) sum(x)
  else mean(x, na.rm = na.rm)
}

# Summary functions -------------------------------------------------------

#Easier summarizing in one line, with option to reduce to one value by iteratively 
#averaging over each grouping var
summarise.by = function(data, group_vars, summary_vars, summary_funs, reduce = FALSE){
  df = data %>%
    group_by(!!!group_vars) %>% 
    summarise_at(summary_vars, summary_funs)
  
  if(reduce){
    while(length(group_vars(df)) > 0) df = df %>% summarise_all(mean.and.count)
    df = df %>% summarise_all(mean.and.count) %>% select(!c(!!!group_vars))
  }
  
  return(df)
}

# Subsetting functions ----------------------------------------------------

#' Function to subset a data frame by a given interval
#'
#' @param data A data frame
#' @param time_var The reference time variable
#' @param interval A vector containing the start and end times of the interval ("hh:mm:ss")
#' @return A data frame
#' 
subset.hmsinterval = function(data, time_var, interval, subset = TRUE){
  dat = data %>% mutate(within_interval = between(as_hms({{time_var}}), as_hms(interval[1]), as_hms(interval[2])))
  if(subset) dat = dat %>% filter(within_interval) %>% select(!c(within_interval))
  return(dat)
}


#' Function to subset a data frame from an interval given in another data frame 
#'
#' @param data A data frame to be subsetted
#' @param group_vars Grouping variables (must be passed within vars() expression)
#' @param interval_DF A data frame with interval (must contain the same grouping variables and start/end times of interval)
#' @param interval_vars Variables containing start and end-times of interval (must be passed within vars() expression)
#' @param sampling_rate Sampling rate of data in the same unit of the time variable. Default is 60 seconds
#' @param time_var The reference time variable 
#' @return A data frame
#' 
subset.interval = function(data, time_var, group_vars, interval_DF, interval_vars, sampling_rate=60, subset=TRUE, drop_ivars=TRUE){
  #Create interval sequences from start and end times
  iDF = interval_DF %>%
    select(!!!group_vars, !!!interval_vars) %>% 
    rowwise() %>%
    mutate(within_interval = TRUE,
           {{time_var}} := ifelse(is.na(!!interval_vars[[1]]) | is.na(!!interval_vars[[2]]) | (!!interval_vars[[1]] > !!interval_vars[[2]]),
                                  list(NA),
                                  list(seq(!!interval_vars[[1]], !!interval_vars[[2]], by = sampling_rate)))) %>%
    unnest(cols = c({{time_var}})) %>%
    ungroup() %>% 
    drop_na() 
  
  #Drop interval variables
  if(drop_ivars) iDF = iDF %>% select(!c(!!!interval_vars))
  
  #Subset data frame
  dat = data %>% 
    left_join(iDF, by = c(unlist(map(group_vars, as_name)), tail(names(iDF), 1))) %>% 
    mutate(within_interval = replace_na(within_interval, FALSE))
  if(subset) dat = dat %>% filter(within_interval) %>% select(!c(within_interval))
  return(dat)
}


