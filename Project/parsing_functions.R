library(stringr)
library(ISOweek)
parse_municipality_name <- function(name) {
  
  return(substr(name,1,4))
  
}
# are on the format Antall.fall.[year].v.[week-number]. We will return this as weeks since start of pandemic
parse_timestamp <- function(timestamp) {
  
  year <- as.numeric(substr(timestamp,12,15))
  week <- as.numeric(substr(timestamp,19,nchar(timestamp)))
  
  return(paste0(year,"-",week))
}

parse_timestamps <- function(timestamps) {
  new_timestamps <- rep(0, length(timestamps))
  for(i in 1:length(timestamps)) {
    new_timestamps[i] <- parse_timestamp(timestamps[i])
  }
  return(new_timestamps)
}

week_range_from_year <- function(year) {
  
  if(year == 1) {
    return(1:53)
  }
  start = 54 + (year - 2) * 52
  end = start + 50
  return(start:end)
  
  
}

time_to_year <- function(x) min(4,as.numeric(substr(x,1,4)) - 2019)

times_to_years <- function(x) {
  to_return <- rep(0,length(x))
  for(i in 1:length(x)) {
    to_return[i] <- time_to_year(x[i])
  }
  return(to_return)
}