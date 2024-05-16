# Utility functions related to units ===========================================

hourMinute <- JS(
  
  # JS function converting ECharts line value in seconds 
  # to string format in H:m format
  
  'function (value) {
    if (value.componentType== "markLine") value = value.value;
    return Math.floor(value / 3600).toString() + "H" + 
      Math.floor(value % 3600 / 60).toString() + "m";
  }'
  
)


toKmString <- JS(
  
  # JS function converting numeric kilometer value to 
  # character string
  
  'function (value) {
    return Math.round(value).toString() + "km";
  }'
  
)


toMString <- JS(
  
  # JS function converting numeric meter value to 
  # character string
  
  'function (value) {
    return Math.round(value).toString() + "m";
  }'
  
)


race_time_to_percentile <- function(race_time, race_time_distribution) {
  
  # Returns the percentile value of a given race time based 
  # on the specified race time distribution. Race time is 
  # passed as a character string in hour:minute format.
  
  race_time <- trimws(race_time)
  hours <- gsub("(^[0-9]{1,2}):.*", "\\1", race_time) %>% as.numeric()
  minutes <- gsub(".*:([0-9]{2}$)", "\\1", race_time) %>% as.numeric()
  
  if (is.na(hours) || is.na(minutes)) {
    
    warning("Non-numeric time input")
    return(0.5)
    
  }
  
  seconds <- (hours * 60 + minutes) * 60
  
  return(race_time_distribution(seconds))
  
}


race_pace_to_percentile <- function(race_pace, race_distance, race_time_distribution) {
  
  # Returns the percentile value of a given race pace based 
  # on the specified race time distribution. Race pace is 
  # passed as a character string in minute:second format.
  
  race_pace <- trimws(race_pace)
  
  minutes <- gsub("(^[0-9]{1,2}):.*", "\\1", race_pace) %>% as.numeric()
  seconds <- gsub(".*:([0-9]{1,2}$)", "\\1", race_pace) %>% as.numeric()
  
  if (is.na(minutes) || is.na(seconds)) {
    
    warning("Non-numeric pace input")
    return(0.5)
    
  }
  
  race_seconds <- (minutes * 60 + seconds) * race_distance
  
  return(race_time_distribution(race_seconds))
  
}

