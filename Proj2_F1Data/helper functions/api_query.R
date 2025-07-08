library(httr)
library(jsonlite)
library(tidyverse)

## setting base url
url_base = "https://api.openf1.org/v1/"

## creating api call
get_f1_data = function(endpoint = "car_data", options = list()) {
  #create api url
  url = paste0(url_base, endpoint)
  
  #call api
  f1_data = httr::GET(url, query = options)
  
  #convert to readable data
  f1_data_parsed = fromJSON(rawToChar(f1_data$content))
  
  return(as_tibble(f1_data_parsed))
  
}

#Testing to see if api call works
#test_df = get_f1_data(endpoint = "car_data", options = list(driver_number = 23 , session_key = "latest"))
#head(test_df)


## making different endpoints

# car data
get_car_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("car_data", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key))
}

# position data
get_position_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("position", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# Lap data
get_lap_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("laps", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# Pit stop data
get_pit_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("pit", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# stint data 
get_stint_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("stints", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# interval data
get_interval_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("intervals", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# driver data
get_driver_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("drivers", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# finishing position
get_finish_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("session_result", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# Race Control indicents
get_incident_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("race_control", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}




# get meeting key based on date
get_meeting_key = function(date2) {
  date1 = get_f1_data("meetings", list(date_start = date2))
  
  
  if(nrow(date1) == 0){
    print("No race found for given date")
    return(NULL)
  } 
  
  
  return(date1$meeting_key)
}

# get session key based on date
get_session_key = function(meet_key) {
  sessions = get_f1_data("sessions", list(meeting_key = meet_key))
  
  
  if(nrow(date1) == 0){
    print("No sessions found for given date")
    return(NULL)
  } 
  
  
  return(date1$meeting_key)
}





