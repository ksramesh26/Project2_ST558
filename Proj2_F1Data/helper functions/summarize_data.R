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

# Testing to see if api call works
#test_df = get_f1_data(endpoint = "position", options = list(driver_number = 23 , session_key = "latest"))
#head(test_df)


## making different endpoints

# car data
get_car_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL) {
  get_f1_data("car_data", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key))
}



# position data
get_position_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date1 = NULL) {
  get_f1_data("position", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# Lap data
get_lap_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date = NULL) {
  get_f1_data("laps", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# Pit stop data
get_pit_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date = NULL) {
  get_f1_data("pit", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

# stint data 
get_stint_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date = NULL) {
  get_f1_data("stints", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

#interval data
get_interval_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date = NULL) {
  get_f1_data("intervals", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}

#driver data
get_driver_data = function(driver = NULL, sesh_key = NULL, meet_key = NULL, date = NULL) {
  get_f1_data("drivers", list(driver_number = driver, session_key = sesh_key, meeting_key = meet_key, date = date1))
}



#test to see if modified base call works 
get_position_data(55, NULL, NULL, "2025-07-05")
  









