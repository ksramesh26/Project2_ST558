library(httr)
library(jsonlite)
library(tidyverse)

url_base = "https://api.openf1.org/v1/"


get_f1_data = function(endpoint = "car_data", options = list()) {
  url = paste0(url_base, endpoint)
  f1_data = httr::GET(url, query = options)
  
  f1_data_parsed = fromJSON(rawToChar(f1_data$content))
  
  return(as.tibble(f1_data_parsed))
  
}

test_df = get_f1_data(endpoint = "car_data", options = list(driver_number = 63, session_key = 9158))
