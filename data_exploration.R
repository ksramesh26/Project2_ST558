library(tidyverse)

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

