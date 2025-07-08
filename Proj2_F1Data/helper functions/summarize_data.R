library(tidyverse)


## contingency tables

tire_usage_by_session = function(meeting_key1) {
  
  #get stint data
  df = get_f1_data("stints", list(meeting_key = meeting_key1))
  
  #force integer and get sessino names
  sessions = get_f1_data("sessions", list(meeting_key = meeting_key1)) %>%
    select(session_key, session_name) %>%
    mutate(session_key = as.integer(session_key))
  
  #get force integer
  filtered_stints <- df %>%
    mutate(session_key = as.integer(session_key))
  
  #merge sets for session name
  merged_data <- left_join(filtered_stints, sessions, by = "session_key")
  
  #filter and organize data 
  tire_contingency = merged_data %>%
    filter(!is.na(compound), !is.na(session_key)) %>%
    count(compound, session_key) %>%
    pivot_wider(
      names_from = session_name,
      values_from = n,
      values_fill = 0
    ) %>%
    arrange(compound)
  
  return(tire_contingency)
}


