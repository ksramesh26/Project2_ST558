library(tidyverse)


### contingency tables


##tire_usage_by_session
tire_usage_by_session = function(meeting_key1) {
  
  #get stint data
  df = get_f1_data("stints", list(meeting_key = meeting_key1))
  
  #force integer and get sessino names
  sessions = get_f1_data("sessions", list(meeting_key = meeting_key1)) %>%
    select(session_key, session_name) %>%
    mutate(session_key = as.integer(session_key))
  
  #for stint as integer
  filtered_stints <- df %>%
    mutate(session_key = as.integer(session_key))
  
  #merge sets for session name
  merged_data <- left_join(filtered_stints, sessions, by = "session_key")
  
  #filter and organize data 
  tire_contingency = merged_data %>%
    filter(!is.na(compound), !is.na(session_name)) %>%
    count(compound, session_name) %>%
    pivot_wider(
      names_from = session_name,
      values_from = n,
      values_fill = 0
    ) %>%
    arrange(compound)
  
  return(tire_contingency)
}


##incidents_by_driver_session
incidents_by_driver_session = function(meeting_key1){
  ##get racecontrol data
  df = get_f1_data("race_control", list(meeting_key = meeting_key1))
  ## get session data
  sessions = get_f1_data("sessions", list(meeting_key = meeting_key1)) %>%
    select(session_key, session_name) %>%
    mutate(session_key = as.integer(session_key))
  #force session key to be integer
  filtered_stints = df %>%
    mutate(session_key = as.integer(session_key))
  ##merge data
  merged_data = left_join(df, sessions, by = "session_key")
  ##filter and piviot data for table 
  incident_contingency = merged_data %>%
    filter(!is.na(driver_number), !is.na(session_name)) %>%
    count(driver_number, session_name) %>%
    pivot_wider(
      names_from = session_name,
      values_from = n,
      values_fill = 0
    ) %>%
    mutate(Total = rowSums(across(is.numeric))) %>%
    arrange(driver_number)
  return(incident_contingency)
  
}


##stints_by_driver_session

stints_by_driver_session = function(meeting_key1) {

  ## get stint data
  df = get_f1_data("stints", list(meeting_key = meeting_key1))
  ## get session data and for integer
  sessions = get_f1_data("sessions", list(meeting_key = meeting_key1)) %>%
    select(session_key, session_name) %>%
    mutate(session_key = as.integer(session_key))
  ##force stint integer
  filtered_stints = df %>%
    mutate(session_key = as.integer(session_key))
  ##merge data
  merged_data = left_join(filtered_stints, sessions, by = "session_key")
  
  #filter and organize data for table 
  stint_contingency = merged_data %>%
    filter(!is.na(driver_number), !is.na(session_name)) %>%
    mutate(driver_number = as.character(driver_number)) %>%
    count(driver_number, session_name) %>%
    pivot_wider(
      names_from = session_name,
      values_from = n,
      values_fill = 0
    ) %>%
    mutate(Total = rowSums(across(is.numeric))) %>%
    arrange(driver_number)
  
  return(stint_contingency)
}








### Numerical summaries of quantitative variables for categorical variables

##Summary stats for stint length by driver
stint_length_by_driver = function(meeting_key1 = NULL, sesh_key = NULL) {
  
  ##get stint data
  df = get_f1_data("stints", list(meeting_key = meeting_key1, session_key = sesh_key)) %>%
    mutate(stint_length = lap_end - lap_start)
  
  #create summary table of stint summary statistics
  stint_summary = df %>%
    group_by(driver_number) %>%
    summarise(
      avg_stint = mean(stint_length, na.rm = TRUE),
      median_stint = median(stint_length, na.rm = TRUE),
      max_stint = max(stint_length, na.rm = TRUE),
      min_stint = min(stint_length, na.rm = TRUE),
      sd_stint = sd(stint_length, na.rm = TRUE),
      count = n()
    ) %>%
    mutate(driver_number = as.character(driver_number)) %>%
    arrange(desc(avg_stint))
  
  return(stint_summary)
}

##Summary stats for stint length by tire compound
stint_length_by_compound = function(meeting_key1 = NULL, sesh_key = NULL) {
  
  ##get stint data
  df = get_f1_data("stints", list(meeting_key = meeting_key1, session_key = sesh_key)) %>%
    mutate(stint_length = lap_end - lap_start)
  
  #create summary table of stint summary statistics
  stint_summary = df %>%
    filter(!is.na(compound), !is.na(stint_length)) %>%
    group_by(compound) %>%
    summarise(
      avg_stint = mean(stint_length, na.rm = TRUE),
      median_stint = median(stint_length, na.rm = TRUE),
      max_stint = max(stint_length, na.rm = TRUE),
      min_stint = min(stint_length, na.rm = TRUE),
      sd_stint = sd(stint_length, na.rm = TRUE),
      count = n()
    ) %>%
    arrange(desc(avg_stint))
  
  return(stint_summary)
}

##Summary stats for lap time by driver
lap_duration_by_driver = function(meeting_key1 = NULL, sesh_key = NULL) {
  
  ##get lap data
  df = get_f1_data("laps", list(meeting_key = meeting_key1, session_key = sesh_key))
  
  #create summary table of lap duration statistics
  lap_summary = df %>%
    filter(!is.na(driver_number), !is.na(lap_duration)) %>%
    group_by(driver_number) %>%
    summarise(
      avg_lap = mean(lap_duration, na.rm = TRUE),
      median_lap = median(lap_duration, na.rm = TRUE),
      max_lap = max(lap_duration, na.rm = TRUE),
      min_lap = min(lap_duration, na.rm = TRUE),
      sd_lap = sd(lap_duration, na.rm = TRUE),
      count = n()
    ) %>%
    mutate(driver_number = as.character(driver_number)) %>%
    arrange(avg_lap)
  
  return(lap_summary)
}

## Summary stat for lap time by tire compound per driver
lap_duration_by_compound = function(meeting_key1 = NULL, sesh_key = NULL) {
  
  ##get lap data
  laps_df = get_f1_data("laps", list(meeting_key = meeting_key1, session_key = sesh_key))
  
  ## get stint data
  stints_df = get_f1_data("stints", list(meeting_key = meeting_key1, session_key = sesh_key))
  
  ##expand so compoud per lap is available
  stints_expanded = stints_df %>%
    filter(!is.na(lap_start), !is.na(lap_end)) %>%
    mutate(lap_number_in_stint = map2(lap_start, lap_end, seq)) %>%
    unnest(lap_number_in_stint) %>% 
    select(driver_number, lap_number_in_stint, compound)
  
  
  ##join compound info with laps
  laps_with_compound = left_join(
    laps_df,stints_expanded, by = c("driver_number", "lap_number" = "lap_number_in_stint")
  )
  
  
  #create summary table of lap duration statistics by compound
  lap_summary = laps_with_compound %>%
    filter(!is.na(driver_number), !is.na(compound), !is.na(lap_duration)) %>%
    group_by(driver_number, compound) %>%
    summarise(
      avg_lap = mean(lap_duration, na.rm = TRUE),
      median_lap = median(lap_duration, na.rm = TRUE),
      max_lap = max(lap_duration, na.rm = TRUE),
      min_lap = min(lap_duration, na.rm = TRUE),
      sd_lap = sd(lap_duration, na.rm = TRUE),
      count = n()
    ) %>%
    mutate(driver_number = as.character(driver_number)) %>%
    arrange(driver_number, compound)
  
  return(lap_summary)
}
