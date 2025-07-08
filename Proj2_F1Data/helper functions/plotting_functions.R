library(tidyverse)

##Plot Lap duration by lap number for a given drivers per session
plot_lap_duration_by_lap_number = function(driver_numbers, session_key) {
  
  ## get lap data for the selected session
  laps_df = get_f1_data("laps", list(session_key = session_key))
  
  ## filter for selected drivers
  driver_laps = laps_df %>%
    filter(driver_number %in% driver_numbers, !is.na(lap_number), !is.na(lap_duration)) %>%
    mutate(driver_number = as.character(driver_number))  #coloring
  
  ## create  plot
  
  
  ggplot(driver_laps, 
         aes(x = lap_number,
             y = lap_duration,
             color = driver_number,
             group = driver_number
         )) +
    geom_line(size = 1) +
    geom_point(size = 1) +
    labs(
      title = "Lap Duration by Lap Number",
      x = "Lap Number",
      y = "Lap Duration (seconds)",
      color = "Driver"
    ) +
    theme_minimal()
}


## Heatmap of compound usage per driver 
plot_compound_usage_heatmap = function(meeting_key1 = NULL, sesh_key = NULL) {
  
  ## get lap data
  laps_df = get_f1_data("laps", list(meeting_key = meeting_key1, session_key = sesh_key))
  
  ## get stint data (contains compound info)
  stints_df = get_f1_data("stints", list(meeting_key = meeting_key1, session_key = sesh_key))
  
  ## expand stint laps to assign compound to each lap
  stints_expanded = stints_df %>%
    filter(!is.na(lap_start), !is.na(lap_end)) %>%
    mutate(lap_number_in_stint = purrr::map2(lap_start, lap_end, seq)) %>%
    tidyr::unnest(lap_number_in_stint) %>%
    select(driver_number, lap_number_in_stint, compound)
  
  ## join compound info to lap data
  laps_with_compound = dplyr::left_join(
    laps_df,
    stints_expanded,
    by = c("driver_number", "lap_number" = "lap_number_in_stint")
  )
  
  ##  heatmap data count of laps by driver and compound
  heatmap_data = laps_with_compound %>%
    filter(!is.na(compound)) %>%
    count(driver_number, compound) %>%
    mutate(driver_number = as.character(driver_number))
  
  ## plot heatmap
  ggplot(heatmap_data, aes(x = compound, y = driver_number, fill = n)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(
      title = "Heatmap of Compound Usage by Driver",
      x = "Compound",
      y = "Driver Number",
      fill = "Lap Count"
    ) +
    theme_minimal()
}


## Lap time standard deviation by driver 
plot_lap_stddev_all_drivers = function(session_key) {
  
  # Get lap data and clean
  laps_df = get_f1_data("laps", list(session_key = session_key)) %>%
    filter(!is_pit_out_lap, !is.na(lap_duration)) %>%
    mutate(driver_number = as.character(driver_number))
  
  # standard deviation of lap times
  std_summary = laps_df %>%
    group_by(driver_number) %>%
    summarise(
      lap_sd = sd(lap_duration, na.rm = TRUE)
    ) 
  
  # Plot: standard deviation per driver
  ggplot(std_summary, aes(x = reorder(driver_number, -lap_sd), y = lap_sd, fill = driver_number)) +
    geom_bar(stat = "identity") +
    labs(
      title = "Lap Time Standard Deviation by Driver",
      x = "Driver Number",
      y = "Standard Deviation (s)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

## Plot gap to leader over session
plot_gap_to_leader = function(driver_numbers, session_key) {
  
  # Get interval data
  interval_df = get_f1_data("intervals", list(session_key = session_key)) %>%
    filter(!is.na(gap_to_leader), driver_number %in% driver_numbers) %>%
    mutate(
      driver_number = as.character(driver_number),
      time = as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
    ) %>%
    arrange(driver_number, time)
  
  
  # Plot
  ggplot(interval_df, aes(x = time, y = gap_to_leader, color = driver_number)) +
    geom_line(size = 1) +
    labs(
      title = "Gap to Leader",
      x = "Time (UTC)",
      y = "Gap to Leader (seconds)",
      color = "Driver"
    ) +
    theme_minimal()
}



