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


