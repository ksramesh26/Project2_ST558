# Project2_ST558

This shiny app allows users to explore historical formula 1 data though interactive tables and visuals

- Look up race meetings and session details

- View contingency tables (e.g., tire usage per session)

- Generate summary statistics (e.g., stint length by driver)

- Visualize lap times, compound usage, and gaps to the leader

- Download and explore raw data

- Use a flexible data exploration tab to create custom visual and numeric summaries

"WARNING: Not all data is able to be used for data exploration plots"

## Required R Packages

The following packages are required to run the app:

- `shiny`
- `tidyverse`
- `httr`
- `jsonlite`

# Install Required Packages

To install all necessary packages in one step, run:

install.packages(c("shiny", "tidyverse", "httr", "jsonlite"))


# Run the App

shiny::runGitHub("Project2_ST558", "ksramesh26")

