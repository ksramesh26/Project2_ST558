library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)


## load external r scrips for api access and generating summaries
source("functions/api_query.R")
source("functions/summarize_data.R")
source("functions/plotting_functions.R")


## UI portion

ui <- dashboardPage(
  dashboardHeader(title = "OpenF1 Data Explorer"),
  
  # Sidebar with navigation menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Meeting Lookup", tabName = "meeting_lookup", icon = icon("calendar")),
      menuItem("Summary Tables", tabName = "summary_tables", icon = icon("table")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-line")),
      menuItem("Data Download", tabName = "data_download", icon = icon("download")),
      menuItem("Data Exploration", tabName = "data_exploration", icon = icon("chart-bar"))
    )
  ),
  
  ## Body with content for each tab
  dashboardBody(
    tabItems(
      # The about tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "App Overview", width = 6, solidHeader = TRUE, status = "primary",
                  p("This app allows users to explore Formula 1 race data using the OpenF1 API."),
                  p("All data is sourced from the ", a("OpenF1 API", href = "https://openf1.org", target = "_blank"), "."),
                  h4("Tabs"),
                  tags$ul(
                    tags$li(strong("Meeting Lookup:"), " Retrieve meeting and session keys."),
                    tags$li(strong("Summary Tables:"), " Display summary statistics."),
                    tags$li(strong("Visualizations:"), " Create session analysis plots."),
                    tags$li(strong("Data Download:"), " Query and save session data.")
                  ),
                  h4("App Features"),
                  tags$ul(
                    tags$li("Download F1 session data"),
                    tags$li("Summarize performance metrics"),
                    tags$li("Create custom plots"),
                    tags$li("WARNING: Not all data is able to be used for data exploration plots")
                  )
                ),
                box(
                  title = "Data Overview", width = 6, solidHeader = TRUE, status = "info",
                  tableOutput("about_table"), # Summary table with basic app info
                  tags$img(src = "f1_logo.jpg", height = "150px", alt = "F1 Logo")
                )
              )
      ),
      ## meeting lookup tab
      tabItem(tabName = "meeting_lookup",
              fluidRow(
                box(
                  title = "Find Meeting & Session Info", width = 6, solidHeader = TRUE, status = "warning",
                  dateInput("meeting_date", "Choose a date:", value = Sys.Date()), # Date picker to select meeting date
                  actionButton("get_meeting", "Look Up Meeting Key"), ## api trigger
                  br(), br(),
                  textOutput("meeting_key_output"), ## meeting key output
                  br(),
                  actionButton("get_session", "Find Sessions for this Meeting"), ## take meet key and return sessions
                  br(), br(),
                  textOutput("session_key_output") ##session key output
                )
              )
      ),
      ## summary tables tab
      tabItem(tabName = "summary_tables",
              fluidRow(
                box(
                  title = "Get Summary Tables", width = 4, solidHeader = TRUE, status = "primary",
                  textInput("summary_meeting_key", "Meeting Key", value = "1277"), ## required meeting key, default 1277
                  textInput("summary_session_key", "Session Key (optional)", value = ""), ## optional session key
                  actionButton("generate_summaries", "Show Tables") ## button to generate tables
                ),
                box( ### summary tables tire, incident, stints contingency tables
                  title = "Contingency Tables", width = 8, solidHeader = TRUE, status = "info",
                  h5("Tire Usage by Session"), 
                  tableOutput("tire_usage"),
                  h5("Incidents by Driver and Session"),
                  tableOutput("incidents"),
                  h5("Stints by Driver and Session"),
                  tableOutput("stints")
                ),
                box( ### summaries 
                  title = "Numerical Summaries", width = 12, solidHeader = TRUE, status = "success",
                  h5("Stint Length by Driver"),
                  tableOutput("stint_length_driver"),
                  h5("Stint Length by Compound"),
                  tableOutput("stint_length_compound"),
                  h5("Lap Duration by Driver"),
                  tableOutput("lap_duration_driver"),
                  h5("Lap Duration by Compound"),
                  tableOutput("lap_duration_compound")
                )
              )
      ),
      ## visualizations
      tabItem(tabName = "visualizations",
              fluidRow(
                box(
                  title = "Plot Options", width = 4, solidHeader = TRUE, status = "primary",
                  textInput("plot_session_key", "Session Key", value = ""), ## required session key
                  uiOutput("driver_selector"), ## dynamic driver selector based on session data
                  selectInput("plot_choice", "Select a Plot", choices = c("Lap Duration", "Compound Heatmap", "Lap Std Dev", "Gap to Leader")), ### choose graph
                  actionButton("generate_plots", "Create Plot")
                ),
                box(
                  title = "Visual Output", width = 8, solidHeader = TRUE, status = "info",
                  plotOutput("selected_plot") ## output selected plot
                )
              )
      ),
      ## data download 
      tabItem(tabName = "data_download",
              fluidRow(
                box(
                  title = "Custom Query and Download", width = 4, solidHeader = TRUE, status = "primary",
                  ## selected api endpoint to collect data
                  selectInput("data_endpoint", "Select a Dataset", choices = c("drivers", "intervals", "laps", "meetings", "pit", "position", "stints", "session_result", "race_control")),
                  textInput("data_meeting_key", "Optional: Meeting ID", value = ""), ## optional filters
                  textInput("data_session_key", "Optional: Session ID", value = ""),
                  actionButton("fetch_data", "Pull Data"), ## trigger api call
                  br(), br(),
                  uiOutput("column_selector"), ## select columsn from pulled dataset
                  downloadButton("download_data", "Save CSV") ## download
                ),
                box(
                  title = "Your Data Table", width = 8, solidHeader = TRUE, status = "info",
                  dataTableOutput("fetched_data") ##output
                )
              )
      ),
      ## data exploration
      tabItem(tabName = "data_exploration",
              fluidRow(
                box(
                  title = "Exploration Controls", width = 4, solidHeader = TRUE, status = "primary",
                  ## select a data set
                  selectInput("explore_endpoint", "Select a Dataset", 
                              choices = c("drivers", "intervals", "laps", "meetings", "pit", "position", "stints", "session_result", "race_control")),
                  ## session input
                  textInput("explore_session_key", "Session Key", value = ""),
                  ## api trigger
                  actionButton("load_explore_data", "Load Data"),
                  br(), br(),
                  #x var
                  selectInput("xvar", "X Variable", choices = NULL),
                  #yvar
                  selectInput("yvar", "Y Variable", choices = NULL),
                  #plot type
                  selectInput("plot_type", "Choose Plot Type", choices = c("Boxplot", "Bar Chart", "Scatterplot")),
                  #facet function
                  selectInput("facet_var", "Facet By", choices = NULL),
                  ##summary statistics
                  radioButtons("summary_type", "Summary", choices = c("Mean", "Median", "SD"))
                ),
                box(
                  title = "Exploration Plot", width = 8, solidHeader = TRUE, status = "info",
                  plotOutput("exploration_plot"), ## create plot
                  tableOutput("exploration_summary")
                )
              )
      )
    )
  )
)






server <- function(input, output, session) {
  ## render table for about section
  output$about_table <- renderTable({
    tibble(
      Component = c("API", "Endpoints", "Summaries", "Plot Types"),
      Description = c(
        "OpenF1 API (https://openf1.org)",
        "Car, Drivers, Laps, Pit, Positions, Race Control, Intervals, Stints",
        "Contingency tables and grouped stats by driver/compound",
        "Line plot, bar chart, heatmap, time series"
      )
    )
  })
  
  ## store meeting key from date
  meeting_key_reactive <- reactiveVal(NULL)
  
  ## call api with selected date
  observeEvent(input$get_meeting, {
    date_selected <- as.character(input$meeting_date)
    key <- get_meeting_key(date_selected)
    
    ## error functionality for incorrect dates 
    
    if (is.null(key)) {
      output$meeting_key_output <- renderText("No race found for the selected date.")
      meeting_key_reactive(NULL)
    } else {
      output$meeting_key_output <- renderText(paste("Meeting Key:", paste(key, collapse = ", ")))
      meeting_key_reactive(key)
    }
  })
  
  ## find session keys for stored meeting key
  observeEvent(input$get_session, {
    key <- meeting_key_reactive()
    
    
    ## error handleing
    if (is.null(key)) {
      output$session_key_output <- renderText("Please get a valid meeting key first.")
    } else {
      session_keys <- get_session_key(key)
      
      if (is.null(session_keys)) {
        output$session_key_output <- renderText("No sessions found for this meeting.")
      } else {
        output$session_key_output <- renderText(paste("Session Keys:", paste(session_keys, collapse = ", ")))
      }
    }
  })
  
  ## generate contingency tables and quantitative stats with meeting keys
  observeEvent(input$generate_summaries, {
    key <- input$summary_meeting_key
    ## makes sure a value is passed to api call to prevent errors
    sesh <- if (nzchar(input$summary_session_key)) input$summary_session_key else NULL
    
    ## summary functions
    output$tire_usage <- renderTable({ tire_usage_by_session(key) })
    output$incidents <- renderTable({ incidents_by_driver_session(key) })
    output$stints <- renderTable({ stints_by_driver_session(key) })
    output$stint_length_driver <- renderTable({ stint_length_by_driver(key, sesh) })
    output$stint_length_compound <- renderTable({ stint_length_by_compound(key, sesh) })
    output$lap_duration_driver <- renderTable({ lap_duration_by_driver(key, sesh) })
    output$lap_duration_compound <- renderTable({ lap_duration_by_compound(key, sesh) })
  })
  
  
  
  ## allows driver selection on visulaization tab
  output$driver_selector <- renderUI({
    session_key <- input$plot_session_key
    if (nzchar(session_key)) {
      laps <- get_f1_data("laps", list(session_key = session_key))
      drivers <- unique(as.character(laps$driver_number))
      selectInput("plot_driver_number", "Choose Driver(s)", choices = drivers, selected = drivers[1], multiple = TRUE)
    }
  })
  
  ## renders selected graphs
  observeEvent(input$generate_plots, {
    session_key <- input$plot_session_key
    driver_numbers <- input$plot_driver_number
    selected_plot <- input$plot_choice
    
    output$selected_plot <- renderPlot({
      if (selected_plot == "Lap Duration") {
        plot_lap_duration_by_lap_number(driver_numbers, session_key)
      } else if (selected_plot == "Compound Heatmap") {
        plot_compound_usage_heatmap(session_key)
      } else if (selected_plot == "Lap Std Dev") {
        plot_lap_stddev_all_drivers(session_key)
      } else if (selected_plot == "Gap to Leader") {
        plot_gap_to_leader(driver_numbers, session_key)
      }
    })
  })
  
  ## pull dataset to be downloaded
  fetched_data <- reactiveVal(NULL)
  
  ## fetch selected end point with meeting ot session key
  observeEvent(input$fetch_data, {
    params <- list()
    if (nzchar(input$data_meeting_key)) params$meeting_key <- input$data_meeting_key
    if (nzchar(input$data_session_key)) params$session_key <- input$data_session_key
    
    fetched <- get_f1_data(input$data_endpoint, params)
    fetched_data(fetched)
  })
  ## render table based on selected columsn
  output$fetched_data <- renderDataTable({
    req(fetched_data())
    DT::datatable(fetched_data()[, input$selected_columns, drop = FALSE])
  })
  
  ## column selected for downloads
  output$column_selector <- renderUI({
    req(fetched_data())
    checkboxGroupInput("selected_columns", "Pick Columns", choices = names(fetched_data()), selected = names(fetched_data()))
  })
  
  # file download for filtered data
  output$download_data <- downloadHandler(
    filename = function() { paste0("f1_data.csv") },
    content = function(file) {
      write.csv(fetched_data()[, input$selected_columns, drop = FALSE], file, row.names = FALSE)
    }
  )
  
  ## temp storage for data exploration
  
  explore_data <- reactiveVal(NULL)
  
  ## call api from endpoint
  observeEvent(input$load_explore_data, {
    req(input$explore_session_key, input$explore_endpoint)
    params <- list(session_key = input$explore_session_key)
    df <- get_f1_data(input$explore_endpoint, params)
    explore_data(df)
  })
  
  ## update xvar and yvar UI based on selected endpoint
  observeEvent(explore_data(), {
    updateSelectInput(session, "xvar", choices = names(explore_data()))
    updateSelectInput(session, "yvar", choices = names(explore_data()))
    updateSelectInput(session, "facet_var", choices = c("None", names(explore_data())))
  })
  
  
  ## show summary of selected x variable
  output$exploration_summary <- renderTable({
    req(explore_data(), input$xvar)
    data <- explore_data()
    var_sym <- rlang::sym(input$xvar)
    summary_df <- data %>%
      filter(!is.na(!!var_sym)) %>%
      summarise(
        Mean = mean(!!var_sym, na.rm = TRUE),
        Median = median(!!var_sym, na.rm = TRUE),
        SD = sd(!!var_sym, na.rm = TRUE)
      )
    summary_df %>% select(all_of(input$summary_type))
  })
  
  
  ## Render plot based on user selection
  output$exploration_plot <- renderPlot({
    req(explore_data(), input$xvar, input$yvar)
    data <- explore_data()
    p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar))
    
    if (input$plot_type == "Boxplot") {
      p <- p + geom_boxplot()
    } else if (input$plot_type == "Bar Chart") {
      p <- p + geom_col()
    } else if (input$plot_type == "Scatterplot") {
      p <- p + geom_point(alpha = 0.6)
    }
    
    if (input$facet_var != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
    }
    
    p + theme_minimal()
  })
}
  
 
## Run shiney app  
shinyApp(ui, server)


