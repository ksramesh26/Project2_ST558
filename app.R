library(shiny)
library(shinydashboard)
library(tibble)
library(tidyverse)

source("functions/api_query.R")
source("functions/summarize_data.R")
source("functions/plotting_functions.R")

ui <- dashboardPage(
  dashboardHeader(title = "OpenF1 Data Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Meeting Lookup", tabName = "meeting_lookup", icon = icon("calendar")),
      menuItem("Summary Tables", tabName = "summary_tables", icon = icon("table")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-line")),
      menuItem("Data Download", tabName = "data_download", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "App Overview", width = 6, solidHeader = TRUE, status = "primary",
                  p("This app allows users to explore Formula 1 race data using the OpenF1 API. It provides tools to download, summarize, and visualize various metrics related to Formula 1 sessions."),
                  p("All data is sourced from the ", a("OpenF1 API", href = "https://openf1.org", target = "_blank"), ". This API provides publicly available race telemetry and event data from Formula 1 events."),
                  h4("Tabs"),
                  tags$ul(
                    tags$li(strong("Meeting Lookup:"), " Retrieve meeting and session keys using a calendar date."),
                    tags$li(strong("Summary Tables:"), " Display summary statistics and contingency tables from session data."),
                    tags$li(strong("Visualizations:"), " Generate visualization for session analysis."),
                    tags$li(strong("Data Download:"), " Query data from the API and save it locally as a CSV file.")
                  ),
                  h4("App Features"),
                  tags$ul(
                    tags$li("Download F1 session data"),
                    tags$li("Summarize performance metrics"),
                    tags$li("Create custom plots"),
                    tags$li("Easy to use for beginners")
                  )
                ),
                box(
                  title = "Data Overview", width = 6, solidHeader = TRUE, status = "info",
                  tableOutput("about_table"),
                  tags$img(src = "f1_logo.jpg", height = "150px", alt = "F1 Logo")
                )
              )
      ),
      
      tabItem(tabName = "meeting_lookup",
              fluidRow(
                box(
                  title = "Find Meeting & Session Info", width = 6, solidHeader = TRUE, status = "warning",
                  dateInput("meeting_date", "Choose a date:", value = Sys.Date()),
                  actionButton("get_meeting", "Look Up Meeting Key"),
                  br(), br(),
                  textOutput("meeting_key_output"),
                  br(),
                  actionButton("get_session", "Find Sessions for this Meeting"),
                  br(), br(),
                  textOutput("session_key_output")
                )
              )
      ),
      
      tabItem(tabName = "summary_tables",
              fluidRow(
                box(
                  title = "Get Summary Tables", width = 4, solidHeader = TRUE, status = "primary",
                  textInput("summary_meeting_key", "Meeting Key", value = "1219"),
                  textInput("summary_session_key", "Session Key (optional)", value = ""),
                  actionButton("generate_summaries", "Show Tables")
                ),
                box(
                  title = "Contingency Tables", width = 8, solidHeader = TRUE, status = "info",
                  h5("Tire Usage by Session"),
                  tableOutput("tire_usage"),
                  h5("Incidents by Driver and Session"),
                  tableOutput("incidents"),
                  h5("Stints by Driver and Session"),
                  tableOutput("stints")
                ),
                box(
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
      
      tabItem(tabName = "visualizations",
              fluidRow(
                box(
                  title = "Plot Options", width = 4, solidHeader = TRUE, status = "primary",
                  textInput("plot_session_key", "Session Key", value = ""),
                  uiOutput("driver_selector"),
                  selectInput("plot_choice", "Select a Plot", choices = c("Lap Duration", "Compound Heatmap", "Lap Std Dev", "Gap to Leader")),
                  actionButton("generate_plots", "Create Plot")
                ),
                box(
                  title = "Visual Output", width = 8, solidHeader = TRUE, status = "info",
                  plotOutput("selected_plot")
                )
              )
      ),
      
      tabItem(tabName = "data_download",
              fluidRow(
                box(
                  title = "Custom Query and Download", width = 4, solidHeader = TRUE, status = "primary",
                  selectInput("data_endpoint", "Select a Dataset", choices = c("drivers", "intervals", "laps", "meetings", "pit", "position", "stints", "session_result", "race_control")),
                  textInput("data_meeting_key", "Optional: Race ID", value = ""),
                  textInput("data_session_key", "Optional: Session ID", value = ""),
                  actionButton("fetch_data", "Pull Data"),
                  br(), br(),
                  uiOutput("column_selector"),
                  downloadButton("download_data", "Save CSV")
                ),
                box(
                  title = "Your Data Table", width = 8, solidHeader = TRUE, status = "info",
                  DT::dataTableOutput("fetched_data")
                )
              )
      )
      
    )
  )
)


server <- function(input, output, session) {
  
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
  
  meeting_key_reactive <- reactiveVal(NULL)
  
  observeEvent(input$get_meeting, {
    date_selected <- as.character(input$meeting_date)
    key <- get_meeting_key(date_selected)
    
    if (is.null(key)) {
      output$meeting_key_output <- renderText("No race found for the selected date.")
      meeting_key_reactive(NULL)
    } else {
      output$meeting_key_output <- renderText(paste("Meeting Key:", paste(key, collapse = ", ")))
      meeting_key_reactive(key)
    }
  })
  
  observeEvent(input$get_session, {
    key <- meeting_key_reactive()
    
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
  
  output$driver_selector <- renderUI({
    session_key <- input$plot_session_key
    if (nzchar(session_key)) {
      laps <- get_f1_data("laps", list(session_key = session_key))
      drivers <- unique(as.character(laps$driver_number))
      selectInput("plot_driver_number", "Choose Driver(s)", choices = drivers, selected = drivers[1], multiple = TRUE)
    }
  })
  
  observeEvent(input$generate_summaries, {
    key <- input$summary_meeting_key
    sesh <- if (nzchar(input$summary_session_key)) input$summary_session_key else NULL
    
    output$tire_usage <- renderTable({ tire_usage_by_session(key) })
    output$incidents <- renderTable({ incidents_by_driver_session(key) })
    output$stints <- renderTable({ stints_by_driver_session(key) })
    output$stint_length_driver <- renderTable({ stint_length_by_driver(key, sesh) })
    output$stint_length_compound <- renderTable({ stint_length_by_compound(key, sesh) })
    output$lap_duration_driver <- renderTable({ lap_duration_by_driver(key, sesh) })
    output$lap_duration_compound <- renderTable({ lap_duration_by_compound(key, sesh) })
  })
  
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
  
  fetched_data <- reactiveVal(NULL)
  
  observeEvent(input$fetch_data, {
    params <- list()
    if (nzchar(input$data_meeting_key)) params$meeting_key <- input$data_meeting_key
    if (nzchar(input$data_session_key)) params$session_key <- input$data_session_key
    
    fetched <- get_f1_data(input$data_endpoint, params)
    fetched_data(fetched)
  })
  
  output$fetched_data <- DT::renderDataTable({
    req(fetched_data())
    DT::datatable(fetched_data()[, input$selected_columns, drop = FALSE])
  })
  
  output$column_selector <- renderUI({
    req(fetched_data())
    checkboxGroupInput("selected_columns", "Pick the Columns You Want", choices = names(fetched_data()), selected = names(fetched_data()))
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste0("f1_data_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(fetched_data()[, input$selected_columns, drop = FALSE], file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)


