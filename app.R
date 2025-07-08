library(shiny)
library(shinydashboard)
library(tibble)

source("functions/api_query.R")  # must contain get_meeting_key and get_session_key

ui <- dashboardPage(
  dashboardHeader(title = "OpenF1 Data Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Meeting Lookup", tabName = "meeting_lookup", icon = icon("calendar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "App Overview", width = 6, solidHeader = TRUE, status = "primary",
                  p("This app allows users to explore Formula 1 race data using the OpenF1 API."),
                  p("All data comes from ", a("OpenF1 API", href = "https://openf1.org", target = "_blank")),
                  h4("App Features"),
                  tags$ul(
                    tags$li("Data Download: Query and download F1 session data"),
                    tags$li("Data Exploration: Summarize and visualize performance metrics"),
                    tags$li("Multiple drivers, sessions, and races supported")
                  )
                ),
                box(
                  title = "Data Overview", width = 6, solidHeader = TRUE, status = "info",
                  tableOutput("about_table")
                )
              )
      ),
      
      # Meeting Lookup Tab
      tabItem(tabName = "meeting_lookup",
              fluidRow(
                box(
                  title = "Lookup Meeting & Session Keys", width = 6, solidHeader = TRUE, status = "warning",
                  dateInput("meeting_date", "Choose a date:", value = Sys.Date()),
                  actionButton("get_meeting", "Get Meeting Key"),
                  br(), br(),
                  textOutput("meeting_key_output"),
                  br(),
                  actionButton("get_session", "Get Session Keys for Meeting"),
                  br(), br(),
                  textOutput("session_key_output")
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
  
  # Meeting key lookup
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
  
  # Session key lookup
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
}

shinyApp(ui, server) 