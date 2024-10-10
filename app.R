# Install and load required packages
# install.packages(c("shiny", "shinydashboard", "plotly", "DT"))
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Clinical Trial Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Account Management", tabName = "account", icon = icon("user")),
      menuItem("Data Overview", tabName = "data", icon = icon("chart-bar")),
      menuItem("Site Performance", tabName = "site", icon = icon("chart-line")),
      menuItem("Data Management", tabName = "manage", icon = icon("database")),
      menuItem("Safety Monitoring", tabName = "safety", icon = icon("shield-alt")),
      menuItem("Endpoints", tabName = "endpoints", icon = icon("flag-checkered"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              h2("Welcome to the Clinical Trial Dashboard"),
              p("Select a menu item to view specific data and analytics.")
      ),
      
      # Account Management tab
      tabItem(tabName = "account",
              h2("Account Management"),
              selectInput("user_role", "Select User Role:",
                          choices = c("Admin", "Investigator", "Data Manager", "Monitor")),
              uiOutput("role_specific_content")
      ),
      
      # Data Overview tab
      tabItem(tabName = "data",
              h2("Data Overview"),
              fluidRow(
                column(4,
                       selectInput("study_phase", "Select Study Phase:",
                                   choices = c("Phase I", "Phase II", "Phase III", "Phase IV"))
                ),
                column(4,
                       dateRangeInput("date_range", "Select Date Range:")
                )
              ),
              fluidRow(
                box(title = "Study Enrollment Status",
                    dataTableOutput("enrollment_table"),
                    downloadButton("download_enrollment", "Download Data")
                ),
                box(title = "Cumulative Study Enrollment per Month",
                    plotlyOutput("cumulative_enrollment"),
                    downloadButton("download_cumulative", "Download Data")
                )
              ),
              fluidRow(
                box(title = "# of Patients Currently at Each Visit",
                    plotlyOutput("patients_by_visit"),
                    downloadButton("download_visits", "Download Data"),
                    width = 12
                )
              )
      ),
      
      # Site Performance tab
      tabItem(tabName = "site",
              h2("Site Performance"),
              selectInput("site_metric", "Select Performance Metric:",
                          choices = c("Enrollment Rate", "Retention Rate", "Data Quality", "Protocol Adherence")),
              plotlyOutput("site_performance_plot")
      ),
      
      # Data Management tab
      tabItem(tabName = "manage",
              h2("Data Management"),
              selectInput("data_type", "Select Data Type:",
                          choices = c("Demographics", "Adverse Events", "Lab Results", "Efficacy Endpoints")),
              dataTableOutput("data_table")
      ),
      
      # Safety Monitoring tab
      tabItem(tabName = "safety",
              h2("Safety Monitoring"),
              selectInput("safety_category", "Select Safety Category:",
                          choices = c("Adverse Events", "Serious Adverse Events", "Deaths", "Discontinuations")),
              plotlyOutput("safety_plot")
      ),
      
      # Endpoints tab
      tabItem(tabName = "endpoints",
              h2("Endpoints"),
              selectInput("endpoint_type", "Select Endpoint Type:",
                          choices = c("Primary", "Secondary", "Exploratory")),
              plotlyOutput("endpoint_plot")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Account Management
  output$role_specific_content <- renderUI({
    switch(input$user_role,
           "Admin" = list(
             h3("Admin Dashboard"),
             actionButton("manage_users", "Manage Users"),
             actionButton("system_settings", "System Settings")
           ),
           "Investigator" = list(
             h3("Investigator Dashboard"),
             actionButton("view_patients", "View Patients"),
             actionButton("enter_data", "Enter Data")
           ),
           "Data Manager" = list(
             h3("Data Manager Dashboard"),
             actionButton("data_cleaning", "Data Cleaning"),
             actionButton("generate_reports", "Generate Reports")
           ),
           "Monitor" = list(
             h3("Monitor Dashboard"),
             actionButton("site_visits", "Schedule Site Visits"),
             actionButton("review_data", "Review Data")
           )
    )
  })
  
  # Data Overview (reusing from previous example)
  # ... [Include the data overview code from the previous example here]
  
  # Site Performance
  output$site_performance_plot <- renderPlotly({
    # Example plot - replace with actual data and logic
    plot_ly(x = c("Site A", "Site B", "Site C", "Site D"),
            y = runif(4, 0, 100),
            type = "bar") %>%
      layout(title = paste(input$site_metric, "by Site"),
             xaxis = list(title = "Site"),
             yaxis = list(title = input$site_metric))
  })
  
  # Data Management
  output$data_table <- renderDataTable({
    # Example data - replace with actual data based on input$data_type
    data.frame(
      ID = 1:10,
      Value1 = runif(10),
      Value2 = runif(10),
      Value3 = runif(10)
    )
  })
  
  # Safety Monitoring
  output$safety_plot <- renderPlotly({
    # Example plot - replace with actual data and logic
    plot_ly(x = c("Mild", "Moderate", "Severe"),
            y = c(15, 10, 5),
            type = "bar") %>%
      layout(title = paste(input$safety_category, "Summary"),
             xaxis = list(title = "Severity"),
             yaxis = list(title = "Count"))
  })
  
  # Endpoints
  output$endpoint_plot <- renderPlotly({
    # Example plot - replace with actual data and logic
    plot_ly(x = c("Baseline", "Week 4", "Week 8", "Week 12"),
            y = cumsum(runif(4, 5, 15)),
            type = "scatter",
            mode = "lines+markers") %>%
      layout(title = paste(input$endpoint_type, "Endpoint Over Time"),
             xaxis = list(title = "Time Point"),
             yaxis = list(title = "Value"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)