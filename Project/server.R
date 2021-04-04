library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)

dt = read.csv("data&figures/dt.csv")

ui <- fluidPage(
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(
        checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                           names(dt), selected = names(dt)),
        width=2
    ),
    mainPanel(
      fluidRow(
        column(4,
               selectInput("St",
                           "State:",
                           c("All",
                             unique(as.character(dt$State))))
        ),
        column(4,
               selectInput("Cty",
                           "County:",
                           c("All",
                             unique(as.character(dt$County))))
        )
      ),
      
      tabsetPanel(
        id = 'dataset',
        tabPanel("data", DT::dataTableOutput("table"))
        
      )
    )
  )
)

server <- function(input, output) {
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data=dt
    if (input$St != "All") {
      data <- data[data$State == input$St,]
    }
    if (input$Cty != "All") {
      data <- data[dt$County == input$Cty,]
    }
    data[, input$show_vars, drop = FALSE]
  }))
}

shinyApp(ui, server)