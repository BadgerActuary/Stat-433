library(tidyverse)
library(stringr)
library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)

dt = read.csv("data&figures/dt.csv")
us_states = map_data("state")
us_counties = map_data("county")
dt$State_Name = state.name[match(dt$State, state.abb)]
dt$County_Name = tolower(dt$County)

# User interface ----
ui = fluidPage(
  navbarPage("Data visualization", 
    tabPanel("Examples of DataTables",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                              names(dt), 
                              selected = names(dt)
          ), 
          width=2
        ),
        
        mainPanel(
           fluidRow(column(4,selectInput("St","State:",c("All",unique(as.character(dt$State))))),
                    column(4,selectInput("Cty","County:",c("All",unique(as.character(dt$County)))))
           ),
                          
           
           DT::dataTableOutput("myTable")
            
        )
      )
    ),
             
    tabPanel("County Map",
      sidebarLayout(
        sidebarPanel(
          helpText("Create county maps given a selected variable in a state."),
                          
          selectInput("var","Variable",
            choices = c("HPI", "Personal_Income",
                        "Poverty_Percentage", "Population",
                        "HighSchoolLess", "HighSchoolOnly",
                        "SomeCollege", "BachelorAndHigher",
                        "Unemployment_Rate"),
                        selected = "HPI"
          ),
          selectInput("St2",
                    "State:",
                    choices = unique(as.character(dt$State_Name)),
                    selected = "Alabama"
          ),
          width=2
        ),
       
      mainPanel(plotOutput("myMap"))
     )
    )
  )
)

# Server logic ----
server = function(input, output) {
  output$myTable = DT::renderDataTable(DT::datatable({
    temp=dt
    if (input$St != "All") {
      temp = temp[temp$State == input$St,]
    }
    if (input$Cty != "All") {
      temp = temp[temp$County == input$Cty,]
    }
    temp[, input$show_vars, drop = FALSE]
  }))
  
  output$myMap = renderPlot({
    data = switch(input$var, 
                   "HPI"=dt[,c(1,2,3,12,13)], 
                   "Personal_Income"=dt[,c(1,2,4,12,13)],
                   "Poverty_Percentage"=dt[,c(1,2,5,12,13)], 
                   "Population"=dt[,c(1,2,6,12,13)],
                   "HighSchoolLess"=dt[,c(1,2,7,12,13)], 
                   "HighSchoolOnly"=dt[,c(1,2,8,12,13)],
                   "SomeCollege"=dt[,c(1,2,9,12,13)], 
                   "BachelorAndHigher"=dt[,c(1,2,10,12,13)],
                   "Unemployment_Rate"=dt[,c(1,2,11,12,13)]
    )
    var = input$var
    
    inputState = subset(us_states, region == tolower(input$St2))
    State_county = subset(us_counties, region == tolower(input$St2))
    
    plot_dt = inner_join(State_county, data[which(dt$State_Name == input$St2),], by = c("subregion"="County_Name"))
    
    ditch_the_axes = theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    )
    
    ggplot(data = inputState, mapping = aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.3) + 
      geom_polygon(fill = "white")+ 
      geom_polygon(data = plot_dt, aes(fill = plot_dt[,var]), color = "white") +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      ditch_the_axes + 
      guides(fill=guide_legend(title=var))

  })
}

# Run app ----
shinyApp(ui=ui, server)