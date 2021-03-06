library(tidyverse)
library(stringr)
library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(plotly)

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
          checkboxGroupInput("show_vars", "Columns in dataset to show:",
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
    tabPanel("Simple linear regression",
      sidebarLayout(
        sidebarPanel(
          selectInput("V1","Variable1",
                      choices = c("HPI", "Personal_Income",
                                  "Poverty_Percentage", "Population",
                                  "HighSchoolLess", "HighSchoolOnly",
                                  "SomeCollege", "BachelorAndHigher",
                                  "Unemployment_Rate"),
                      selected = "HPI"
          ),
          selectInput("V2","Variable2",
                      choices = c("HPI", "Personal_Income",
                                  "Poverty_Percentage", "Population",
                                  "HighSchoolLess", "HighSchoolOnly",
                                  "SomeCollege", "BachelorAndHigher",
                                  "Unemployment_Rate"),
                      selected = "Personal_Income"
          ),
          width = 2
        ),
        mainPanel(plotlyOutput("SLR"))
      ),
    ),
             
    tabPanel("County Map",
      sidebarLayout(
        sidebarPanel(
          helpText("Create county maps given a selected variable in a state."),
                          
          selectInput("var1","Variable1",
            choices = c("HPI", "Personal_Income",
                        "Poverty_Percentage", "Population",
                        "HighSchoolLess", "HighSchoolOnly",
                        "SomeCollege", "BachelorAndHigher",
                        "Unemployment_Rate"),
                        selected = "HPI"
          ),
          selectInput("var2","Variable2",
                      choices = c("HPI", "Personal_Income",
                                  "Poverty_Percentage", "Population",
                                  "HighSchoolLess", "HighSchoolOnly",
                                  "SomeCollege", "BachelorAndHigher",
                                  "Unemployment_Rate"),
                      selected = "Personal_Income"
          ),
          selectInput("St2",
                    "State:",
                    choices = unique(as.character(dt$State_Name)),
                    selected = "Wisconsin"
          ),
          selectInput("overlap",
                      "Check overlap",
                      choices = c("TRUE","FALSE"),
                      selected = "TRUE"
            
          ),
          selectInput("size",
                      "Front Size",
                      choices = c(1,2,3,4,5,6),
                      selected = 3
                      
          ),
          width=2
        ),
       
      mainPanel(
        fluidRow(
          splitLayout(cellWidths = c("50%","50%"), plotlyOutput("Map1"), plotlyOutput("Map2"))
          
        ), fluidRow(plotlyOutput("Scatter"))
      )
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
  
  output$SLR = renderPlotly({
    dt1 = switch(input$V1, 
                  "HPI"=dt[,c(3)], 
                  "Personal_Income"=dt[,c(4)],
                  "Poverty_Percentage"=dt[,c(5)], 
                  "Population"=dt[,c(6)],
                  "HighSchoolLess"=dt[,c(7)], 
                  "HighSchoolOnly"=dt[,c(8)],
                  "SomeCollege"=dt[,c(9)], 
                  "BachelorAndHigher"=dt[,c(10)],
                  "Unemployment_Rate"=dt[,c(11)]
    )
    
    dt2 = switch(input$V2, 
                 "HPI"=dt[,c(3)], 
                 "Personal_Income"=dt[,c(4)],
                 "Poverty_Percentage"=dt[,c(5)], 
                 "Population"=dt[,c(6)],
                 "HighSchoolLess"=dt[,c(7)], 
                 "HighSchoolOnly"=dt[,c(8)],
                 "SomeCollege"=dt[,c(9)], 
                 "BachelorAndHigher"=dt[,c(10)],
                 "Unemployment_Rate"=dt[,c(11)]
    )
    
    dt_temp = as.data.frame(cbind(dt1,dt2))
    colnames(dt_temp)=c("V1","V2")
    fit = lm(V2~V1, data = dt_temp)
    dt_temp$predicted = predict(fit)
    dt_temp$residuals = residuals(fit)
    
    p = ggplot(dt_temp, aes(x = V1, y = V2)) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      geom_segment(aes(xend = V1, yend = predicted), alpha=0.2) +
      geom_point() +
      geom_point(aes(y = predicted), shape = 1) +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$Map1 = renderPlotly({
    data = switch(input$var1, 
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
    var = input$var1
    
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
    
    cnames = aggregate(cbind(long,lat)~subregion, data=State_county,
                       FUN=function(x)mean(range(x)))
    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    
    var1 = plot_dt[,var]
    p=ggplot(data = inputState, mapping = aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.3) + 
      geom_polygon(fill = "white")+ 
      geom_polygon(data = plot_dt, aes(fill = var1), color = "grey") +
      scale_fill_distiller(palette = "Blues", trans = "reverse")+
      geom_text(data=cnames, aes(long, lat, label = subregion, group=subregion), color = "black", size=as.numeric(input$size),check_overlap = input$overlap)+
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      ditch_the_axes 
    
    ggplotly(p) %>%
      layout(autosize = F, width = 600, height = 600, margin = m) %>% 
      colorbar(title = var)
    
  })
  
  output$Map2 = renderPlotly({
    data = switch(input$var2, 
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
    var = input$var2
    
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
    
    cnames = aggregate(cbind(long,lat)~subregion, data=State_county,
                       FUN=function(x)mean(range(x)))
    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    
    var2 = plot_dt[,var]
    
    p=ggplot(data = inputState, mapping = aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.3) + 
      geom_polygon(fill = "white")+ 
      geom_polygon(data = plot_dt, aes(fill = var2), color = "grey") +
      scale_fill_distiller(palette = "Blues", trans = "reverse")+
      geom_text(data=cnames, aes(long, lat, label = subregion, group=subregion), color = "black", size=as.numeric(input$size),check_overlap = input$overlap)+
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      ditch_the_axes
    
    ggplotly(p) %>%
      layout(autosize = F, width = 600, height = 600, margin = m) %>% 
      colorbar(title = var)
    
  })
  
  output$Scatter = renderPlotly({
    Variable1 = switch(input$var1, 
                  "HPI"=dt[which(dt$State_Name == input$St2),c(3)], 
                  "Personal_Income"=dt[which(dt$State_Name == input$St2),c(4)],
                  "Poverty_Percentage"=dt[which(dt$State_Name == input$St2),c(5)], 
                  "Population"=dt[which(dt$State_Name == input$St2),c(6)],
                  "HighSchoolLess"=dt[which(dt$State_Name == input$St2),c(7)], 
                  "HighSchoolOnly"=dt[which(dt$State_Name == input$St2),c(8)],
                  "SomeCollege"=dt[which(dt$State_Name == input$St2),c(9)], 
                  "BachelorAndHigher"=dt[which(dt$State_Name == input$St2),c(10)],
                  "Unemployment_Rate"=dt[which(dt$State_Name == input$St2),c(11)]
    )
    Variable2 = switch(input$var2, 
                  "HPI"=dt[which(dt$State_Name == input$St2),c(3)], 
                  "Personal_Income"=dt[which(dt$State_Name == input$St2),c(4)],
                  "Poverty_Percentage"=dt[which(dt$State_Name == input$St2),c(5)], 
                  "Population"=dt[which(dt$State_Name == input$St2),c(6)],
                  "HighSchoolLess"=dt[which(dt$State_Name == input$St2),c(7)], 
                  "HighSchoolOnly"=dt[which(dt$State_Name == input$St2),c(8)],
                  "SomeCollege"=dt[which(dt$State_Name == input$St2),c(9)], 
                  "BachelorAndHigher"=dt[which(dt$State_Name == input$St2),c(10)],
                  "Unemployment_Rate"=dt[which(dt$State_Name == input$St2),c(11)]
    )
    
    temp2 = as.data.frame(cbind(Variable1, Variable2)) 
    colnames(temp2)=c("V1","V2")
    
    fit = lm(V2~V1, data = temp2)
    temp2$predicted = predict(fit)
    temp2$residuals = residuals(fit)
    
    
    p = ggplot(temp2, aes(x = V1, y = V2)) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      geom_segment(aes(xend = V1, yend = predicted), alpha=0.2) +
      geom_point(aes(color = residuals)) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red") +
      guides (color = FALSE) + 
      geom_point(aes(y = predicted), shape = 1) +
      theme_bw()
    
    p
    
    ggplotly(p)
  })
}

# Run app ----
shinyApp(ui=ui, server)