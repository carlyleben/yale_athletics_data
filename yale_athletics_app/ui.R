library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyverse)

# define ui
fluidPage(

    theme = bs_theme(bootswatch = "flatly"),
    
    # create navigation bar layout
    navbarPage(title = "Yale Football: Strength and Conditioning",
               
               # create home tab
               tabPanel("Home",
                          # Sidebar with a slider input for number of bins
                          sidebarLayout(
                            sidebarPanel(
                              h4("Table Filters"),
                              
                              # name selector 
                              selectInput(
                                "select_name", 
                                "Select athlete name:",
                                unique(in_athlete_data$Name),
                                multiple = TRUE),
                              
                              # position selector
                              selectInput(
                                "select_position", 
                                "Select athlete position:",
                                unique(in_athlete_data$Position),
                                multiple = TRUE),
                              
                              # date selector
                              dateRangeInput(
                                "select_date",
                                "Select session date range:",
                                start = min(in_athlete_data$Date, na.rm = T),
                                end = max(in_athlete_data$Date, na.rm = T),
                                min = min(in_athlete_data$Date, na.rm = T),
                                max = max(in_athlete_data$Date, na.rm = T)
                              ),
                              
                              # metric selector
                              selectInput(
                                "select_metric",
                                "Select the metrics you want to see:",
                                unique(in_athlete_data$variable),
                                selected = c("System.Weight", "Jump.Height", "Jump.Momentum"),
                                multiple = TRUE)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              
                              # display DT
                              DTOutput("display_table")
                            )
                          )
                          
                        ) # close home tab
               ) # close navbar
) # close ui
