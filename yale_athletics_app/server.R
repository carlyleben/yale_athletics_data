#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
    # load in data so it can be accessed in UI
    in_athlete_data <- readRDS("data/stacked_athletics_data.rds")

    # reactively filter data
    filtered_data <- reactive({
      
      # make copy of data
      sub_athlete_data <- in_athlete_data
      
      # if filters are present, apply them
      if (!is.null(input$select_name))     {sub_athlete_data <- sub_athlete_data %>% filter(Name %in% input$select_name)}
      if (!is.null(input$select_position)) {sub_athlete_data <- sub_athlete_data %>% filter(Position %in% input$select_position)}
      if (!is.null(input$select_date))     {sub_athlete_data <- sub_athlete_data %>% filter(Date >= input$select_date[1], Date <= input$select_date[2])}
      if (!is.null(input$select_metric))   {sub_athlete_data <- sub_athlete_data %>% filter(variable %in% input$select_metric)}
      
      # now subset columns and reshape wider
      out_data <- sub_athlete_data %>% select(Name, Date, Position, variable, value) %>% 
                                       pivot_wider(names_from = variable, values_from = value)
      
      # return
      out_data
    })

    # create DT
    output$display_table <- renderDT({datatable(filtered_data())})
}
