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
    in_athlete_data <- readRDS("data/stacked_athletics_data2.rds")

    # reactively filter data
    filtered_data <- reactive({
      
      # make copy of data
      sub_athlete_data <- in_athlete_data
      
      # if filters are present, apply them
      if (!is.null(input$select_name))     {sub_athlete_data <- sub_athlete_data %>% dplyr::filter(Name %in% input$select_name)}
      if (!is.null(input$select_position)) {sub_athlete_data <- sub_athlete_data %>% dplyr::filter(Position %in% input$select_position)}
      if (!is.null(input$select_date))     {sub_athlete_data <- sub_athlete_data %>% dplyr::filter(Date >= input$select_date[1], Date <= input$select_date[2])}
      if (!is.null(input$select_metric))   {sub_athlete_data <- sub_athlete_data %>% dplyr::filter(variable %in% input$select_metric)}
      
      # add new ID column
      sub_athlete_data <- sub_athlete_data %>% mutate(new_id = data.table::rowid(Name, Date, Position, Session.Id, variable))
      
      # now subset columns and reshape wider
      out_data <- sub_athlete_data %>% dplyr::select(Name, Date, Session.Id, Position, new_id, variable, value) %>% 
                                       pivot_wider(names_from = variable, values_from = value)

      
      # return
      out_data
    })

    # create DT
    output$display_table <- renderDT({
      
      # set up base table
      base_table <- datatable(filtered_data())
      
      # loop over columns to add color
      for (col in input$select_metric) {
        
        # create breaks
        breaks <- quantile(as.numeric(unlist(filtered_data()[,col])), seq(0, 1, 0.25), na.rm = T)
        
        # add color for this column
        base_table <- base_table %>% formatStyle(col, backgroundColor = styleInterval(unname(breaks), c('#f5f5f5', '#f7dcc9', '#f9ed95', '#c9fb64', '#62fd33', '#00ff37')))
      }
      
      #return
      base_table
      
      })
}
