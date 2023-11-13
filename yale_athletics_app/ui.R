library(shiny)
library(bslib)

# define ui
fluidPage(

    theme = bs_theme(bootswatch = "darkly"),
    
    # create navigation bar layout
    navbarPage(title = "Yale Football: Strength and Conditioning",
               
               # create home tab
               tabPanel("Home",
                          # Sidebar with a slider input for number of bins
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("bins",
                                          "Number of bins:",
                                          min = 1,
                                          max = 50,
                                          value = 30)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotOutput("distPlot")
                            )
                          )
                          
                        ) # close home tab
               ) # close navbar
) # close ui
