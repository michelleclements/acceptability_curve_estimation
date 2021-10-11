#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Acceptability curve estimation (ACE)"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          
          radioButtons("Ntrials", "Number of ACE to create",
                       choices = c(1, 2, 3), selected = 1),
          
     
        numericInput("point_est", "Point estimate", value = 0),
        numericInput("std_err", "Standard Error", value = 1),
        numericInput("df", "Degrees of freedom", value = 10000)

        
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("acceptability_curve")
        )
    )
))
