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
          
            numericInput("point_est1", "Trial 1: Point estimate", value = 0),
            numericInput("std_err1", "Trial 1: Standard Error", value = 1),
            numericInput("df1", "Trial 1: Degrees of freedom (optional)", value = NA), 
            
            numericInput("point_est2", "Trial 2: Point estimate", value = NA),
            numericInput("std_err2", "Trial 2: Standard Error", value = NA),
            numericInput("df2", "Trial 2: Degrees of freedom (optional)", value = NA), 
            
            numericInput("point_est3", "Trial 3: Point estimate", value = NA),
            numericInput("std_err3", "Trial 3: Standard Error", value = NA),
            numericInput("df3", "Trial 3: Degrees of freedom (optional)", value = NA), 
            
            numericInput("comp_val", "Comparison threshold value", value = NA)
        
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("acceptability_curve"),
            tableOutput("acceptability_values")
        )
    )
))
