#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinythemes)


# reactive parameter tabs

# Trial 1
t1_param_tab <- tabsetPanel(
    id = "t1param",
    type = "hidden",
    tabPanel("Standard Error", 
             numericInput("std_err1", "Standard error", min = 0, value = 1)
    ),
    tabPanel("Lower 95% confidence limit", 
             numericInput("l951", "Lower 95% confidence limit", value = 1)
    )
)

# Trial 2
t2_param_tab <- tabsetPanel(
  id = "t2param",
  type = "hidden",
  tabPanel("Standard Error", 
           numericInput("std_err2", "Standard error", min = 0, value = NA)
  ),
  tabPanel("Lower 95% confidence limit", 
           numericInput("l952", "Lower 95% confidence limit", value = NA)
  )
)

# Trial 3
t3_param_tab <- tabsetPanel(
  id = "t3param",
  type = "hidden",
  tabPanel("Standard Error", 
           numericInput("std_err3", "Standard error", min = 0, value = NA)
  ),
  tabPanel("Lower 95% confidence limit", 
           numericInput("l953", "Lower 95% confidence limit", value = NA)
  )
)


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
  
  # Application title
  titlePanel("Acceptability curve estimation (ACE)"),
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h1("Trial 1"),
      # Trial 1
      textInput("trialname1", "Name", value = "Trial 1"),
      numericInput("mean1", "Mean", value = 0),
      
      selectInput("t1param_choice", "Select error measurement", 
                  choices = c("Standard Error", "Lower 95% confidence limit")
      ), 
      t1_param_tab,
      numericInput("df1", "Degrees of freedom (optional)", min = 1, value = NA),
      
      # Comparison values
      h1("Acceptability thresholds"),
      numericInput("comp_val1", "Acceptability threshold value 1", value = NA), 
      numericInput("comp_val2", "Acceptability threshold value 2", value = NA), 
      numericInput("comp_val3", "Acceptability threshold value 3", value = NA),
      # Trial 2
      h1("Trial 2"),
      textInput("trialname2", "Name", value = "Trial 2"),
      numericInput("mean2", "Mean", value = NA),
      
      selectInput("t2param_choice", "Error measurement", 
                  choices = c("Standard Error", "Lower 95% confidence limit")
      ), 
      t2_param_tab,
      numericInput("df2", "Degrees of freedom (optional)", min = 1, value = NA),
      # Trial 3
      h1("Trial 3"),
      textInput("trialname3", "Name", value = "Trial 3"),
      numericInput("mean3", "Mean", value = NA),
      
      selectInput("t3param_choice", "Error measurement", 
                  choices = c("Standard Error", "Lower 95% confidence limit")
      ), 
      t3_param_tab,
      numericInput("df3", "Degrees of freedom (optional)", min = 1, value = NA)

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs", 
        tabPanel("Intro", 
                 htmlOutput("instruction_text")),
        tabPanel("ACE", 
                 plotlyOutput("acceptability_curve"),
                 br(),
                 br(),
                 span(textOutput("table_title1"), style="font-size: 20px"),
                 br(),
                 tableOutput("acceptability_thresholds"),
                 br(),
                 span(textOutput("table_title"), style="font-size: 20px"),
                 br(),
                 tableOutput("acceptability_values")
                 )

        
      )
      
 
    )
  )
))
