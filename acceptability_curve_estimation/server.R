#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)

# preferences
theme_set(theme_bw(base_size = 16))
theme_update(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), 
             strip.background=element_rect(fill="white"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    output$acceptability_curve <- renderPlot({
        
        # get estimates
        lower <- input$point_est -3.5*input$std_err
        upper <- input$point_est + 3.5*input$std_err
        
        
        threshold <- seq(lower, upper, length.out = 100)
        
        crit_val <- (input$point_est - threshold)/ input$std_err
        
        prob <- pt(crit_val, input$df)
        
        
        data_frame <- tibble(threshold, crit_val, prob)
        
        
        
        ggplot(data_frame, 
               aes(x = threshold, 
                   y = prob)) +
            geom_line() +
            scale_y_continuous(labels = percent_format(accuracy = 1))
        
        
    })
    
    

})
