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
        
        # values 1
        point_est1 <- input$point_est1
        std_err1 <- input$std_err1
        df1 <- ifelse(!is.na(input$df1), input$df1, 1000000)
        
        # values 2
        point_est2 <- ifelse(!is.na(input$point_est2) & !is.na(input$std_err2), input$point_est2, NA)
        std_err2 <- ifelse(!is.na(input$point_est2) & !is.na(input$std_err2), input$std_err2, NA)
        df2 <- ifelse(!is.na(input$point_est2) & !is.na(input$std_err2) & !is.na(input$df2), input$df2, 1000000)
        
        # values 3
        point_est3 <- ifelse(!is.na(input$point_est3) & !is.na(input$std_err3), input$point_est3, NA)
        std_err3 <- ifelse(!is.na(input$point_est3) & !is.na(input$std_err3), input$std_err3, NA)
        df3 <- ifelse(!is.na(input$point_est3) & !is.na(input$std_err3) & !is.na(input$df3), input$df3, 1000000)
        
        # get estimates
        mutiplier <- 3.5
        lower <- min(point_est1 - mutiplier*std_err1, 
                     point_est2 - mutiplier*std_err2, 
                     point_est3 - mutiplier*std_err3, 
                     input$comp_val,
                     na.rm = T)
                     
        upper <- max(point_est1 + mutiplier*std_err1, 
                     point_est2 + mutiplier*std_err2, 
                     point_est3 + mutiplier*std_err3, 
                     input$comp_val,
                     na.rm = T)
        
        params <- tibble(
            est = c("ACE 1", "ACE 2", "ACE 3"),
            point_est = c(point_est1, point_est2, point_est3), 
            std_err = c(std_err1, std_err2, std_err3), 
            df = c(df1, df2, df3)
        ) %>% 
            drop_na()
        
        data <- crossing(est = params$est,  
                         threshold = seq(lower, upper, length.out = 1000)) %>% 
            left_join(params, by = "est") %>% 
            mutate(crit_val = (point_est - threshold)/ std_err,
                   prob = pt(crit_val, df))
        
        
        p1 <- ggplot(data, 
               aes(x = threshold, 
                   y = prob, 
                   group = est, 
                  linetype = est)) +
            geom_line(colour = "gray50", size = 1) +
            scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
            scale_y_continuous(labels = percent_format(accuracy = 1)) +
            theme(legend.position = "bottom", legend.title = element_blank()) +
            labs(x = "Acceptability threshold", y = "ACE value")
        

        # add on vertical line
        if(!is.na(input$comp_val)){
            p1 <- p1 + geom_vline(aes(xintercept = input$comp_val), 
                                  linetype = "dashed", colour = "firebrick3", size = 0.8)
        }else{
            p1 <- p1}

        p1
    })
    
    
    output$acceptability_values <-renderTable({
        
        # values 1
        point_est1 <- input$point_est1
        std_err1 <- input$std_err1
        df1 <- ifelse(!is.na(input$df1), input$df1, 1000000)
        
        # values 2
        point_est2 <- ifelse(!is.na(input$point_est2) & !is.na(input$std_err2), input$point_est2, NA)
        std_err2 <- ifelse(!is.na(input$point_est2) & !is.na(input$std_err2), input$std_err2, NA)
        df2 <- ifelse(!is.na(input$point_est2) & !is.na(input$std_err2) & !is.na(input$df2), input$df2, 1000000)
        
        # values 3
        point_est3 <- ifelse(!is.na(input$point_est3) & !is.na(input$std_err3), input$point_est3, NA)
        std_err3 <- ifelse(!is.na(input$point_est3) & !is.na(input$std_err3), input$std_err3, NA)
        df3 <- ifelse(!is.na(input$point_est3) & !is.na(input$std_err3) & !is.na(input$df3), input$df3, 1000000)
        
        params <- tibble(
            trial = c("Trial 1", "Trial 2", "Trial 3"),
            threshold = rep(input$comp_val, 3),
            point_est = c(point_est1, point_est2, point_est3), 
            std_err = c(std_err1, std_err2, std_err3), 
            df = c(df1, df2, df3)
        ) %>% 
            # get rid of any rows with missing values
            drop_na()
        
        data <- params %>% 
            mutate(crit_val = (point_est - threshold)/ std_err,
                   `ACE value` = pt(crit_val, df)) %>% 
            select(trial, threshold, `ACE value`)
        
        if(!is.na(input$comp_val)){
            data
        }else{
            NULL}
        

        
    })
    
})