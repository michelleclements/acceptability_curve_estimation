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
library(plotly)

# preferences
theme_set(theme_bw(base_size = 16))
theme_update(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), 
             strip.background=element_rect(fill="white"))


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # trial 1
    observeEvent(input$t1param_choice, {
        updateTabsetPanel(inputId = "t1param", selected = input$t1param_choice)
    }) 
    
    # trial 2
    observeEvent(input$t2param_choice, {
        updateTabsetPanel(inputId = "t2param", selected = input$t2param_choice)
    }) 
    
    
    # trial 3
    observeEvent(input$t3param_choice, {
        updateTabsetPanel(inputId = "t3param", selected = input$t3param_choice)
    }) 
    
    
    # means
    mean1 <- reactive(input$mean1)
    mean2 <- reactive(input$mean2)
    mean3 <- reactive(input$mean2)
    
    # degrees of freedom
    df1 <-  reactive(ifelse(!is.na(input$df1), input$df1, 1000000))
    df2 <-  reactive(ifelse(!is.na(input$mean2) & !is.na(input$df2), input$df2, 1000000))
    df3 <-  reactive(ifelse(!is.na(input$mean3) & !is.na(input$df3), input$df3, 1000000))
    
    # standard errors
    std_err1 <-  reactive({
      if(input$t1param_choice == "Standard Error"){
        
        validate(need(input$std_err1 >0, "Trial 1 standard error needs to be positive"))

        input$std_err1
        
      }else if(input$t1param_choice == "Lower 95% confidence limit" & !is.na(input$l951)){
        
        validate(need(input$l951 < mean1(), "Trial 1 lower 95% confidence limit needs to be less than the mean"))
        (mean1() - input$l951) /  qt(0.05/2, df = df1(), lower.tail = F)
      }else{
        NA
      }
    })
      
      
    std_err2 <-  reactive({
      if(input$t2param_choice == "Standard Error" & !is.na(input$std_err2)){
        
        validate(need(input$std_err2 >0, "Trial 2 standard error needs to be positive"))
        
        input$std_err2
        
      }else if(input$t2param_choice == "Lower 95% confidence limit" & !is.na(input$l952)){
        
        validate(need(input$l952 < mean2(), "Trial 2 lower 95% confidence limit needs to be less than the mean"))
        (mean2() - input$l952) /  qt(0.05/2, df = df2(), lower.tail = F)
      }else{
        NA
      }
    })
    
    std_err3 <-  reactive({
      if(input$t3param_choice == "Standard Error" & !is.na(input$std_err3)){
        
        validate(need(input$std_err3 >0, "Trial 3 standard error needs to be positive"))
        
        input$std_err3
        
      }else if(input$t3param_choice == "Lower 95% confidence limit" & !is.na(input$l953)){
        
        validate(need(input$l953 < mean3(), "Trial 3 lower 95% confidence limit needs to be less than the mean"))
        (mean3() - input$l953) /  qt(0.05/2, df = df3(), lower.tail = F)
      }else{
        NA
      }
    })
    

    output$acceptability_curve <- renderPlotly({
        
        # get estimates
        mutiplier <- 3.5
        lower <- min(mean1() - mutiplier*std_err1(), 
                     mean2() - mutiplier*std_err2(), 
                     mean3() - mutiplier*std_err3(), 
                     input$comp_val1,
                     input$comp_val2,
                     input$comp_val3,
                     na.rm = T)
                     
        upper <- max(mean1() + mutiplier*std_err1(), 
                     mean2() + mutiplier*std_err2(), 
                     mean3() + mutiplier*std_err3(), 
                     input$comp_val1,
                     input$comp_val2,
                     input$comp_val3,
                     na.rm = T)
        
        params <- tibble(
            est = c(input$trialname1, input$trialname2, input$trialname3),
            mean = c(input$mean1, input$mean2, input$mean3), 
            std_err = c(std_err1(), std_err2(), std_err3()), 
            df = c(df1(), df2(), df3())
        ) %>% 
            drop_na()
        
        data <- crossing(est = params$est,  
                         threshold = seq(lower, upper, length.out = 1000)) %>% 
          left_join(params, by = "est") %>% 
          mutate(crit_val = (mean - threshold)/ std_err,
                 prob = pt(crit_val, df)) %>% 
          rename(Trial = est) %>% 
          # get in trial order
          mutate(Trial = factor(Trial, levels = c(input$trialname1, input$trialname2, input$trialname3))) %>%
          arrange(Trial)
        
        
        # setting rounding for text
        span <- upper - lower
        dec_places <- floor(log10(span)*-1) + 4
        dec_places <- ifelse(dec_places <0 , 0, dec_places)
        
        
        p1 <- ggplot(data, 
                     aes(x = threshold, 
                         y = prob,
                         group = Trial,
                         linetype = Trial,
                         text = paste(Trial, "\nACE threshold:", round(threshold, dec_places), 
                                      "\nACE value:", round(prob*100, 1), "%"
                                      
                         ))) +
          geom_line(colour = "gray50", size = 1) +
          scale_linetype_manual(values = c("solid", "longdash", "dotted")) +
          scale_y_continuous(labels = percent_format(accuracy = 1)) +
          theme(legend.position = "bottom", legend.title = element_blank(),
                legend.key.width = unit(2, 'cm')) +
          labs(x = "Acceptability threshold", y = "Acceptability value")

        # add on vertical line
        if(!is.na(input$comp_val1)){
            p1 <- p1 + geom_vline(aes(xintercept = input$comp_val1), 
                                  linetype = "dashed", colour = "firebrick3", size = 0.8)
        }else{
            p1 <- p1}
        
        if(!is.na(input$comp_val2)){
          p1 <- p1 + geom_vline(aes(xintercept = input$comp_val2), 
                                linetype = "dashed", colour = "blue", size = 0.8)
        }else{
          p1 <- p1}
        
        if(!is.na(input$comp_val3)){
          p1 <- p1 + geom_vline(aes(xintercept = input$comp_val3), 
                                linetype = "dashed", colour = "green3", size = 0.8)
        }else{
          p1 <- p1}

        ggplotly(p1,  tooltip = "text")
   
    })
    
    # acceptabiilty thresholds for different accepatbilty values
    output$acceptability_thresholds <-renderTable({


      params1 <- tibble(
        trial = c(input$trialname1, input$trialname2, input$trialname3),
        mean = c(input$mean1, input$mean2, input$mean3),
        std_err = c(std_err1(), std_err2(), std_err3()),
        df = c(df1(), df2(), df3())
      ) %>%
        crossing(tibble(
          acc_val = c(0.025, 0.5, 0.975),
          `Acceptability value` = c("2.5%", "50%","97.5%")
        )) %>%
        # get rid of any rows with missing values
        drop_na() %>%
        # get in trial order
        mutate(trial = factor(trial, levels = c(input$trialname1, input$trialname2, input$trialname3) )) %>%
        arrange(trial)

      data1 <- params1 %>%
        mutate(Threshold = mean - std_err * qt(acc_val, df))

      data_wide1  <- data1 %>%
        select(trial, Threshold, `Acceptability value`) %>%
        pivot_wider(names_from = "trial", values_from =  "Threshold")

    })

    output$table_title1 <- renderText({
        "\nAcceptability thresholds for standard acceptability values\n"
      
    })
    
    # acceptability values for user inputted acceptabilty values
    output$acceptability_values <-renderTable({
        
        
      params <- tibble(
        trial = c(input$trialname1, input$trialname2, input$trialname3),
        mean = c(input$mean1, input$mean2, input$mean3), 
        std_err = c(std_err1(), std_err2(), std_err3()), 
        df = c(df1(), df2(), df3())
      ) %>% 
        crossing(tibble(
          threshold = c(input$comp_val3, input$comp_val2, input$comp_val1)
        )) %>%         
        # get rid of any rows with missing values
        drop_na()
      
      data <- params %>% 
        mutate(crit_val = (mean - threshold)/ std_err,
               `Acceptability value` = paste0(round(pt(crit_val, df)*100, 0), "%")) %>% 
        select(Trial = trial, Threshold = threshold, `Acceptability value`) %>% 
        # get in trial order
        mutate(Trial = factor(Trial, levels = c(input$trialname1, input$trialname2, input$trialname3))) %>%
        arrange(Trial)
      
      data_wide  <- data %>% 
        pivot_wider(names_from = "Trial", values_from = `Acceptability value`) %>% 
        rename(`Acceptability threshold` = Threshold)
      
      if(is.na(input$comp_val1) & is.na(input$comp_val2) & is.na(input$comp_val3)){
        NULL
      }else{
        data_wide}
    })
    
    output$table_title <- renderText({
      if(is.na(input$comp_val1) & is.na(input$comp_val2) & is.na(input$comp_val3)){
        NULL
      }else{
        "\nAcceptability values for selected acceptability thresholds\n"}
      
    })
    
    
    output$instruction_text <- renderUI({
      HTML(markdown::markdownToHTML('text.md'))
    })
    
    
})