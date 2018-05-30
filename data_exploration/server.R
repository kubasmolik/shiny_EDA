#------------------------------------------------------------------------------#
####                          SERVER SCRIPT                                 ####
#------------------------------------------------------------------------------#

library(shiny)

#------------------------------------------------------------------------------#
####                          Define SERVER                                 ####
#------------------------------------------------------------------------------#

# load("./dane.RData")
# source("./data_exploration/plot_histogram.R")
source("./plot_histogram.R")

shinyServer(function(input, output, session) {
   
    #### REACTIVES ####
    
    ## df_raw
    df_raw <- eventReactive(input$upload, {
            req(input$file_input, input$separator)
            df_0 <- data.table::fread(file = input$file_input$datapath,
                              stringsAsFactors = F,
                              sep = input$separator)
            vars <- names(df_0)
            updateSelectInput(session,
                              inputId = "target",
                              label = "Select target variable",
                              choices = vars)
            updateSelectInput(session,
                              inputId = "var_name",
                              label = "Select predictor variable",
                              choices = vars)
            df_0
    })
    
    ## df_base
    df_base <- reactive({
        req(input$var_name, input$target)
        
        df_raw() %>%
            select(rlang::UQ(as.name(input$var_name)), 
                   rlang::UQ(as.name(input$target))) 
    })
    
    ## df
    df <- reactive({
        # req(input$log_dummy, input$outlier_dummy, input$outlier_def)
        
        ## calculate min to enable applying logarithm
        df_base() %>%
            select(rlang::UQ(as.name(input$var_name))) %>%
            min(na.rm = T) -> min_x
        
        df_temp <- df_base()
        
        ## change names to simplify code
        names(df_temp) <- c("x", "y")
        df_temp$x <- as.numeric(df_temp$x)
        
        ## if log_dummy == T then apply logarithm
        if(input$log_dummy){
            if(min_x <= 0){
                df_temp %>%
                    mutate(x = log(x + min_x + 1)) %>% 
                    select(x, y) -> df_temp
            } else {
                df_temp %>%
                    mutate(x = log(x)) %>% 
                    select(x, y) -> df_temp
            }
        }
        
        ## if outlier_dummy == T then remove outliers according to definition
        if(input$outlier_dummy){
            df_temp %>%
                mutate(x_scaled = as.numeric(scale(x))) %>%
                filter(abs(x_scaled) <= input$outlier_def) %>%
                select(- x_scaled) -> df_temp
        }
        
        #####
        
        # if(input$target_type == "numeric") {
        #     
        #     ## calculate min to enable applying logarithm
        #     df_base() %>%
        #         select(rlang::UQ(as.name(input$target))) %>%
        #         min(na.rm = T) -> min_y
        #     
        #     df_temp$y <- as.numeric(df_temp$y)
        #     
        #     ## if log_dummy_tar == T then apply logarithm
        #     if(input$log_dummy_tar){
        #         if(min_y <= 0){
        #             df_temp %>%
        #                 mutate(y = log(y + min_y + 1)) %>% 
        #                 select(x, y) -> df_temp
        #         } else {
        #             df_temp %>%
        #                 mutate(y = log(y)) %>% 
        #                 select(x, y) -> df_temp
        #         }
        #     }
        #     
        #     ## if outlier_dummy_tar == T then remove outliers according to definition
        #     if(input$outlier_dummy_tar){
        #         df_temp %>%
        #             mutate(y_scaled = as.numeric(scale(y))) %>%
        #             filter(abs(y_scaled) <= 3) %>%
        #             select(- y_scaled) -> df_temp
        #     }
        #     
        # }
        
        #####
        
        ## fix the names
        names(df_temp) <- c(input$var_name, input$target)
        
        df_temp
    })
    
    df_y <- reactive({

        if(input$target_type == "numeric") {

            ## calculate min to enable applying logarithm
            df() %>%
                select(rlang::UQ(as.name(input$target))) %>%
                min(na.rm = T) -> min_y

            df_temp <- df()

            ## change names to simplify code
            names(df_temp) <- c("x", "y")
            df_temp$x <- as.numeric(df_temp$x)
            df_temp$y <- as.numeric(df_temp$y)

            ## if log_dummy_tar == T then apply logarithm
            if(input$log_dummy_tar){
                if(min_y <= 0){
                    df_temp %>%
                        mutate(y = log(y + min_y + 1)) %>%
                        select(x, y) -> df_temp
                } else {
                    df_temp %>%
                        mutate(y = log(y)) %>%
                        select(x, y) -> df_temp
                }
            }

            ## if outlier_dummy_tar == T then remove outliers according to definition
            if(input$outlier_dummy_tar){
                df_temp %>%
                    mutate(y_scaled = as.numeric(scale(y))) %>%
                    filter(abs(y_scaled) <= 3) %>%
                    select(- y_scaled) -> df_temp
            }

            ## fix the names
            names(df_temp) <- c(input$var_name, input$target)

            df_temp
        } else {
            df()
        }

    })
    
    
    #### OUTPUTS ####
    
    output$dane <- renderDataTable({
        df_raw() %>% head(100)
    })
    
    output$summary <- renderPrint({
        df() %>%
            select(rlang::UQ(as.name(input$var_name))) %>%
            DescTools::Desc(plotit = F)
    })
    
    output$hist_plot <- renderPlotly({
        
        df() %>%
            select(rlang::UQ(as.name(input$var_name))) %>%
            summarise_all(mean) %>%
            .[[1]] -> mean_x
        
        df() %>%
            select(rlang::UQ(as.name(input$var_name))) %>%
            summarise_all(sd) %>%
            .[[1]] -> sd_x
        
        plot_histogram(df = df(), 
                       var_name = input$var_name, 
                       bins_num = input$bins, 
                       mean_x = mean_x, 
                       sd_x = sd_x, 
                       title = paste0("Histogram of ", input$var_name)
                       ) -> plot_1
        plotly::ggplotly(plot_1)
    })
  
    output$box_plot <- renderPlotly({
        
        df() %>%
            select(rlang::UQ(as.name(input$var_name))) %>%
            ggplot(aes_string(x = factor(0), y = input$var_name)) +
            geom_boxplot(fill = RColorBrewer::brewer.pal(n = 3, name = "Set1")[2],
                         na.rm = TRUE, 
                         outlier.color = "red",
                         outlier.fill = "red") +
            scale_x_discrete(breaks = NULL) +
            labs(title = paste0("Boxplot of ", input$var_name)) +
            xlab(NULL) +
            coord_fixed(ratio = 0.05) +
            theme_bw() -> plot_2
        
        plotly::ggplotly(plot_2)
    })
    
    output$norm_test <- renderPrint({
        
        df() %>%
            select(rlang::UQ(as.name(input$var_name))) %>%
            .[[1]] %>%
            nortest::lillie.test()
    })
    
    output$scatter_plot <- renderPlotly({
        
        df_y() %>%
        
        ## define plot
        ggplot(aes_string(x = input$var_name, y = input$target)) +
            geom_point(fill = "black", alpha = 0.5) + 
            geom_smooth(color = "red", fill = "darkblue") +
            
            ## layout
            labs(x = paste(input$var_name),
                 y = paste(input$target),
                 title = paste0(input$target, " vs. ", input$var_name)) +
            theme_bw() -> plot_2
        
        plotly::ggplotly(plot_2)
    })
    
    output$correlation <- renderText({
        
        df_y() %>%
            cor() %>%
            .[1,2] -> cor_coef
        
        paste0("Correlation coefficient = ",
               round(cor_coef, digits = 3))
    })
    
})
