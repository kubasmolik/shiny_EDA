#------------------------------------------------------------------------------#
####                          SERVER SCRIPT                                 ####
#------------------------------------------------------------------------------#

library(shiny)

#------------------------------------------------------------------------------#
####                          Define SERVER                                 ####
#------------------------------------------------------------------------------#

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
        
        ## fix the names
        names(df_temp) <- c(input$var_name, input$target)
        
        df_temp
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
  
})
