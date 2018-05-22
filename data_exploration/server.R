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
shinyServer(function(input, output, session) {
   
    # df <- reactive({
    #     req(input$file_input, input$separator)
    #     data.table::fread(file = input$file_input$datapath,
    #                       stringsAsFactors = F,
    #                       sep = input$separator)
    #     })
    df <- eventReactive(input$choice, {
            req(input$file_input, input$separator)
            df_raw <- data.table::fread(file = input$file_input$datapath,
                              stringsAsFactors = F,
                              sep = input$separator)
            vars <- names(df_raw)
            updateSelectInput(session,
                              inputId = "target",
                              label = "Select target variable",
                              choices = vars)
            updateSelectInput(session,
                              inputId = "var_name",
                              label = "Select predictor variable",
                              choices = vars)
            df_raw
        
    })
    
    output$dane <- renderDataTable({
        
        df()
    })
  
})
