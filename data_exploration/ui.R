#


library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(shiny)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
        title = "Exploratory data analysis", 
        
        ## Load data
        tabPanel(
            title = "Load data",
            sidebarLayout(
                
                ## sidebar
                sidebarPanel(
                    
                    ## load data
                    fileInput(
                        inputId = "file_input",
                        label = "Choose .CSV file",
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                    ),
                    
                    ## input the separator sign
                    textInput(
                        inputId = "separator",
                        label = "Put in the separator sign",
                        value = ","
                    ),
                    
                    actionButton("choice", "incorporate external information"),
                    
                    ## select Y variable
                    selectInput(
                        inputId = "target",
                        label = "Select target variable",
                        choices = names(df)
                    ),
                    
                    ## select X variable
                    selectInput(
                        inputId = "var_name",
                        label = "Select predictor variable",
                        choices = names(df)
                    ),
                    
                    ## select predictor type
                    radioButtons(
                        inputId = "target_type",
                        label = "Select target type",
                        choices = c("binary", "numeric")
                    )
                    
                ),
                mainPanel(
                    dataTableOutput("dane")
                    
                )
            )
        )
    )
    
    
    
    
)
