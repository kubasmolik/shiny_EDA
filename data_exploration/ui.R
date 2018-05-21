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
                        id = "file_input",
                        label = "Choose .RData file",
                        accept = "Rdata"
                    ),
                    
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
                        inputId = ""
                    )
                    
                )
            )
        )
    )
    
    
    
    
)
