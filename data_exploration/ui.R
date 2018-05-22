#
## check for packages
if(!("DescTools" %in% installed.packages()))    install.packages("DescTools")
if(!("RColorBrewer" %in% installed.packages())) install.packages("RColorBrewer")
if(!("dplyr" %in% installed.packages()))        install.packages("dplyr")
if(!("ggplot2" %in% installed.packages()))      install.packages("ggplot2")
if(!("rlang" %in% installed.packages()))        install.packages("rlang")
if(!("nortest" %in% installed.packages()))      install.packages("nortest")
if(!("scales" %in% installed.packages()))       install.packages("scales")
if(!("plotly" %in% installed.packages()))       install.packages("plotly")
if(!("markdown" %in% installed.packages()))     install.packages("markdown")

library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shiny)
library(markdown)
library(DescTools)

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
                    
                    tags$h2("Load data"),
                    
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
                    
                    ## action button to upload the file
                    actionButton(
                        inputId = "upload", 
                        label = "Upload file"),
                    
                    tags$hr(),
                    
                    tags$h2("Select variables"),
                    
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
                    
                    tags$h2("Data Overview"),
                    
                    dataTableOutput("dane")
                    
                )
            )
        )
    )
    
    
    
    
)
