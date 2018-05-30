#------------------------------------------------------------------------------#
####                            UI SCRIPT                                   ####
#------------------------------------------------------------------------------#

## check for installed packages
if(!("DescTools" %in% installed.packages()))    install.packages("DescTools")
if(!("RColorBrewer" %in% installed.packages())) install.packages("RColorBrewer")
if(!("dplyr" %in% installed.packages()))        install.packages("dplyr")
if(!("ggplot2" %in% installed.packages()))      install.packages("ggplot2")
if(!("rlang" %in% installed.packages()))        install.packages("rlang")
if(!("nortest" %in% installed.packages()))      install.packages("nortest")
if(!("scales" %in% installed.packages()))       install.packages("scales")
if(!("plotly" %in% installed.packages()))       install.packages("plotly")
if(!("markdown" %in% installed.packages()))     install.packages("markdown")

## load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shiny)
library(markdown)
library(DescTools)
library(plotly)

#------------------------------------------------------------------------------#
####                            Define UI                                   ####
#------------------------------------------------------------------------------#

shinyUI(
    navbarPage(
        title = "Exploratory data analysis", 
        
        #### Load data ####
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
                        label = "Upload file")
                    # ,
                    
                    # tags$hr(),
                    # 
                    # tags$h2("Select variables"),
                    # 
                    # ## select Y variable
                    # selectInput(
                    #     inputId = "target",
                    #     label = "Select target variable",
                    #     choices = names(df)
                    # ),
                    # 
                    # ## select X variable
                    # selectInput(
                    #     inputId = "var_name",
                    #     label = "Select predictor variable",
                    #     choices = names(df)
                    # ),
                    # 
                    # ## select predictor type
                    # radioButtons(
                    #     inputId = "target_type",
                    #     label = "Select target type",
                    #     choices = c("binary", "numeric")
                    # )
                    
                ),
                mainPanel(
                    
                    tags$h2("Data Overview"),
                    dataTableOutput("dane")
                    
                )
            )
        ),
        
        #### Variable Description ####
        tabPanel(
            title = "Variable Description",
            
            sidebarLayout(
                
                sidebarPanel(width = 2,
                    
                    style = "position:fixed;width:inherit;",
                             
                    tags$h2("Select predictor"),
                    
                    ## select X variable
                    selectInput(
                        inputId = "var_name",
                        label = "Select predictor variable",
                        choices = names(df)
                    ),
                    
                    ## logarithm the variable or not
                    checkboxInput(
                        inputId = "log_dummy",
                        label = "Log transformation",
                        value = FALSE
                    ),
                    
                    ## remove outliers or not
                    checkboxInput(
                        inputId = "outlier_dummy",
                        label = "Remove outliers",
                        value = FALSE
                    ),
                    
                    ## outlier definition
                    numericInput(
                        inputId = "outlier_def",
                        label = "Outlier definition - # of sd from the mean",
                        value = 3,
                        min = 2,
                        max = 6,
                        step = 1
                    ),
                    
                    ## number of bins in histogram
                    sliderInput(
                        inputId = "bins",
                        label = "Select number of bins",
                        value = 30,
                        min = 10,
                        max = 50,
                        step = 1
                    ),
                    
                    tags$hr(),
                    
                    tags$h2("Select variables"),
                    
                    ## select Y variable
                    selectInput(
                        inputId = "target",
                        label = "Select target variable",
                        choices = names(df)
                    ),
                    
                    ## select predictor type
                    radioButtons(
                        inputId = "target_type",
                        label = "Select target type",
                        choices = c("binary", "numeric")
                    ),
                    
                    conditionalPanel(
                        condition = "input.target_type == 'numeric'",
                        
                        ## logarithm the target or not
                        checkboxInput(
                            inputId = "log_dummy_tar",
                            label = "Log transformation",
                            value = FALSE
                        ),
                        
                        ## remove outliers from target or not
                        checkboxInput(
                            inputId = "outlier_dummy_tar",
                            label = "Remove outliers",
                            value = FALSE
                        )
                    )
                    
                ),
                
                mainPanel(
                    
                    ## descriptive statistics
                    fluidRow(
                        column(width = 12,
                               tags$h2("Descriptive statistics"),
                        
                               verbatimTextOutput("summary"),
                               tags$br()
                        )
                    ),
                    
                    ## histogram and boxplot
                    fluidRow(
                        
                        column(width = 7, 
                               tags$h2("Histogram & boxplot"),
                               plotlyOutput("hist_plot")),
                        column(width = 5,
                               plotlyOutput("box_plot"))
                    ),
                    
                    ## normality test
                    fluidRow(
                        
                        column(width = 12,
                            tags$h2("Normality test"),
                            p(em(paste0("P-values smaller than 0.1 indicate that ",
                                    "the variable is not normally distributed")
                                )),
                            verbatimTextOutput("norm_test")
                        )
                        
                    ),
                    
                    ## CONDITIONAL 
                    fluidRow(
                        # tags$h2("Relation with target"),
                        
                        ## target numeric - scatterplot and correlation
                        conditionalPanel(
                            condition = "input.target_type == 'numeric'",
                            
                            column(width = 10,
                                   tags$h2("Relation with target"),
                                plotlyOutput("scatter_plot")
                            ),
                            column(width = 2,
                                tags$h4(textOutput("correlation"))
                            )
                        ),
                        
                        ## target factor
                        conditionalPanel(
                            condition = "input.target_type == 'binary'",
                            
                            column(width = 7,
                                   tags$h2("Relation with target"),
                                plotlyOutput("dens_plot")
                            ),
                            column(width = 5,
                                plotlyOutput("box_plot_2")
                            )
                        )
                    ),
                    
                    ## CONDITIONAL 2
                    fluidRow(
                        conditionalPanel(
                            condition = "input.target_type == 'binary'",
                            
                            column(width = 6,
                                   tags$h3("T test for means"),
                                   verbatimTextOutput("t_test")
                            ),
                            column(width = 6,
                                   tags$h3("Wilcoxon test for means"),
                                   verbatimTextOutput("wilcoxon_test")
                            )
                        )
                    )
                )
                
            )
            
        )
    )
)
