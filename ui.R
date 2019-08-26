###########################################
## Title: SPC_ShinyApp
## Author: Sydney Paul
## Date Created: 6/5/2019 
## Date Modified: 7/24/2019
## 
## Description: ui.R file
## Allows users to upload a csv or excel file. 
## Runs basic EDA. Checks assumptions are valid for run chart. 
## Walks user through creation of run and control charts. 
## 
## Run at the command line using:
## runApp('./spc_shiny_app')
###########################################

# Load necessary libraries
library(tidyverse)
library(ggplot2) # for general plotting
library(lubridate) # for easier date/time casting
library(forecast) # for plotting and forecasts
library(qicharts2) # for simple run charts and control charts
library(seasonal) # for seasonal adjusment calculations
library(ggseas) # for on-the-fly seasonal adjustment plotting
library(ggExtra) # for making line+histogram marginal plots
library(gridExtra) # for creating multi-graph plots
library(shiny)
library(plotly)
library(shinythemes)



navbarPage(
  
  theme = shinytheme("simplex"),
  title = "SPC ShinyApp",
  id = "tabs",
  
  # Tab 1 -----------------------------------------------------------------------------------------
  #### This is the tab where the user uploads their file
  
  tabPanel(
    title = "Step 1: Load your file",
    value = "tab1",
    sidebarLayout(
      sidebarPanel(
        h2('Select a file or files:'),
        
        h5('Selecting multiple files will create one large table of stacked files.'),
        h5('Please ensure that all column names and file types are the same.'),
        br(),
        
        fileInput("file1", "Choose a File", multiple = TRUE,
          accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv", ".xlsx", ".xls")
        ),
        
        radioButtons("file_type", "File Type", choices = c('.xlsx or .xls' = "excel", '.csv or .txt' = 'csv'), selected = 'excel'),

        radioButtons("header", "Header", choices = c(Yes = TRUE, No = FALSE), selected = TRUE),
        radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
        radioButtons("quote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
      
      ),
      
      mainPanel(
        fluidRow(
          textOutput("upload_feedback"),
          br(),
          dataTableOutput("contents")
        ),
        
        fluidRow(
          # Continue button, moves to next page AFTER file loaded
          column(width = 1, offset = 10,
                 actionButton("tab1to2", "Continue"))
        )
      ) # End of mainPanel
    )
  ),
  
  # Tab 2 -----------------------------------------------------------------------------------------
  #### This is the tab where the user selects their desired paramters for later analysis
  
  tabPanel(
    title = "Step 2: Set your parameters",
    value = "tab2",
    fluidPage(
      fluidRow(
        h4("Set parameters:"),
        
        selectInput("multiple", label = "Per how many patient days or rate multiplier (Choose 1 if not applicable)",
                    choices = list("1" = 1, "10" = 10, "100" = 100, "1000" = 1000, "10000" = 10000),
                    selected = "1000")),
      
      br(),
      br(),

      
      fluidRow(
        h4("Match variables with column names:"),
        selectInput('x_col', "Date column or subgroups (values to plot along the x axis)", choices = NULL), 
        selectInput('y_col', "Numerator (measures or counts to plot on the y axis)", choices = NULL),
        selectInput('n_col', "Denominator (subgroup sizes)", choices = NULL),
        
        br(),
        
        checkboxInput("facet", "Check this box if you want to compare your data based on a qualitative grouping column, ex. departments", value = FALSE),
        uiOutput("fcolControl")
      ),
      
      fluidRow (
          column(width = 1, offset = 10,
                 actionButton("tab2to3", "Continue"))
      )
    )
  ),
  
  
  # Tab 3 -----------------------------------------------------------------------------------------
  #### This is the tab that displays the EDA graphs and checks assumptions
  
  tabPanel(
    title = "Step 3: Exploratory Data Analysis",
    value = "tab3",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          h3("In these plots, consider:"),
          
          tags$ul(
            tags$li("The shape of the distribution: symmetrical/skewed, uniform/peaked/multimodal, whether changes in binwidth show patterning, etc."),
            tags$li("Whether you see any trending, cycles, or suggestions of autocorrelation."),
            tags$li("Whether there are any obvious outliers or inliers—basically, any points deviating form the expected pattern.")
          )
        ),
        
        br(),
        br(),
        
        fluidRow(
          h3("Trending:"),
          tags$ul(
            tags$li("You can test whether a process is trending first by eye: does it look like it’s trending over a large span of the time series? Then it probably is."),
            tags$li("Because trends can be an indication of special cause variation in a stable process, standard control limits don’t make sense around long-trending data, and calculation of center lines and control limits will be incorrect.")
          ),
          
          br(),
          
          h4("Mann-Kendall Trend Test"),
          tags$ul(
            tags$li("When sample size is low (n < 20) this test is not useful/accurate.")
          ),
          verbatimTextOutput("test_output"),
          htmlOutput("trending_check")
        ),
        
        br(),
        br(), 
        
        h3("Distributions:"),
        sliderInput(inputId = "bins",
                    label = "Number of bins for histograms:",
                    min = 1,
                    max = 50,
                    value = 30),
        br(),
        br(),
        
        fluidRow(
          h3("Independence and Autocorrelation:"),
          tags$ul(
            tags$li("For either run charts or control charts, the data points must be independent for the guidelines to be effective.
                    The first test of that is conceptual—do you expect that one value in this series will influence a subsequent
                    value?")
            )
        ),
        
        br(),
        br(), 
        
        fluidRow(
          h3("Evaluate these plots before moving on!", style = 'color:red'),
          
          column(width = 12,
                 checkboxGroupInput("checkGroup", label = "Run chart interpretation will be wrong or misleading unless:",
                                    choices = list(
                                      "There are no obvious patterns" = TRUE,
                                      "There are few, if any, bars that cross the blue lines in the ACF plot" = TRUE,
                                      "There are no sharp peaks in the spectrum plot" = TRUE
                                    )
                 )
          )
        ),
        
        br(),
        
        fluidRow(
          h5("Understanding your data is a fundamental prerequisite of SPC work. Do not move on to SPC
          work until you have explored your data and fully understand whether the data is suitable for SPC tools."),
          
          br(),
          
          h5("You may press continue to move on AFTER you check all the boxes that your assumptions have been assessed.")
        )
      ),
      mainPanel(
        fluidRow(
          plotOutput("EDA_plot", height = '800px')
        ),
        
        br(),
        br(),
        
        fluidRow(
          selectInput('which_facet', "Which facet to plot:", choices = 'Not Applicable'),
          
          plotOutput('afc_plot')
        ),
        
        column(width = 1, offset = 10,
               actionButton("tab3to4", "Continue"))
      )
    )
  ),
  
  
  # Tab 4 -----------------------------------------------------------------------------------------
  #### This is the tab that displays the run chart
  
  tabPanel(
    title = "Step 4: SPC Run Chart",
    value = "tab4",
    
    fluidPage(
      
      fluidRow(
        h1("Identifying possible signals of change in run charts"),
        tags$ul(
          tags$li("“Astronomical” data point: a point so different from the rest that anyone would agree that the value is unusual."),
          tags$li("Process shift: log2(n) + 3 data points are all above or all below the median line, where n is the total number of points that do not fall directly on the median line."),
          tags$li("Number of crossings: Too many or too few median line crossings suggest a pattern inconsistent with natural variation")
        )
      ),
      
      br(),
      br(),
      
      fluidRow(
        plotOutput("SPC_run_plot", height = '800px'),
        
        h2("Run Chart Analysis"),
        tableOutput("run_chart_summary")
      ),
      
      fluidRow(
        htmlOutput("summary_checks"),

        br(),
        br(),
        
        
        column(width = 1, offset = 10,
               actionButton("tab4to5", "Continue"))
      )
    )
  ),
  
  
  # Tab 5 -----------------------------------------------------------------------------------------
  #### This is the tab that displays the desired control plot
  
  tabPanel(
    title = "Step 5: Which control chart should I use?",
    value = "tab5",
    
    fluidPage(
      h3("Statistical Process Control is not about statistics, it is not about 'process-hyphen-control', and it is not about conformance to specifications. It is about the continual improvement of processes and outcomes. And it is, first and foremost, a way of thinking with some tools attached."),
      br(),
      h4("- Donald J. Wheeler, Understanding Variation"),

      br(),
      br(),
      
      h2('SPC Flowchart'),
      fluidRow(
        img(src = 'control_chart_flowchart.png', width = 700)
      ),
      
      fluidRow(
        h2("SPC Plot"),
        column(width = 2, 
        selectInput("choose_control_plot", label = "Choose your SPC plot",
          choices = list(
            "None selected" = "none",
            "Run chart" = 'run',
            "EWMA chart" = "EWMA",
            "CUSUM chart" = "CUSUM",
            "I chart & MR chart" = 'imr',
            "x̄ chart & s chart" = "xbars",
            "p chart" = "p",
            "np chart" = "np",
            "u chart" = "u", 
            "c chart" = "c",
            "g chart" = "g",
            "t chart" = "t"
          ),
          selected = "none"
        ),

        # User options for control plot
        uiOutput('aggFunControl'),
        
        checkboxInput('should_break', label = 'Do you want to break the x-axis?', value = FALSE),
        uiOutput('breakDataControl'),
        uiOutput('breakDateCalendar'),
        
        checkboxInput('already_grouped', "Data has already been grouped", value = TRUE),
        uiOutput("groupedControls")
      ),
      
      column(width = 10,
      fluidRow(
        plotlyOutput("control_plot", height = '800px'),
        verbatimTextOutput('not_available')
      ))),
      
      fluidRow(
               actionButton("return_to_start", "Startover with a new file"),
               # downloadButton("save_plot", "Save your plot as .png"), # TODO: with plotly don't need this anymore??
               actionButton("quit_app", "Quit")
        
      )
    )
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "spc.css")
  )
  
)
