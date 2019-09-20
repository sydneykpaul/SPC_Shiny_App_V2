###########################################
## Title: SPC_ShinyApp
## Author: Sydney Paul
## Date Created: 6/5/2019 
## Date Modified: 09/20/2019
## 
## Description: server.R file
## Allows users to upload a csv or excel file. 
## Runs basic EDA. Checks assumptions are valid for run chart. 
## Walks user through creation of run and control charts. 
## 
## Run at the command line using:
## runApp('./spc_shiny_app')
###########################################
source('dataToChartFunction.R')

 
# Load necessary libraries
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
library(tidyverse)
# TODO: might need zoo for dataToChart function? 

function(input, output, session) {
  # Hide all tabs from user at beginning
  # hideTab("tabs", "tab2")
  # hideTab("tabs", "tab3")
  # hideTab("tabs", "tab4") #TODO: uncomment after testing
  # hideTab("tabs", "tab5")

  
  # Tab 1 -----------------------------------------------------------------------------------------
  #### This is the tab where the user uploads their file
  
  fileData <- reactive({
    req(input$file1)
    df <- NULL
    message <- c("File could not be uploaded. Check that it is the correct file type.")
    tryCatch({
      if (nrow(input$file1) == 1) {
        if (input$file_type == 'csv') {
          df <- read_delim(
            file = input$file1$datapath,
            col_names = as.logical(input$header),
            delim = input$sep,
            quote = input$quote,
            progress = TRUE)
  
        } else if (input$file_type == 'excel') {
          df <- readxl::read_excel(
            path = input$file1$datapath,
            sheet = 1,
            col_names =  as.logical(input$header))
        }
        message <- c("File successfully uploaded.")
        
      } else {
        for (file in input$file1) {
          if (input$file_type == 'csv') {
            df <- data.table::rbindlist(lapply(input$file1$datapath, 
                                     data.table::fread, 
                                     header = as.logical(input$header), 
                                     sep = input$sep, 
                                     quote = input$quote),
                                  use.names = TRUE, fill = TRUE)
            
          } else if (input$file_type == 'excel') {
            df <- data.table::rbindlist(lapply(input$file1$datapath,
                                               readxl::read_excel,
                                               sheet = 1,
                                               col_names = as.logical(input$header)),
                                  use.names = TRUE, fill = TRUE)
          }
        }
        df <- as_tibble(df)
        message <- c("Files successfully uploaded and stacked.")
      }
      
    }, error = function(e){
      # Displays error has occured to the user, but doesn't stop executing
      print(e)
    })
    output$upload_feedback <- renderText(message)
    df
  })
  
  
  output$contents <- renderDataTable(expr = fileData(), options = list(pageLength = 10))
  
  dataChanged <- reactive({
    list(input$file1$datapath, input$file_type, input$header, input$sep, input$quote)
  })
  
  observeEvent(dataChanged(), {
    output$control_plot <- renderPlot({})
    
    df <- fileData()
    
    
    # Makes sure all the inputs are reset when user starts over w/ new file
    updateSelectInput(session, "multiple", label = "Per how many patient days or rate multiplier (Choose 1 if not applicable)",
                choices = list("1" = 1, "10" = 10, "100" = 100, "1000" = 1000, "10000" = 10000),
                selected = "1000")
  
    updateSelectInput(session, 'x_col', choices = c('SELECT', names(df)))
    updateSelectInput(session, 'y_col', choices = c('SELECT'))
    updateSelectInput(session, 'n_col', choices = c('SELECT'))
    
    updateCheckboxInput(session, 'facet', value = FALSE)
    
    updateSliderInput(session, 'bins', value = 30)
    updateSliderInput(session, 'bin_width', value = 1)
    updateCheckboxInput(session, 'histogramBinWidth', value = FALSE)
    
    updateCheckboxGroupInput(session, "checkGroup", label = "Run chart interpretation will be wrong or misleading unless:",
                             choices = list(
                               "There are no obvious patterns" = TRUE,
                               "There are few, if any, bars that cross the blue lines in the ACF plot" = TRUE,
                               "There are no sharp peaks in the spectrum plot" = TRUE
                             ))
    updateSelectInput(session, 'which_facet', "Which facet to plot:", choices = 'Not Applicable')
    
    updateSelectInput(session = session, "choose_control_plot", label = "Choose your SPC plot",
                      choices = list(
                        "None selected" = "none",
                        "Run chart" = 'run chart',
                        "i chart & mr chart" = 'I chart',
                        "x-bar chart & s chart" = "X-bar chart",
                        "p chart" = "p-chart",
                        "p\'-chart" = "p\'-chart",
                        "np chart" = "np",
                        "u chart" = "u-chart",
                        "u\' chart" = "u\'-chart",
                        "c chart" = "c",
                        "g chart" = "g-chart",
                        "t chart" = "t-chart",
                        "EWMA chart" = "EWMA chart",
                        "CUSUM chart" = "CUSUM chart",
                        "Moving Average" = "moving average"
                      ),
                      selected = "none"
    )

    updateCheckboxInput(session, "should_break", value = FALSE)
    updateCheckboxInput(session, "already_grouped", value = TRUE)
    
  })
  

  observeEvent(input$tab1to2, {
    req(!is.null(input$file1) & !is.null(fileData()))
    showTab('tabs', 'tab2')
    updateNavbarPage(session, inputId = 'tabs', selected = 'tab2')
    hideTab("tabs", "tab1") # TODO: hide first tab or not...?
  })
  

  # Tab 2 -----------------------------------------------------------------------------------------
  #### This is the tab where the user selects their desired paramters for later analysis
  
  output$fcolControl <- renderUI({
    if (input$facet)
    {
      df <- fileData()
      selectInput('f_col', "Comparison qualitative groups column", choices = c(names(df)))
    }
  })
  
  # Get user input to match actual column names to desired variables (prevent selecting the same column for multiple variables)
  observeEvent(input$x_col, {
    df <- fileData()
    if (input$x_col != 'SELECT') {
      choices_df <- df %>% dplyr::select(-c(input$x_col))
      updateSelectInput(session, 'y_col', choices = c('SELECT', names(choices_df)))
    }
  })
  
  observeEvent(input$y_col,  {
    df <- fileData()
    if (input$y_col != 'SELECT') {
      choices_df <- df %>% dplyr::select(-c(input$x_col, input$y_col))
      updateSelectInput(session, 'n_col', choices = c('SELECT', names(choices_df)))
    } 
  })
  
  observeEvent(input$n_col,  {
    df <- fileData()
    if (input$n_col != 'SELECT') {
      choices_df <- df %>% dplyr::select(-c(input$x_col, input$y_col)) 
      updateSelectInput(session, 'f_col', choices = c('SELECT', names(choices_df))) 
    }
  })
  
  # Modifying datafile for use
  formatData <- reactive({
    
    df <- fileData()
    
    if (input$n_col == 'SELECT') {
      df[input$n_col] = rep(1,nrow(df))
    }
    if (input$facet) {
      reordered_df <- df[c(input$x_col, input$y_col, input$n_col, input$f_col)]      
      names(reordered_df) <- c('x', 'y', 'n', 'f')
      
    } else  {
      reordered_df <- df[c(input$x_col, input$y_col, input$n_col)]      
      names(reordered_df) <- c('x', 'y', 'n')
      reordered_df <- arrange(reordered_df, x)
    }
  
    reordered_df
  })
  
  observeEvent(input$tab2to3, {
    req(input$file1)
    showTab('tabs', 'tab3')
    updateNavbarPage(session, inputId = 'tabs', selected = 'tab3')
    updateSelectInput(session, 'which_facet', choices = c(unique(formatData()$f)))
  })
  
  
  # Tab 3 -----------------------------------------------------------------------------------------
  #### This is the tab that displays the EDA graphs and checks assumptions
  
  output$histogramBinControl <- renderUI({
    if (as.logical(input$histogramBinWidth)) {
      sliderInput(inputId = "bin_width",
                  label = "Adjust binwidth for histograms:",
                  min = 0.25,
                  max = 20,
                  value = 1, 
                  step = 0.25)
    } else {
      sliderInput(inputId = "bins",
                  label = "Number of bins for histograms:",
                  min = 1,
                  max = 50,
                  value = 30)
    }
  })
  
  output$EDA_plot <- renderPlot({
    eda_df <- formatData()  
    
    bins = switch(as.logical(input$histogramBinWidth) + 1, input$bins, NULL)
    binwidth = switch(as.logical(input$histogramBinWidth) + 1, NULL, input$bin_width)
    
    if(input$facet) {
      # Dot plot with loess smoother for assessing trend
      p1 <- ggplot(eda_df, aes(x = x, y = y, group = 1)) +  # all the data belongs to one group (default creates as many groups as observations)
        geom_smooth(method = 'loess', formula = y ~ x) +
        geom_point() +
        facet_wrap(~f) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x = input$x_col, y = input$y_col)

      # Histogram with density overlay
      p2 <- ggplot(eda_df, aes(y)) +
        geom_histogram(aes(y = ..density..), color = "gray95", bins = bins, binwidth = binwidth) +
        geom_density(fill = "blue", alpha = 0.3) +
        facet_wrap(~f) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        labs(x = paste0(input$y_col, " per ", input$multiple, " patient days"))

    } else {
      # Line plot with loess smoother for assessing trend
      p1 <- ggplot(eda_df, aes(x = x, y = y, group = 1)) + # all the data belongs to one group (default creates as many groups as observations)
        geom_smooth(method = 'loess', formula = y ~ x) +
        geom_point() +
        theme_bw() +
        labs(x = input$x_col, y = input$y_col)
      
      # Histogram with density overlay
      p2 <- ggplot(eda_df, aes(y)) +
        geom_histogram(aes(y = ..density..), color = "gray95", bins = bins, binwidth = binwidth) +
        geom_density(fill = "blue", alpha = 0.3) +
        theme_bw() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
        labs(x = paste0(input$y_col, " per ", input$multiple, " patient days"))
    }
    
    grid.arrange(p1, p2)
  })
  
  ### Begins checking assumptions and plotting ggtsdisplays
  
  output$test_output <- renderPrint({
    df <- formatData()
    message <- c()
    
    if (input$facet) {
      for (d in split.data.frame(df, f = as.factor(df$f))) {
        m <- trend::mk.test(d$y)$p.value
        message <- c(message, m)
      }
    } else {
        message <- trend::mk.test(df$y)
    }
    return(message)
  })
  
  output$trending_check <- renderText({
    df <- formatData()
    message <- c()
    
    if (input$facet) {
      
      for (d in split.data.frame(df, f = as.factor(df$f))) {
        
        mk_test <- trend::mk.test(d$y)

        if (mk_test$p.value > 0.05) {
          has_trend_flag <- FALSE
          m <- paste0(unique(d$f), "<span class='pass'>: Passes the MK trend test at 5%!</span><br/>")
          message <- c(message, m)
          
        } else {
          has_trend_flag <- TRUE
          m <- paste0(unique(d$f),"<span class='fail'>: Failed the MK trend test. Series contains a trend.</span><br/>")
          message <- c(message, m)
        }
      }
      return(message)
      
    } else {
      
      mk_test <- trend::mk.test(df$y)
      
      if (mk_test$p.value > 0.05) {
        has_trend_flag <- FALSE
        return("<span class='pass'>Passes the MK trend test at 5%!</span><br/>")
        
      } else {
        has_trend_flag <- TRUE
        return("<span class='fail'>Failed the MK trend test. Series contains a trend.</span><br/>")
      }
    }
  })
    

  output$afc_plot <- renderPlot({
    df <- formatData()   

    if (input$facet) {
      selected <- filter(df, f == input$which_facet)
      ggtsdisplay(ts(selected$y), theme = theme_bw())
      
    } else {
      df_ts <- ts(df$y)
      ggtsdisplay(df_ts, plot.type = "spectrum", theme = theme_bw())
    }
  })
  
  observeEvent(input$tab3to4, {
    if (length(input$checkGroup) == 3) { # the number of boxes to check is 3
      showTab('tabs', 'tab4')
      updateNavbarPage(session, inputId = 'tabs', selected = 'tab4')
    }
  })

 
   
  # Tab 4 -----------------------------------------------------------------------------------------
  #### This is the tab that displays the run chart
  
  
  output$SPC_run_plot <- renderPlotly({
    df <- formatData()
    dataToChart(df = df, 
                chart_type = 'run chart',
                xLabel = input$x_col,
                yLabel = paste0(input$y_col, " per ", input$multiple, " patient days"),
                multiple = input$multiple)
  })
  
  output$run_chart_summary <- renderTable({
    # summary(get_run_chart())
  })
  
  output$summary_checks <- renderText({
    # chart_summary <- summary(get_run_chart())
    # message <- c()
    # 
    # for (i in 1:nrow(chart_summary)) {
    #   # check number of useful observations
    #   if (chart_summary[i,]$n.obs >= chart_summary[i,]$n.useful) {
    #     m <- paste0(chart_summary[i,]$facet1, "<span class='pass'> - PASS: the number of observations is greater or equal to the number useful.</span><br/>")
    #     message <- c(message, m)
    #   }
    #   else {
    #     m <- paste0(chart_summary[i,]$facet1, "<span class='fail'> - FAIL: the number of observations is NOT greater or equal to the number useful.</span><br/>")
    #     message <- c(message, m)
    #   }     
    #   
    #   # check longest run 
    #   if (chart_summary[i,]$longest.run.max > chart_summary[i,]$longest.run) {
    #     m <- paste0(chart_summary[i,]$facet1, "<span class='pass'> - PASS: the longest run is less than the max allowed.</span><br/>")
    #     message <- c(message, m)
    #   }
    #   else {
    #     m <- paste0(chart_summary[i,]$facet1, "<span class='fail'> - FAIL: the longest run is greater than allowed.</span><br/>")
    #     message <- c(message, m)
    #   }
    #   # check number of crossings
    #   if (chart_summary[i,]$n.crossings >= chart_summary[i,]$n.crossings.min) {
    #     m <- paste0(chart_summary[i,]$facet1, "<span class='pass'> - PASS: there are enough crossings.</span><br/>")
    #     message <- c(message, m)
    #   }
    #   else {
    #     m <- paste0(chart_summary[i,]$facet1, "<span class='fail'> - FAIL: there are not enough crossings.</span><br/>")
    #     message <- c(message, m)
    #   }
    #   message <- c(message, '<br/>', '<br/>')
    # }
    # return(message)
  })
  
  observeEvent(input$tab4to5, {
    showTab('tabs', 'tab5')
    updateNavbarPage(session, inputId = 'tabs', selected = 'tab5')
  })
  
  

  
  # Tab 5 -----------------------------------------------------------------------------------------
  #### This is the tab that displays the desired control plot
  
  output$breakDataControl <- renderUI({
    if (input$should_break)
    {
      selectInput('break_col', label = 'Choose column to break on:', 
                  choices = NULL)
    }
  })
  
  output$breakDateCalendar <- renderUI({
    req(input$break_col)
    if (input$should_break & input$break_col == "Choose date on calendar") {
      dateInput("break_date", label = 'Choose date to break on:')
    }
  })
  
  output$aggFunControl <- renderUI({
    if (input$choose_control_plot == 'run chart' || input$choose_control_plot == 'I chart')
    {
      selectInput("agg_fun", label = "Aggregate function for summarising the y variable if there are more than one observation per subgroup",
                  choices = list('mean' = 'mean', 'median' = 'median', 'sum' = 'sum', 'sd' = 'sd'),
                  selected = 'mean') #TODO: This only does things if n_col isn't null?
    }
    else {
      
    }
  })
  
  output$groupedControls <- renderUI({
    if (input$already_grouped)
    {
      selectInput("pregrouped_on", label = "The data is already subgrouped by:",
                  choices = list('Days' = 'Days', 'Weeks' = 'Weeks', 'Months' = 'Months', 'Quarters' = 'Quarters', 'Years' = 'Years'),
                  selected = "Months")
      # old choice list if needed later: list('Days' = 'Days', 'Weeks' = 'Weeks', 'Months' = 'Months', 'Quarters' = 'Quarters', 'Years' = 'Years', 'Sequential Patients' = 'Sequential Patients', 'Sequential Procedures' = 'Sequential Procedures', 'Other' = 'Subgroups')
    }
    else
    {
      selectInput("subgroup_on", label = "The data needs to be subgrouped by:",
                  choices = list('Days' = 'day', 'Weeks' = 'week', 'Months' = 'month', 'Quarters' = 'quarter', 'Years' = 'year'),
                  selected = "Months")
    }
  })
  
  output$overdispersion_results <- renderPrint({
    if (input$choose_control_plot == 'u-chart' | input$choose_control_plot == 'p-chart') {
      df <- formatData()
      od_result <- overdispersion.test(df$y, df$n, input$choose_control_plot)
      return(od_result)
    }
  })
  
  output$overdispersion_text <- renderText({
    if (input$choose_control_plot == 'u-chart' | input$choose_control_plot == 'u\'-chart' | input$choose_control_plot == 'p-chart' | input$choose_control_plot == 'p\'-chart') {
      df <- formatData()
      od_result <- overdispersion.test(df$y, df$n, input$choose_control_plot)
      
      if (od_result$p_value > 0.05) {
        m <- paste0("<span class='fail'>FAIL: Use prime chart instead </span><br/>")
      } else {
        m <- paste0("<span class='pass'>PASS: Overdispersion is not a problem </span><br/>")
      }
      
    } else {
      m <- "You do not have to account for overdispersion with this type of chart."
    }
    
    return(m)
  })

  get_breaks <- reactive({
    req(input$break_col)
    df <- fileData()
    
    # break on user input date
    if (input$break_col == "Choose date on calendar") {
      if (!is.null(input$break_date)) {
        if (input$already_grouped) {
          dataDates <- as.Date(arrange_at(df, input$x_col)[[input$x_col]])
          breaks <- which.min(abs(dataDates - as.Date(input$break_date)))
  
        } else if (is.null(input$subgroup_on)) {
          return(NULL)
        } else {
          only_dates <- as_datetime(arrange_at(df, input$x_col)[[input$x_col]])
          changed_dates <- cut.POSIXt(only_dates, input$subgroup_on) %>% unique()

          breaks <- which.min(abs(as.Date(changed_dates) - as.Date(input$break_date)))
        }
      } else {
        return(NULL) # user hasn't picked date yet
      }
      
    # break on column
    } else {
      cutoffDates <- (df %>% group_by_at(input$break_col) %>% summarise_at(input$x_col, max) %>% arrange_at(input$x_col))[[input$x_col]]
  
      if (input$already_grouped) {
        dataDates <- arrange_at(df, input$x_col)[[input$x_col]]
  
      } else if (is.null(input$subgroup_on)) {
          return(NULL)
        
      } else {
        cutoffDates <- cut.POSIXt(as_datetime(cutoffDates), input$subgroup_on)
        dataDates <- (df %>% mutate_at(input$x_col, cut.POSIXt, input$subgroup_on) %>% group_by_at(input$x_col) %>% summarise_at(input$break_col, length) %>% arrange_at(input$x_col))[[input$x_col]]
      }
  
      breaks <- which(dataDates %in% cutoffDates)
    }
    
    return(breaks) 
  })
  
  breakColNamesChange <- reactive({
    list(dataChanged(), input$should_break)
  })
  
  observeEvent(breakColNamesChange(), {
    if (input$should_break)
    {
      df <- fileData()
      colNames <- names(df)
      colNames <- colNames[colNames != input$x_col] # TODO: Also remove the y_col? n and f columns?
      updateSelectInput(session, 'break_col', choices = c(colNames, "Choose date on calendar"))
    }
  })
  
  chartChange <- reactive({
    list(input$should_break, 
         input$choose_control_plot, 
         input$break_col, 
         input$already_grouped, 
         input$subgroup_on, 
         input$agg_fun, 
         input$break_date)
  })
  
  observeEvent(chartChange(), {
    if (input$choose_control_plot == "none") {
      output$control_plot <- renderPlotly({})
      return();
    }
    
    if (input$choose_control_plot == 'np') {
      output$control_plot <- renderPlotly({
        plotly_empty() %>% layout(title = str_wrap("For proportion data, prefer p-charts to np-charts. In most cases, we do not have a constant denominator, so np-charts would not be appropriate. Even when we do, using a p-chart helps reduce audience confusion by explicitly stating the 'per x'."))
      })
      return()
    }
    if (input$choose_control_plot == 'c') {
      output$control_plot <- renderPlotly({
        plotly_empty() %>% layout(title = str_wrap("For count data, prefer u-charts to c-charts. In most cases, we do not have a constant denominator, so c-charts would not be appropriate. Even when we do, using a u-chart helps reduce audience confusion because you are explicitly stating the 'per x'."))
      })
      return()      
    }

    df <- formatData()
    
    # Using "switch" instead of "ifelse", because "ifelse" can't handle returning NULL
    xlabel = switch(input$already_grouped + 1, input$subgroup_on, input$pregrouped_on)
    xperiod = switch(input$already_grouped + 1, input$subgroup_on, NULL)
    parts = switch(input$should_break + 1, NULL, get_breaks())
    facet = switch(input$facet + 1, NULL, ~f)
    aggfun = switch((input$choose_control_plot == 'run chart' || input$choose_control_plot == 'I chart') + 1, NULL, input$agg_fun)
    
    output$control_plot <- renderPlotly({
      dataToChart(df = df, 
                  chart_type = input$choose_control_plot,
                  xLabel = input$x_col,
                  yLabel = paste0(input$y_col, " per ", input$multiple, " patient days"),
                  multiple = input$multiple,
                  already_subgrouped = input$already_grouped, 
                  subgroup_on = input$subgroup_on,
                  should_break = FALSE)
    })
  })
  
  
  observeEvent(input$return_to_start, {
    showTab("tabs", "tab1")
    updateNavbarPage(session, inputId = 'tabs', selected = 'tab1')

    # hideTab("tabs", "tab2")
    # hideTab("tabs", "tab3") # TODO: uncomment after testing
    # hideTab("tabs", "tab4")
    # hideTab("tabs", "tab5")

  })
  
    # output$save_plot <- downloadHandler( # TODO: with plotly don't need this anymore??
    #   filename = function() { paste0(input$choose_control_plot, "_", "chart.png") },
    #   content = function(file) {
    #     ggsave(file, device = "png", width = 20, height = 12, units = 'in')
    #   }
    # )
  
  observeEvent(input$quit_app, {
    stopApp()
  })
}
