###########################################
## Title: SPC_ShinyApp
## Author: Sydney Paul
## Date Created: 6/5/2019 
## Date Modified: 7/24/2019
## 
## Description: server.R file
## Allows users to upload a csv or excel file. 
## Runs basic EDA. Checks assumptions are valid for run chart. 
## Walks user through creation of run and control charts. 
## 
## Run at the command line using:
## runApp('./spc_shiny_app')
###########################################


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


plotSPC <- function(subgroup, point, mean, sigma, k = 3,
                    ucl.show = TRUE, lcl.show = TRUE,
                    band.show = TRUE, rule.show = TRUE,
                    ucl.max = Inf, lcl.min = -Inf,
                    label.x = "Subgroup", label.y = "Value") {
  # Plots control chart with ggplot
  ##
  # Args:
  # subgroup: Subgroup definition (for x-axis)
  # point: Subgroup sample values (for y-axis)
  # mean: Process mean value (for center line)
  # sigma: Process variation value (for control limits)
  # k: Specification for k-sigma limits above and below center line, default is 3
  # ucl.show: Visible upper control limit? Default is true
  # lcl.show: Visible lower control limit? Default is true
  # band.show: Visible bands between 1-2 sigma limits? Default is true
  # rule.show: Highlight run rule indicators in orange? Default is true
  # ucl.max: Maximum feasible value for upper control limit
  # lcl.min: Minimum feasible value for lower control limit
  # label.x: Specify x-axis label
  # label.y: Specify y-axis label
  
  df = data.frame(subgroup, point)
  df$ucl = pmin(ucl.max, mean + k*sigma)
  df$lcl = pmax(lcl.min, mean - k*sigma)
  warn.points = function(rule, num, den) {
    sets = mapply(seq, 1:(length(subgroup) - (den - 1)),
                  den:length(subgroup))
    hits = apply(sets, 2, function(x) sum(rule[x])) >= num
    intersect(c(sets[,hits]), which(rule))
  }
  orange.sigma = numeric()
  
  p = ggplot(data = df, aes(x = subgroup)) +
    geom_hline(yintercept = mean, col = "gray", size = 1)
  if (ucl.show) {
    p = p + geom_line(aes(y = ucl), col = "gray", size = 1)
  }
  if (lcl.show) {
    p = p + geom_line(aes(y = lcl), col = "gray", size = 1)
  }
  if (band.show) {
    p = p +
      geom_ribbon(aes(ymin = mean + sigma,
                      ymax = mean + 2*sigma), alpha = 0.1) +
      geom_ribbon(aes(ymin = pmax(lcl.min, mean - 2*sigma),
                      ymax = mean - sigma), alpha = 0.1)
    orange.sigma = unique(c(
      warn.points(point > mean + sigma, 4, 5),
      warn.points(point < mean - sigma, 4, 5),
      warn.points(point > mean + 2*sigma, 2, 3),
      warn.points(point < mean - 2*sigma, 2, 3)
    ))
  }
  df$warn = "blue"
  if (rule.show) {
    shift.n = round(log(sum(point!=mean), 2) + 3)
    orange = unique(c(orange.sigma,
                      warn.points(point > mean - sigma & point < mean + sigma, 15, 15),
                      warn.points(point > mean, shift.n, shift.n),
                      warn.points(point < mean, shift.n, shift.n)))
    df$warn[orange] = "orange"
  }
  df$warn[point > df$ucl | point < df$lcl] = "red"
  
  
  p_final <- p +
    geom_line(aes(y = point), col = "royalblue3") +
    geom_point(data = df, aes(x = subgroup, y = point, col = warn)) +
    scale_color_manual(values = c("blue" = "royalblue3", "orange" = "orangered", "red" = "red3"), guide = FALSE) +
    labs(x = label.x, y = label.y) +
    theme_bw()
  
  ggplotly(p_final)
}

layout_ggplotly <- function(gg, x = -0.1, y = -0.03){
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
  gg[['x']][['layout']][['annotations']][[2]][['x']] <- y
  gg
}

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
      updateSelectInput(session, 'n_col', choices = c('SELECT', names(choices_df), 'NONE'))
    } 
  })
  
  observeEvent(input$n_col,  {
    df <- fileData()
    if (input$n_col != 'SELECT') {
      if (input$n_col == 'NONE') {
        choices_df <- df %>% dplyr::select(-c(input$x_col, input$y_col)) 
        updateSelectInput(session, 'f_col', choices = c('SELECT', names(choices_df), 'NONE')) 
        
      } else {
      # f_col lines aren't necessary right now as we display all columns as options in renderUI for f_col
      # if want to restrict to unused columns, will have to start here
      choices_df <- df %>% dplyr::select(-c(input$x_col, input$y_col, input$n_col))
      updateSelectInput(session, 'f_col', choices = c('SELECT', names(choices_df), 'NONE'))
      }
    }
  })
  
  # Modifying datafile for use
  formatData <- reactive({
    
    df <- fileData()
    
    if (input$n_col == 'NONE') {
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
  
  output$EDA_plot <- renderPlot({
    eda_df <- formatData()    
    
    if(input$facet) {
      # Line plot with loess smoother for assessing trend
      p1 <- ggplot(eda_df, aes(x = x, y = y, group = 1)) +  # all the data belongs to one group (default creates as many groups as observations)
        geom_smooth(method = 'loess', formula = y ~ x) +
        geom_line() +
        facet_wrap(~f) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x = 'Subgroup', y = 'Value')

      # Histogram with density overlay
      p2 <- ggplot(eda_df, aes(y)) +
        geom_histogram(aes(y = ..density..), color = "gray95", bins = input$bins) +
        geom_density(fill = "blue", alpha = 0.3) +
        facet_wrap(~f) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        labs(x = paste0("Value per", " ", input$multiple, " patient days"))

    } else {
      # Line plot with loess smoother for assessing trend
      p1 <- ggplot(eda_df, aes(x = x, y = y, group = 1)) + # all the data belongs to one group (default creates as many groups as observations)
        geom_smooth(method = 'loess', formula = y ~ x) +
        geom_line() +
        theme_bw() +
        labs(x = 'Subgroup', y = 'Value')
      
      # Histogram with density overlay
      p2 <- ggplot(eda_df, aes(y)) +
        geom_histogram(aes(y = ..density..), color = "gray95", bins = input$bins) +
        geom_density(fill = "blue", alpha = 0.3) +
        theme_bw() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
        labs(x = paste0("Value per", " ", input$multiple, " patient days"))
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
  
  get_run_chart <- reactive({
    df <- formatData()
    if(input$facet) {
      run_chart <- qicharts2::qic(x = x, y = y, n = n, data = df, 
                                  multiply = as.numeric(input$multiple),
                                  chart = 'run',
                                  agg.fun = input$agg_fun, 
                                  ylab = paste0("Value per", " ", input$multiple, " patient days"), 
                                  xlab = input$pregrouped_on, 
                                  title = 'Run Chart',
                                  facets = ~f,
                                  x.angle = 45)      
    } else {
      run_chart <- qicharts2::qic(x = x, y = y, n = n, data = df, 
                                  multiply = as.numeric(input$multiple),
                                  chart = 'run',
                                  agg.fun = input$agg_fun,
                                  ylab = paste0("Value per", " ", input$multiple, " patient days"), 
                                  xlab = input$pregrouped_on, 
                                  title = 'Run Chart')
    }
    run_chart
  })
  
  
  output$SPC_run_plot <- renderPlot({show(get_run_chart())})
  
  output$run_chart_summary <- renderTable({
    summary(get_run_chart())
  })
  
  output$summary_checks <- renderText({
    chart_summary <- summary(get_run_chart())
    message <- c()

    for (i in 1:nrow(chart_summary)) {
      # check number of useful observations
      if (chart_summary[i,]$n.obs >= chart_summary[i,]$n.useful) {
        m <- paste0(chart_summary[i,]$facet1, "<span class='pass'> - PASS: the number of observations is greater or equal to the number useful.</span><br/>")
        message <- c(message, m)
      }
      else {
        m <- paste0(chart_summary[i,]$facet1, "<span class='fail'> - FAIL: the number of observations is NOT greater or equal to the number useful.</span><br/>")
        message <- c(message, m)
      }     
      
      # check longest run 
      if (chart_summary[i,]$longest.run.max > chart_summary[i,]$longest.run) {
        m <- paste0(chart_summary[i,]$facet1, "<span class='pass'> - PASS: the longest run is less than the max allowed.</span><br/>")
        message <- c(message, m)
      }
      else {
        m <- paste0(chart_summary[i,]$facet1, "<span class='fail'> - FAIL: the longest run is greater than allowed.</span><br/>")
        message <- c(message, m)
      }
      # check number of crossings
      if (chart_summary[i,]$n.crossings >= chart_summary[i,]$n.crossings.min) {
        m <- paste0(chart_summary[i,]$facet1, "<span class='pass'> - PASS: there are enough crossings.</span><br/>")
        message <- c(message, m)
      }
      else {
        m <- paste0(chart_summary[i,]$facet1, "<span class='fail'> - FAIL: there are not enough crossings.</span><br/>")
        message <- c(message, m)
      }
      message <- c(message, '<br/>', '<br/>')
    }
    return(message)
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
    if (input$choose_control_plot == 'run' || input$choose_control_plot == 'imr')
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
  
  get_EWMA_chart <- reactive({
    df <- formatData()
    
    subgroup.x = unique(df$x)
    subgroup.s = subgroup.x
    
    point.x = aggregate(df$y, by = list(df$x), FUN = mean, na.rm = TRUE)$x
    point.s = aggregate(df$y, by = list(df$x), FUN = sd, na.rm = TRUE)$x
    
    mean.x = mean(df$y)
    sample.n = as.numeric(table(df$x))
    mean.s = sqrt(sum((sample.n - 1) * point.s ^ 2) / (sum(sample.n) - length(sample.n)))
    sigma.x = mean.s / sqrt(sample.n)
    c4 = sqrt(2 / (sample.n - 1)) * gamma(sample.n / 2) /
      gamma((sample.n - 1) / 2)
    sigma.s = mean.s * sqrt(1 - c4 ^ 2)
    
    # Calculate control chart inputs
    subgroup.z = subgroup.x
    lambda = 0.2
    point.z = matrix(data = NA, nrow = length(point.x))
    point.z[1] = mean.x
    for (i in 2:length(point.z)) {
      point.z[i] = lambda * point.x[i] + (1 - lambda) * point.z[i-1]
    }
    mean.z = mean.x
    sigma.z = (mean.s / sqrt(sample.n)) *
      sqrt(lambda/(2-lambda) * (1 - (1-lambda)^(seq(1:length(point.z)))))
    
    plotSPC(subgroup.z, point.z, mean.z, sigma.z, k = 3, band.show = FALSE,
            rule.show = FALSE, 
            label.x = switch(input$already_grouped + 1, input$subgroup_on, input$pregrouped_on),
            label.y = paste0("Value per ", input$multiple, " patient days moving average")) 
  })
  
  get_CUSUM_chart <- reactive({
    df <- formatData()
    
    subgroup.x = unique(df$x)
    subgroup.s = subgroup.x
    
    point.x = aggregate(df$y, by = list(df$x), FUN = mean, na.rm = TRUE)$x
    point.s = aggregate(df$y, by = list(df$x), FUN = sd, na.rm = TRUE)$x
    
    mean.x = mean(df$y)
    sample.n = as.numeric(table(df$x))
    mean.s = sqrt(sum((sample.n - 1) * point.s ^ 2) / (sum(sample.n) - length(sample.n)))
    sigma.x = mean.s / sqrt(sample.n)
    c4 = sqrt(2 / (sample.n - 1)) * gamma(sample.n / 2) /
      gamma((sample.n - 1) / 2)
    sigma.s = mean.s * sqrt(1 - c4 ^ 2)
    
    subgroup.cusum = subgroup.x
    slack = 0.5
    zscore = (point.x - mean.x)/sigma.x
    
    point.cusuml = matrix(data = NA, nrow = length(zscore))
    point.cusuml[1] = -max(0, -zscore[1] - slack)
    
    for (i in 2:length(point.cusuml)) {
      point.cusuml[i] = -max(0, -zscore[i] - slack - point.cusuml[i-1])
    }
    
    point.cusumh = matrix(data = NA, nrow = length(zscore))
    point.cusumh[1] = max(0, zscore[1] - slack)
    
    for (i in 2:length(point.cusuml)) {
      point.cusumh[i] = max(0, zscore[i] - slack - point.cusumh[i - 1])
    }
    
    mean.cusum = 0
    sigma.cusum = rep(1, length(subgroup.cusum))
    
    # Plot CUSUM chart
    lower.plot = plotSPC(subgroup.cusum, point.cusuml, mean.cusum, sigma.cusum,
                          k = 5, band.show = FALSE, rule.show = FALSE,
                          label.y = paste0("Value per ", input$multiple, " patient days cumulative sum"), 
                          label.x = switch(input$already_grouped + 1, input$subgroup_on, input$pregrouped_on))
    p_final <- lower.plot + geom_line(aes(y = point.cusumh), col = "royalblue3") +
      geom_point(aes(y = point.cusumh), col = "royalblue3")
    
    ggplotly(p_final)
  })
  
  # TODO: EWMA and CUSUM faceted? Subgrouped? Breaks? 

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
    list(input$should_break, input$choose_control_plot, input$break_col, input$already_grouped, input$subgroup_on, input$agg_fun, input$break_date)
  })
  
  observeEvent(chartChange(), {
    if (input$choose_control_plot == "none") {
      output$control_plot <- renderPlotly({})
      return();
    }

    df <- formatData()
    
    # Using "switch" instead of "ifelse", because "ifelse" can't handle returning NULL
    xlabel = switch(input$already_grouped + 1, input$subgroup_on, input$pregrouped_on)
    xperiod = switch(input$already_grouped + 1, input$subgroup_on, NULL)
    parts = switch(input$should_break + 1, NULL, get_breaks())
    facet = switch(input$facet + 1, NULL, ~f)
    aggfun = switch((input$choose_control_plot == 'run' || input$choose_control_plot == 'imr') + 1, NULL, input$agg_fun)
    
      
    if (input$choose_control_plot == "EWMA") {
        output$control_plot <- renderPlotly(get_EWMA_chart())
        
    } else if (input$choose_control_plot == 'CUSUM') {
        output$control_plot <- renderPlotly(get_CUSUM_chart())
      
    } else if (input$choose_control_plot == 'np') {
        output$control_plot <- renderPlotly({
          ggplotly(ggplot(tibble(x = 1, y = 1), aes(x, y)) + 
            geom_point() + 
            labs(title = "p-charts are prefered", x = '', y = ''))
        })
      
    } else if (input$choose_control_plot == 'imr' || input$choose_control_plot == 'xbars') {
      if (input$choose_control_plot == 'imr') {
        title1 = 'I chart'
        chart1 = 'i'
        
        title2 = 'MR chart'
        chart2 = 'mr'
        
      } else {
        title1 = 'xbar chart'
        chart1 = 'xbar'
        
        title2 = 's chart'
        chart2 = 's'
      }
      
      p1 <- ggplotly(
               qicharts2::qic(x = x, y = y, n = n, data = df, multiply = as.numeric(input$multiple),
                     ylab = paste0("Value per", " ", input$multiple, " patient days"), xlab = xlabel,
                     title = title1,
                     chart = chart1,
                     x.period = xperiod,
                     part = parts,
                     facets = facet,
                     agg.fun = aggfun,
                     show.labels = TRUE)
      )
      
      p2 <- ggplotly(
                qicharts2::qic(x = x, y = y, n = n, data = df, multiply = as.numeric(input$multiple),
                     ylab = paste0("Value per", " ", input$multiple, " patient days"), xlab = xlabel,
                     title = paste0(title1, " (top), ", title2, " (bottom)"),
                     chart = chart2,
                     x.period = xperiod,
                     part = parts,
                     facets = facet,
                     show.labels = TRUE)
      )
      
      output$control_plot <- renderPlotly({
        plotly::subplot(p1, p2, nrows = 2, titleX = T, titleY = T, widths = c(1,0)) %>% 
          plotly::layout(yaxis = list(domain = c(0, 0.35), axis.automargin = T), yaxis2 = list(domain = c(0.65, 1), axis.automargin = T),
                 xaxis = list(domain = c(0, 1), axis.automargin = T), xaxis2 = list(domain = c(0, 1), axis.automargin = T),
                 margin=list(l = 100))

      })
      
    } else {
        output$control_plot <- renderPlotly({
          p <- ggplotly(
            qicharts2::qic(x = x, y = y, n = n, data = df, multiply = as.numeric(input$multiple),
                         ylab = paste0("Value per", " ", input$multiple, " patient days"), xlab = xlabel,
                         title = paste0(input$choose_control_plot, " ", "chart"),
                         chart = input$choose_control_plot,
                         x.period = xperiod,
                         part = parts,
                         facets = facet,
                         agg.fun = aggfun,
                         x.angle = 45,
                         show.labels = TRUE)
          ) %>% layout(margin=list(l = 100))
          if (input$facet) {
            p <- p %>% layout_ggplotly()
          }
          p
      })
    }
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