source('stat_functions.r')
source('plot_helper_functions.r')
source('controlChartWarnings.R')
source('createRawDataset.R')
source('checkDate.R')
source('spcData.R')
source('plotRangeChart.R')
source('spcPlot.R')

runChartSummary <- function(df, 
                            chart_type,
                            xLabel,
                            yLabel,
                            multiple,
                            already_subgrouped = TRUE,
                            subgroup_on = "month",
                            should_break = FALSE,
                            break_points = NA,
                            facet_column = NULL,
                            y_negative = TRUE,
                            benchmark = NULL,
                            target = NULL,
                            annotations = NA
) {
  ###################################### SET UP ######################################
  
  ## Initialize constants
  source('constants.R')
  
  # Initialize user input variables
  x <- df$x
  y <- df$y
  n <- df$n
  user_annotations <- annotations
  yIncludeZero <- as.logical(y_negative)     # Y axis include zero TODO: make user input
  
  chart_caption <- chart_type
  rateMultiplier <- as.numeric(multiple) 
  already_subgrouped <- as.logical(already_subgrouped)
  bpbool <- as.logical(should_break)  # changepoints boolean
  
  
  # Initialize chart specific variables
  shewhart <- chart_type %in% c('u-chart', 'p-chart', 'X-bar chart', 'I chart') # Traditional Shewhart charts
  rare_event <- chart_type %in% c('g-chart', 't-chart')                         # Rare event charts
  quick_detect <- chart_type %in% c('EWMA chart', 'CUSUM chart')                # Quicker detection charts
  prime <- chart_type %in% c('p\'-chart', 'u\'-chart')                          # Prime charts
  
  cc <- shewhart | rare_event | quick_detect | prime
  
  chart_params <- data.frame(chart_caption, rateMultiplier, bpbool,
                             shewhart, rare_event, quick_detect, cc,
                             xLabel, yLabel)
  
  
  # Create the raw dataset
  raw_components <- createRawDataset(already_subgrouped, x, y, n, subgroup_on, benchmark, target, user_annotations) # returns dfraw, subgroup, subgroup_row
  dfraw <- raw_components[[1]]
  
  # Group data
  dfbase <- groupData(dfraw, chart_params[["rare_event"]])
  dfrows <- nrow(dfbase)
  
  # Apply control chart guidelines
  ccWarn <- controlChartWarnings(dfbase, dfrows, chart_params)
  
  # Create dataframe
  df <- spcData(dfbase, chart_params, break_points)
  
  
  #####  Run chart summary ######
  
  # already calculated
  lcl <- df$lcl
  cl <- df$mean
  ucl <- df$ucl
  y <- df$df$y
  
  # need to calculate (formulas from qicharts2)
  n_obs <- length(y)
  
  runs <- sign(y - cl)
  runs <- runs[runs != 0 & !is.na(runs)]
  run_lengths <- rle(runs)$lengths
  n_runs <- length(run_lengths)
  
  longest_run <- max(run_lengths)
  longest_run.max <- round(log2(n_useful)) + 3  # Schilling 2012
  
  n_crossings <- max(n.runs - 1, 0)
  n_crossings_min <- stats::qbinom(0.05, max(n.useful - 1, 0), 0.5) # Chen 2010 (7)
  
  runs_signal <- longest_run > longest_run_max || n_crossings < n_crossings_min
  sigma_signal <- y > ucl | y < lcl
  
  
  summary_df <- data.frame(lcl = lcl,
                           cl = cl, 
                           ucl = ucl, 
                           n_obs = n_obs, 
                           n_useful = n_useful,
                           longest_run = longest_run, 
                           longest_run_max = longest_run_max,
                           n_crossings = n_crossings,
                           n_crossings_min = n_crossings_min,
                           runs_signal = runs_signal,
                           sigma.signal = sigma_signal)
}