source('stat_functions.r')
source('plot_functions.r')


spcData <- function(df, rateMultiplier, sdType, chart_type, cc, dfrows, bpbool, CL_MULTIPLIER, MA_PERIOD, laneybool) {
  dfrows <- nrow(df)
  grp <- rep(0, dfrows)
  
  if (bpbool) {
    grp <- findbp(df)
  }
  
  gcount <- groupCount(grp)
   
  rate <- (df$num_count / df$denom_count) * rateMultiplier
  mean <- rep(sapply(split(df, grp), gmean, rateMultiplier), gcount)
  
  sbar <- rep(NaN, dfrows)
  
  if (sdType == 'normal') {
    sbar <- rep(sapply(split(df, grp), gsbar), gcount)
  } else if (sdType == 'poisson') {
    sbar <- sqrt(mean)
  } else if (sdType == 'binomial') {
    sbar <- sqrt(mean*(1-mean))
  }
  sbar_adj <- sqrt(df$denom_count/rateMultiplier)
  
  df <- data.frame(
    sg = 1:nrow(df), 
    df, 
    rate, 
    mean, 
    sbar,
    ucl = NaN,
    lcl = NaN,
    rate_s = NaN,
    mean_s = NaN,
    ucl_s = NaN,
    lcl_s = NaN,
    ma = NaN
  )
  
  uclmin <- ifelse (sum(df$rate < 0) > 0, -Inf, 0)
  uclmax <- Inf
  
  if (chart_type == 'run chart') {
    sigma <- rep(NaN, dfrows)
  }
  else if (chart_type == 'p-chart' | chart_type == 'p\'-chart') {
    sigma_z <- 1
    if (laneybool) {
      sigma_z <- rep(sapply(split(df, grp), gsigmaz, rateMultiplier), gcount)
    }
    sigma <- sigma_z * df$sbar / sbar_adj
    uclmin <- 0
    uclmax <- ifelse (rateMultiplier == 100, 100, 1)
  }
  else if (chart_type == 'u-chart' | chart_type == 'u\'-chart') {
    sigma_z <- 1
    if (laneybool) {
      sigma_z <- rep(sapply(split(df, grp), gsigmaz, rateMultiplier), gcount)
    }
    sigma <- sigma_z * df$sbar / sbar_adj
  }
  else if (chart_type == 'X-bar chart') {
    sigma <- df$sbar / (c4(df$denom_count) * sbar_adj)
    
    df$rate_s <- df$standard_deviation
    df$mean_s <- df$sbar
    df$ucl_s <- b4(df$denom_count)*sbar
    df$lcl_s <- pmax(0, b3(df$denom_count)*sbar)
    
    df$ucl_s[dfrows] = NaN
    df$lcl_s[dfrows] = NaN
  }
  else if (chart_type == 'I chart') {
    mr <- c(abs(diff(df$rate)), NaN)
    df$rate_s <- mr
    df$mean_s <- rep(sapply(split(mr, grp), gmrbar), gcount)
    sigma <- df$mean_s/1.128
    
    df$ucl_s <- pmin(uclmax, 3.267*df$mean_s)
    df$lcl_s = NaN
    df$ucl_s[dfrows] = NaN
  }
  else if (chart_type == 'g-chart') {
    sigma <- sqrt(df$mean*(df$mean + 1))
  }
  else if (chart_type == 't-chart') {
    df$num_count <- df$num_count ^ (1/3.6)
    df$rate <- df$num_count
    df$mean <- rep(sapply(split(df, grp), gmean, rateMultiplier), gcount)
    mr <- c(abs(diff(df$rate)), NaN)
    mrbar <- rep(sapply(split(mr, grp), gmrbar), gcount)
    sigma <- mrbar/1.128
  }
  else if (chart_type == 'moving average') {
    df$ma <- zoo::rollsum(df$num_count, MA_PERIOD, fill = NA, align = 'right') / 
      zoo::rollsum(df$denom_count, MA_PERIOD, fill = NA, align = 'right') * rateMultiplier
  }
  else if (chart_type == 'EWMA chart') {
    lambda <- 0.2
    seq <- as.vector(sapply(gcount, function(x){1:gcount}))
    ewma <- rep(0, dfrows)
    ewmafirst <- seq == 1
    ewma[ewmafirst] <- df$mean[ewmafirst]
    for(i in (1:dfrows)[!ewmafirst]) {
      ewma[i] <- lambda * df$rate[i] + (1 - lambda) * ewma[i-1]
    }
    df$rate <- ewma
    sigma <- 
      (df$sbar / sbar_adj) *
      sqrt(lambda/(2-lambda)) * 
      sqrt(1 - ((1 - lambda)^(2*seq)))
  }
  else if (chart_type == 'CUSUM chart') {
    slack = 0.5
    seq <- as.vector(sapply(gcount, function(x){1:gcount}))
    cusumfirst <- seq == 1
    zscore <- (df$rate - df$mean) / (df$sbar / sbar_adj)
    cusuml <- rep(0, dfrows)
    cusuml[cusumfirst] <- -pmax(0, -zscore[cusumfirst] - slack)
    cusumh <- rep(0, dfrows)
    cusumh[cusumfirst] <- pmax(0, zscore[cusumfirst] - slack)
    
    for(i in (1:dfrows)[!cusumfirst]) {
      cusuml[i] <- -max(0, -zscore[i] - slack + cusuml[i-1]) 
      cusumh[i] <- max(0, zscore[i] - slack + cusumh[i-1])
    }
    
    df$rate <- cusuml
    df$rate2 <- cusumh
    
    df$mean <- 0
    sigma <- rep(1, dfrows)
    CL_MULTIPLIER <- CL_MULTIPLIER + 1
  }
  
  if (cc) {
    df$ucl = pmin(uclmax, df$mean + CL_MULTIPLIER*sigma)
    df$lcl = pmax(uclmin, df$mean - CL_MULTIPLIER*sigma)
    
    df$ucl[dfrows] = NaN
    df$lcl[dfrows] = NaN
  }

  df
}

dataToChart <- function(df, 
                        chart_type,
                        xLabel,
                        yLabel,
                        multiple,
                        already_subgrouped = TRUE,
                        subgroup_on = NULL,
                        should_break = FALSE,
                        groupedBy = FALSE) {
  
  ###################################### SET UP ######################################
  
  ## Initialize constants
  PRIMARY_COLOR <- "#0090b4"   # Primary line color
  SECONDARY_COLOR <- "#b3b3b3" # Secondary line color
  BENCHMARK_COLOR <- "#b3b3b3" # Benchmark line color
  TARGET_COLOR <- "#3F526F"    # Target line color
  
  DECIMAL_PLACES <- 2                                 # Numeric display decimal places
  DECIMAL_FORMAT <- paste0('%.',DECIMAL_PLACES, 'f')  # Format display decimal places

  CC_GUIDELINES <- "strict"   # Apply control chart guidelines (other option is 'relaxed')
  CL_MULTIPLIER <- 3          # Control limit multiplier
  MA_PERIOD <- 12             # Moving average periods
  FY_START <- 1               # FY start month
  LEGEND_POSITION <- "right"  # Legend position
  SHOW_CAPTION <- TRUE        # Show chart caption
  SHOW_ANN_TABLE = TRUE       # Show annotation table
  Y_INCLUDE_ZERO <- FALSE     # Y axis include zero

  
  # Initialize plot toggle settings
  pann <- FALSE   # plot annotation
  pcl <- FALSE    # plot control limts
  pma <- FALSE    # plot moving average
  pmean <- FALSE  # plot mean
  prng <- FALSE   # plot standard deviation/range
  pbnch <- FALSE  # plot benchmark
  ptrg <- FALSE   # plot target
  
  
  # Initialize user input variables
  x <- df$x
  y <- df$y
  n <- df$n

  chart_caption <- chart_type
  rateMultiplier <- as.numeric(multiple) 
  already_subgrouped <- as.logical(already_subgrouped)
  bpbool <- as.logical(should_break)  # changepoints boolean
  
  
  # Initialize chart specific variables
  shewhart <- chart_type %in% c('u-chart', 'p-chart', 'X-bar chart', 'I chart') # Traditional Shewhart charts
  rare_event <- chart_type %in% c('g-chart', 't-chart')                         # Rare event charts
  quick_detect <- chart_type %in% c('EWMA chart', 'CUSUM chart')                # Quicker detection charts
  
  cc <- shewhart | rare_event | quick_detect
  pmean <- cc | chart_type == 'run chart'
  pma <- chart_type == 'moving average'
  prng <- chart_type %in% c('X-bar chart', 'I chart')
  
  if (chart_type == 'p-chart' | chart_type == 'p\'-chart') {
    sdType <- 'binomial'
  } else if (chart_type == 'u-chart' | chart_type == 'u\'-chart') {
    sdType <- 'poisson'
  } else {
    sdType <- 'normal'
  }
  
  if (chart_type == 'p\'-chart' | chart_type == 'u\'-chart') {
    laneybool <- TRUE # Laney p'/u' adjustment boolean
  } else {
    laneybool <- FALSE
  }
  
  
  ###################################### Create the raw dataset ######################################
  
  # Load subgroup data
  if (already_subgrouped) {
    subgroup <- x
    
    # Convert to date/datetime if appropriate
    subgroup_row <- as_datetime(gsub('.0000000', '', subgroup))
    
    if (sum(is.na(subgroup_row)) > 0) {
      subgroup_row <- as.character(subgroup)
      checkDate <- -1
      
    } else {
      checkDate <- max(hour(subgroup_row) + minute(subgroup_row))
      if (checkDate > 0) {
        subgroup_row <- as_datetime(subgroup_row)
      } else if (checkDate == 0) {
        subgroup_row <- as_date(subgroup_row)
      } else {
        subgroup_row <- as.character(subgroup[, 1])
      }
    }
    
  } else{
    # message('Subgroup required\n') # TODO: create subgroups?
    aggregated <- aggregate(y, list(day = cut(x, subgroup_on)), mean) # mean/median/sum??
    
    subgroup <- as.POSIXct(aggregated[[1]])
    y <- aggregated[[2]]
    
    print(y)
    print(subgroup)
    # Convert to date/datetime if appropriate
    subgroup_row <- as_datetime(gsub('.0000000', '', subgroup))
    
    if (sum(is.na(subgroup_row)) > 0) {
      subgroup_row <- as.character(subgroup)
      checkDate <- -1
      
    } else {
      checkDate <- max(hour(subgroup_row) + minute(subgroup_row))
      if (checkDate > 0) {
        subgroup_row <- as_datetime(subgroup_row)
      } else if (checkDate == 0) {
        subgroup_row <- as_date(subgroup_row)
      } else {
        subgroup_row <- as.character(subgroup[, 1])
      }
    }
  }

  # Load numerator data
  valid_rows <- rep(T, length(subgroup_row))
  num_count <- y
  valid_rows <- valid_rows & !is.na(num_count)
  
  # Load denominator data
  denom_count <- n  # if no denominator, n is column of ones
  valid_rows <- valid_rows & denom_count > 0 & !is.na(denom_count)
  
  # Remove invalid data rows
  denom_count <- denom_count[valid_rows]
  num_count <- num_count[valid_rows]
  subgroup_row <- subgroup_row[valid_rows]
  
  # Load standard deviation data  
  standarddeviation = sqrt(y/n)
  if (length(valid_rows == length(standarddeviation))) {
    standard_deviation <- standarddeviation[valid_rows]
  } else {
    standard_deviation <- standarddeviation
  }
  
  # Load annotation data
  annotation = cbind(rep(NA, length(subgroup_row)))
  annotation[10] = 'Annotation 1' # TODO: user provided somehow?
  annotation[11] = 'Annotation 2'
  annotation[20] = 'Annotation 3'
  annotation_text <- annotation[valid_rows, 1]
  annotation_text[annotation_text == ''] <- NA
  if (sum(!is.na(annotation_text)) > 0) {
    pann <- TRUE
  }
  else {
    annotation_text <- rep(NA, length(subgroup_row))
  }
  
  # Load benchmark data
  benchmark = cbind(rep(15, length(subgroup_row))) # TODO: get value from user
  benchmark_rate <- benchmark[valid_rows, 1]
  pbnch <- TRUE
  
  # Load target data
  target = cbind(rep(10, length(subgroup_row))) # TODO: get value from user
  target_rate <- target[valid_rows, 1]
  ptrg <- TRUE
  
  # Combine raw data into data frame
  dfraw <- data.frame(subgroup_row,
                      num_count, 
                      denom_count, 
                      standard_deviation,
                      annotation_text,
                      benchmark_rate,
                      target_rate,
                      stringsAsFactors = FALSE)
  
  # Group data
  dfbase <- groupData(dfraw)
  dfrows <- nrow(dfbase)
  
  # Apply control chart guidelines
  ccWarn <- ''
  if (cc) {
    pcl <- T
    if (!quick_detect) {
      if (CC_GUIDELINES == 'strict') {
        if (dfrows < 20) {
          ccWarn <- paste0(ccWarn, '\n<20 data points')
          pcl <- FALSE
          prng <- FALSE
        }
        if (sum(dfbase$num_count == 0) / dfrows > 0.5
           & chart_type %in% c('p-chart', 'u-chart')) {
          ccWarn <- paste0(ccWarn, '\n>50% subgroup rates = 0')
          pcl <- FALSE
        }
      }
      if (CC_GUIDELINES == 'relaxed') {
        if (dfrows < 10) {
          ccWarn <- paste0(ccWarn, '\n<10 data points')
          pcl <- FALSE
          prng <- FALSE
        }
        if (sum(dfbase$num_count == 0) / dfrows > 0.75
           & chart_type %in% c('p-chart', 'u-chart')) {
          ccWarn <- paste0(ccWarn, '\n>75% subgroup rates = 0')
          pcl <- FALSE
        }
      }
    }
  }
  
  df <- spcData(dfbase, rateMultiplier, sdType, chart_type, cc, dfrows, bpbool, CL_MULTIPLIER, MA_PERIOD, laneybool)

  
  ###################################### Create plot ######################################
  
  ## Add annotation table
  thtml <- ''
  if (pann) {
    ann_ind <- !is.na(df$annotation_text)
    
    dfann <- data.frame(
      Ref = 1:sum(ann_ind),
      df[ann_ind, c('annotation_text', 'sg', 'rate')]
    )
    thtml <- createTable(dfann, pann, SHOW_ANN_TABLE)
  }
  twidth <- ifelse (thtml == '', 0, 2)
  
  ## Add x-axis breaks and labels
  xbl <- xAxisFormat(df, dfrows, chart_type, checkDate, rare_event, FY_START, Y_INCLUDE_ZERO)
  xbrks <- xbl[[1]]
  xlabels <- xbl[[2]]
  
  # Create plot
  pl <- spcPlot(df, Y_INCLUDE_ZERO, LEGEND_POSITION, chart_type, laneybool, pann, dfann, SECONDARY_COLOR, pmean, pcl, prng, pma, pbnch, BENCHMARK_COLOR, DECIMAL_FORMAT, ptrg, TARGET_COLOR, PRIMARY_COLOR, SHOW_CAPTION, ccWarn, xbrks, xlabels, xLabel, yLabel, CL_MULTIPLIER, subgroup_row, MA_PERIOD)
  
  return(pl)
}
