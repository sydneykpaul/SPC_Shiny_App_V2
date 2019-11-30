## Create plotly object
spcPlot <- function(df, chart_params, raw_components, ccWarn, dfrows) {

  # Intitialize variables from arguments
  yIncludeZero <- chart_params[["yIncludeZero"]]
  chart_type <- chart_params[["chart_caption"]]
  cc <- chart_params[["cc"]]
  rare_event <- chart_params[["rare_event"]]
  xLabel <- chart_params[["xLabel"]] 
  yLabel <- chart_params[["yLabel"]]
  subgroup <- raw_components[[2]]
  subgroup_row <- raw_components[[3]]
  
  # Initialize plot toggle settings
  pma <- chart_type == 'moving average'    # plot moving average
  pmean <- cc | chart_type == 'run chart'  # plot mean/median

  pcl <- FALSE #plot control limts
  prng <- FALSE #plot standard deviation/range
  
  # temporary for testing TODO: fix
  pann <- sum(!is.na(df$annotation_text)) > 0 # plot annotation
  pbnch <- all(!is.na(df$benchmark_rate))  # plot benchmark
  ptrg <- all(!is.na(df$target_rate))   # plot target

  if (ccWarn == '') {
    pcl <- cc    # plot control limts
    prng <- chart_type %in% c('X-bar chart', 'I chart')   # plot standard deviation/range
    print("ccWarm:")
    print(cc)
    print("prng:")
    print(prng)
  }
  
  check_date_results <- checkDate(subgroup, subgroup_row)
  check_date <- check_date_results[[1]]
  subgroup_row <- check_date_results[[2]]
  
  # Initialize plot
  rngmd <- ifelse(yIncludeZero, 'tozero', 'normal')
  btns <- list()
  plann <- list()
  legend_orientation <- ifelse(LEGEND_POSITION == 'bottom', 'h', 'v')
  legend_show <- ifelse(LEGEND_POSITION == 'hide', F, T)
  
  # Adjust chart caption
  chart_caption <- chart_type
  if(chart_type == 'p\'-chart' | chart_type == 'u\'-chart') {
    if(chart_type=='p-chart') {
      chart_caption <- 'p\'-chart'
    }
    if(chart_type=='u-chart') {
      chart_caption <- 'u\'-chart'
    }
    if(ccWarn != '') {
      chart_caption <= 'run chart*'
    }
  }
  
  # Plot annotations (add annotation table) TODO: add annotations
  thtml <- ''
  if (pann) {
    ann_ind <- !is.na(df$annotation_text)
    dfann <- data.frame(Ref = 1:sum(ann_ind), df[ann_ind, c('annotation_text', 'sg', 'rate')])
    thtml <- createTable(dfann, pann)
    annmax <- max(df[ ,c('rate', 'ucl')], na.rm = T)
    plann <- list(x = dfann$sg,
                  y = dfann$rate,
                  ax = 0,
                  ay = annmax * 1.01,
                  ayref = 'y',
                  arrowhead = 0,
                  arrowwidth = 0.2,
                  text = dfann$Ref,
                  hovertext = dfann$annotation_text)
  }
  twidth <- ifelse (thtml == '', 0, 2)
  
  # Plotly base
  pl <- plot_ly(df,
                type = 'scatter', 
                mode = 'lines',
                line = list(color = SECONDARY_COLOR),
                hoverinfo = 'text')
  
  # Plot mean
  if(pmean) {
    mean_or_median <- "Mean: "
    if (chart_type == 'run chart') {
      mean_or_median <- "Median: "
    }
    
    pl <- pl %>%
      add_trace(x = df$sg,
                y = df$mean,
                text = ~paste0(mean_or_median, sprintf(DECIMAL_FORMAT, df$mean)),
                showlegend = F,
                name = 'SPC')
  }
  
  # Plot control limits
  if(pcl) {
    pl <- pl %>%
      add_trace(x = df$sg,
                y = df$ucl,
                text = paste0(chart_caption, '\nUCL +', CL_MULTIPLIER),
                showlegend = F) %>%
      add_trace(x = df$sg,
                y = df$lcl,
                text = paste0(chart_caption, '\nLCL -', CL_MULTIPLIER),
                showlegend = F)
  }
  
  ## Plot range chart
  if(prng) {
    imr_xbars_results <- plotRangeChart(df, chart_type, pl)
    pl <- imr_xbars_results[[1]]
    btns <- imr_xbars_results[[2]]
  }
  
  # Plot moving average
  if(pma) {
    pl <- pl %>%
      add_trace(x = df$sg,
                y = df$ma,
                text = paste0(subgroup_row, '\nMoving average: ', sprintf(DECIMAL_FORMAT, df$ma), '\n', MA_PERIOD, ' periods'),
                name = paste0(MA_PERIOD, '-period\nmoving average'),
                showlegend = T)
  }
  
  # Plot benchmark rate
  if(pbnch) {
    pl <- pl %>%
      add_trace(x = df$sg,
                y = df$benchmark_rate,
                line = list(dash = 'dot', color = BENCHMARK_COLOR),
                name = 'Benchmark',
                text = paste0('Benchmark: ', sprintf(DECIMAL_FORMAT, df$benchmark_rate)),
                showlegend = T
      )
  }
  
  # Plot target rate
  if(ptrg) {
    pl <- pl %>%
      add_trace(x = df$sg,
                y = df$target_rate,
                line = list(dash = 'dot', color = TARGET_COLOR),
                text = paste0('Target: ', sprintf(DECIMAL_FORMAT, df$target_rate)),
                name = 'Target',
                showlegend = T
      )
  }
  
  # Add second CUSUM line
  if(chart_type == 'CUSUM chart') {
    pl <- pl %>%
      add_trace(x = df$sg,
                y = df$rate2,
                line = list(color = PRIMARY_COLOR),
                text = ~paste0(df$subgroup_row, '\nRate: ', sprintf(DECIMAL_FORMAT, df$rate2)),
                showlegend = F)
  }
  
  # Plot rate
  pl <- pl %>%
    add_trace(x = df$sg,
              y = df$rate,
              mode = 'lines',
              line = list(color = PRIMARY_COLOR),
              text = ~paste0(df$subgroup_row, '\nRate: ', sprintf(DECIMAL_FORMAT, df$rate)),
              showlegend = F,
              name = 'Rate')
  
  # Plot caption
  if(SHOW_CAPTION) {
    pl <- pl %>% 
      add_annotations(text = paste0('<i>', chart_caption, '</i>'),
                      showarrow = F,
                      xref = 'paper',
                      x = 0,
                      yref = 'paper',
                      y = 1.05,
                      hovertext = paste0(chart_caption, ccWarn))
  }
  
  # Plot layout
  ## Add x-axis breaks and labels
  xbl <- xAxisFormat(df, dfrows, chart_type, check_date, rare_event, yIncludeZero)
  xbrks <- xbl[[1]]
  xlabels <- xbl[[2]]
  
  pl <- pl %>%
    layout(margin = list(t = 50),
           xaxis = list(tickvals = xbrks, 
                        ticktext = xlabels,
                        title = xLabel,
                        showgrid = F,
                        zeroline = F),
           yaxis = list(title = yLabel,
                        zerolinecolor = '#DEDEDE',
                        rangemode = rngmd),
           annotations = plann,
           updatemenus = btns,
           legend = list(orientation = legend_orientation),
           showlegend = legend_show) %>%
    config(displayModeBar = T) 
  

  return(pl)
}