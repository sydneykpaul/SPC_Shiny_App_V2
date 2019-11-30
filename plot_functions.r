############### Plot functions ###############

## Create HTML table object
createTable <- function(df, pann, showAnnTable) {
  if(pann & showAnnTable) {
    css <- '<style>
    .anndiv {
    height: 100%;
    overflow-y: auto;
    padding-top: 20px;
    }
    
    .anntable {
    border-collapse: collapse;
    font-size: 10pt;
    }
    
    .annrow {
    vertical-align:top;
    border-top:1px solid #666666;
    }
    
    td {
    padding:5px;
    }
    </style>
    '
    tbl <- tags$table(
      tags$tbody(
        apply(
          df[ , c('Ref', 'annotation_text')],
          1, 
          function(x) {
            tags$tr(lapply(x, function(y) tags$td(y)), class='annrow')
            }
          )
      ), class='anntable')
    
    
    thtml <- HTML(
      paste(
        css,
        '<div class ="anndiv">',
        tbl,
        '</div>'
      ))
    
    thtml <- gsub('&gt;', '>', gsub('&lt;', '<', thtml))
  } else {
    thtml <- ''
  }
  return(thtml)
}


## Determine x-axis breaks and labels
xAxisFormat <- function(df, dfrows, chart_type, checkDate, rare_event, fyStart, yIncludeZero) {
  interval <- ceiling(dfrows/8)
  xbrks <- seq(interval, dfrows, interval)
  
  if(chart_type %in% c('I chart', 'g-chart', 't-chart')) {
    xlabels <- df$sg[xbrks]
  } else { 
    xlabels <- df$subgroup_row[xbrks]
  }
  
  ## Date x-axis labels
  if(checkDate == 0 & !rare_event) {
    day_interval <- as.integer(
      mean(
        difftime(
          tail(df$subgroup_row, dfrows - 1), 
          head(df$subgroup_row, dfrows - 1), 
          units = 'days'
        )
      )
    )
    
    ## Yearly x-axis ticks
    if(day_interval >= 360 | (day_interval >= 28 & interval > 3)) {
      if(fyStart == 1) {
        fy_label = ''
        fy_add = 0
      } else {
        fy_label = 'FY'
        fy_add = 1
      }
      year_start <- which(month(df$subgroup_row) == fyStart)
      iy <- ceiling(length(year_start) / 8)
      xbrks <- year_start[seq(iy, length(year_start), iy)]
      xlabels <- paste0(fy_label, year(df$subgroup_row) + fy_add)[xbrks]
    }
    ## Quarterly x-axis ticks
    else if(day_interval >= 28) {
      quarter_start <- which(month(df$subgroup_row) %% 3 == 1)[1]
      xbrks <- seq(quarter_start, dfrows, 3)
      xlabels <- format(df$subgroup_row[xbrks], '%b %Y')
    }
  }
  return(list(xbrks, xlabels))
}

## Create plotly object
spcPlot <- function(df, yIncludeZero, legendPosition, chart_type, laneybool, pann, dfann, secondaryColor, pmean, pcl, prng, pma, pbnch, benchmarkColor, decFormat, ptrg, targetColor, primaryColor, showCaption, ccWarn, xbrks, xlabels, xLabel, yLabel, clMultiplier, subgroup_row, maPeriod) {
  
  ## Initialize plot
  rngmd <- ifelse(yIncludeZero, 'tozero', 'normal')
  btns <- list()
  plann <- list()
  legend_orientation <- ifelse(legendPosition == 'bottom', 'h', 'v')
  legend_show <- ifelse(legendPosition == 'hide', F, T)
  
  ## Adjust chart caption
  chart_caption <- chart_type
  if(laneybool) {
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
  
  # Plot annotations
  if(pann) {
    annmax <- max(df[ ,c('rate', 'ucl')], na.rm = T)
    
    plann <- list(
      x = dfann$sg,
      y = dfann$rate,
      ax = 0,
      ay = annmax * 1.01,
      ayref = 'y',
      arrowhead = 0,
      arrowwidth = 0.2,
      text = dfann$Ref,
      hovertext = dfann$annotation_text
    )
  }
  
  ## Plotly base
  pl <- plot_ly(
    df,
    type = 'scatter', 
    mode = 'lines',
    line = list(color = secondaryColor),
    hoverinfo = 'text'
  )
  
  ## Plot mean
  if(pmean) {
    pl <- pl %>%
      add_trace(
        x = df$sg,
        y = df$mean,
        text = ~paste0('Mean: ', sprintf(decFormat, df$mean)),
        showlegend = F,
        name = 'SPC'
      )
  }
  
  ## Plot control limits
  if(pcl) {
    
    pl <- pl %>%
      add_trace(
        x = df$sg,
        y = df$ucl,
        text = paste0(chart_caption, '\nUCL +', clMultiplier),
        showlegend = F
      ) %>%
      add_trace(
        x = df$sg,
        y = df$lcl,
        text = paste0(chart_caption, '\nLCL -', clMultiplier),
        showlegend = F
      )
  }
  
  ## Plot range chart
  if(prng) {
    ## X-bar and s or I-MR
    if(chart_type == 'X-bar chart') {
      chart1 <- 'X-bar chart'
      chart2 <- 's chart'
    }
    if(chart_type == 'I chart') {
      chart1 <- 'I chart'
      chart2 <- 'MR chart'
    }
    
    ## Chart caption
    if(showCaption) {
      captn1 <- list(
        text = paste0('<i>', chart1, '</i>'),
        showarrow = F,
        xref = 'paper',
        x = 0,
        yref = 'paper',
        y = 1.05
      )
      captn2 <- list(
        text = paste0('<i>', chart2, '</i>'),
        showarrow = F,
        xref = 'paper',
        x = 0,
        yref = 'paper',
        y = 1.05
      )
    }  else {
      captn1 <- list()
      captn2 <- list()
    }
    
    ## Plot s chart or MR chart
    pl <- pl %>%
      add_trace(
        x = df$sg,
        y = df$mean_s,
        text = ~paste0('Mean: ', sprintf(decFormat, df$mean_s)),
        visible = F,
        showlegend = F,
        name = 'SPC'
      ) %>%
      add_trace(
        x = df$sg,
        y = df$ucl_s,
        text = paste0(chart2, '\nUCL +', clMultiplier, ' sigma'),
        visible = F,
        showlegend = F
      ) %>%
      add_trace(
        x = df$sg,
        y = df$lcl_s,
        text = paste0(chart2, '\nLCL -', clMultiplier, ' sigma'),
        visible = F,
        showlegend = F
      ) %>%
      add_trace(
        x = df$sg,
        y = df$rate_s,
        line = list(color = primaryColor),
        text = ~paste0(
          df$subgroup_row,
          '\nRate: ', 
          sprintf(decFormat, df$rate_s)),
        visible = F,
        showlegend = F,
        name = 'Rate'
      )
    
    ## Create buttons to choose chart
    btns <- list(
      list(
        active = 0,
        type = 'buttons',
        xanchor = 'left',
        yanchor = 'bottom',
        y = 0, 
        x = 1.02,
        buttons = list(
          list(
            method = 'update',
            label = paste0('<i>',chart1,'</i>'),
            args = list(
              list(visible = c(T, T, T, T, F, F, F, F)),
              list(annotations = list(captn1, c())))),
          list(
            method = 'update',
            label = paste0('<i>',chart2,'</i>'),
            args = list(
              list(visible = c(F, F, F, F, T, T, T, T)),
              list(annotations = list(c(), captn2))))
        )
      )
    )
    
  }
  
  # Plot moving average
  if(pma) {
    pl <- pl %>%
      add_trace(
        x = df$sg,
        y = df$ma,
        text = paste0(subgroup_row,
                      '\nMoving average: ', 
                      sprintf(decFormat, df$ma),
                      '\n',
                      maPeriod, ' periods'),
        name = paste0(maPeriod, '-period\nmoving average'),
        showlegend = T
      )
  }
  
  # Plot benchmark rate
  if(pbnch) {
    pl <- pl %>%
      add_trace(
        x = df$sg,
        y = df$benchmark_rate,
        line = list(dash = 'dot', color = benchmarkColor),
        name = 'Benchmark',
        text = paste0(
          'Benchmark: ', 
          sprintf(decFormat, df$benchmark_rate)
          ),
        showlegend = T
      )
  }
  
  # Plot target rate
  if(ptrg) {
    pl <- pl %>%
      add_trace(
        x = df$sg,
        y = df$target_rate,
        line = list(dash = 'dot', color = targetColor),
        text = paste0('Target: ', sprintf(decFormat, df$target_rate)),
        name = 'Target',
        showlegend = T
      )
  }
  
  # Add second CUSUM line
  if(chart_type == 'CUSUM chart') {
    pl <- pl %>%
      add_trace(
        x = df$sg,
        y = df$rate2,
        line = list(color = primaryColor),
        text = ~paste0(
          df$subgroup_row,
          '\nRate: ', 
          sprintf(decFormat, df$rate2)),
        showlegend = F
      )
  }
  
  # Plot rate
  pl <- pl %>%
    add_trace(
      x = df$sg,
      y = df$rate,
      mode = 'lines',
      line = list(color = primaryColor),
      text = ~paste0(
        df$subgroup_row,
        '\nRate: ', 
        sprintf(decFormat, df$rate)),
      showlegend = F,
      name = 'Rate'
    )
  
  # Plot caption
  if(showCaption) {
    
    pl <- pl %>% 
      add_annotations(
        text = paste0('<i>', chart_caption, '</i>'),
        showarrow = F,
        xref = 'paper',
        x = 0,
        yref = 'paper',
        y = 1.05,
        hovertext = paste0(
          chart_caption,
          ccWarn
        )
      )
  } else {
  }
  
  # Plot layout
  pl <- pl %>%
    layout(
      margin = list(t = 50),
      xaxis = list(
        tickvals = xbrks, 
        ticktext = xlabels,
        title = xLabel,
        showgrid = F,
        zeroline = F
      ),
      yaxis = list(
        title = yLabel,
        zerolinecolor = '#DEDEDE',
        rangemode = rngmd
      ),
      annotations = plann,
      updatemenus = btns,
      legend = list(
        orientation = legend_orientation
        ),
      showlegend = legend_show
    ) %>%
    config(displayModeBar = T) 
  
  return(pl)
}
#################################################
