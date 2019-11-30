############### Plot helper functions ###############

## Create HTML table object
createTable <- function(df, pann) {
  if(pann & SHOW_ANN_TABLE) {
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
xAxisFormat <- function(df, dfrows, chart_type, checkDate, rare_event, yIncludeZero) {
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
      if(FY_START == 1) {
        fy_label = ''
        fy_add = 0
      } else {
        fy_label = 'FY'
        fy_add = 1
      }
      year_start <- which(month(df$subgroup_row) == FY_START)
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


#################################################
