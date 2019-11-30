plotRangeChart <- function(df, chart_type, pl) {
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
  if(SHOW_CAPTION) {
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
      text = ~paste0('Mean: ', sprintf(DECIMAL_FORMAT, df$mean_s)),
      visible = F,
      showlegend = F,
      name = 'SPC'
    ) %>%
    add_trace(
      x = df$sg,
      y = df$ucl_s,
      text = paste0(chart2, '\nUCL +', CL_MULTIPLIER, ' sigma'),
      visible = F,
      showlegend = F
    ) %>%
    add_trace(
      x = df$sg,
      y = df$lcl_s,
      text = paste0(chart2, '\nLCL -', CL_MULTIPLIER, ' sigma'),
      visible = F,
      showlegend = F
    ) %>%
    add_trace(
      x = df$sg,
      y = df$rate_s,
      line = list(color = PRIMARY_COLOR),
      text = ~paste0(
        df$subgroup_row,
        '\nRate: ', 
        sprintf(DECIMAL_FORMAT, df$rate_s)),
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
  return(list(pl, btns))
}