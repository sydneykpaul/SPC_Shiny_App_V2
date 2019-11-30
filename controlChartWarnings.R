controlChartWarnings <- function(dfbase, dfrows, chart_params) {
  chart_type <- chart_params[["chart_caption"]]
  cc <- chart_params[["cc"]]
  quick_detect <- chart_params[["quick_detect"]]
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
  return(ccWarn)
}
