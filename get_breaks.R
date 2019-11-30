get_breaks <- function(df, 
                       break_column,
                       x_col,
                       break_date = NULL, 
                       already_grouped = TRUE,  
                       subgroup_on = "month") {
  
  # catch always NULL at start
  if (is.null(break_column)) {
    return (NULL)
  }
  
  # break on user input date
  if (break_column == "Choose date on calendar") {
    if (!is.null(break_date)) {
      if (already_grouped) {
        dataDates <- as.Date(arrange_at(df, x_col)[[x_col]])
        breaks <- which.min(abs(dataDates - as.Date(break_date)))
        
      } else if (is.null(subgroup_on)) {
        return(NULL)
      } else {
        only_dates <- as_datetime(arrange_at(df, x_col)[[x_col]])
        changed_dates <- cut.POSIXt(only_dates, subgroup_on) %>% unique()
        
        breaks <- which.min(abs(as.Date(changed_dates) - as.Date(break_date)))
      }
    } else {
      return(NULL) # user hasn't picked date yet
    }
    
  # break on column
  } else {
    cutoffDates <- (df %>% group_by_at(break_column) %>% summarise_at(x_col, max) %>% arrange_at(x_col))[[x_col]]
    
    if (already_grouped) {
      dataDates <- arrange_at(df, x_col)[[x_col]]
      
    } else if (is.null(subgroup_on)) {
      return(NULL)
      
    } else {
      cutoffDates <- cut.POSIXt(as_datetime(cutoffDates), subgroup_on)
      dataDates <- (df %>% mutate_at(x_col, cut.POSIXt, subgroup_on) %>% group_by_at(x_col) %>% summarise_at(break_column, length) %>% arrange_at(x_col))[[x_col]]
    }
    
    breaks <- which(dataDates %in% cutoffDates)
  }
  
  return(breaks) 
}