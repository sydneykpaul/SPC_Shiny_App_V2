checkDate <- function(subgroup, subgroup_row) {
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
  

  return(list(checkDate, subgroup_row))
}

