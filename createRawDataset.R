createRawDataset <- function(already_subgrouped, x, y, n, subgroup_on, benchmark, target, user_annotations) {
  
  # Load subgroup data
  if (already_subgrouped | is.null(subgroup_on)) {
    x <- as.POSIXct(x)
    subgroup <- x
    
    # Convert to date/datetime if appropriate
    subgroup_row <- as_datetime(gsub('.0000000', '', subgroup))

    check_date_results <- checkDate(subgroup, subgroup_row)
    check_date <- check_date_results[[1]]
    subgroup_row <- check_date_results[[2]]
    
  } else{
    x <- as.POSIXct(x)
    aggregated <- aggregate(y, list(day = cut(x, subgroup_on)), sum)
    
    subgroup <- as.POSIXct(aggregated[[1]])
    y <- aggregated[[2]]
    
    n <- aggregate(n, list(day = cut(x, subgroup_on)), sum)[[2]]
    
    # Convert to date/datetime if appropriate
    subgroup_row <- as_datetime(gsub('.0000000', '', subgroup))
    
    check_date_results <- checkDate(subgroup, subgroup_row)
    check_date <- check_date_results[[1]]
    subgroup_row <- check_date_results[[2]]
  }
  
  # Load numerator data
  valid_rows <- rep(T, length(subgroup_row))
  num_count <- y
  valid_rows <- valid_rows & !is.na(num_count)
  
  # Load denominator data
  denom_count <- n  # if no denominator, n is column of ones
  valid_rows <- (valid_rows) & (denom_count > 0) & (!is.na(denom_count))
  
  # Remove invalid data rows
  denom_count <- denom_count[valid_rows]
  num_count <- num_count[valid_rows]
  subgroup_row <- subgroup_row[valid_rows]
  
  # Load standard deviation data  
  standarddeviation = sqrt(y/n)
  standard_deviation <- standarddeviation[valid_rows]
  # if (length(valid_rows) != length(standarddeviation)) {
  #   standard_deviation <- standarddeviation[valid_rows]
  # } else {
  #   standard_deviation <- standarddeviation
  # }
  
  # Load annotation data
  annotation = user_annotations
  annotation_text <- annotation[valid_rows]
  annotation_text[annotation_text == ''] <- NA
  
  if (sum(!is.na(annotation_text)) == 0) {
    annotation_text <- cbind(rep(NA, length(subgroup_row)))
  }
  
  pann <- sum(!is.na(annotation_text)) > 0
  
  


  # Load benchmark data
  if (!is.null(benchmark)) {
    benchmark = cbind(rep(benchmark, length(subgroup_row)))
    benchmark_rate <- benchmark[valid_rows, 1]    
  } else {
    benchmark = cbind(rep(NA, length(subgroup_row)))
    benchmark_rate <- benchmark[valid_rows, 1]        
  }

  # Load target data
  if (!is.null(target)) {
    target = cbind(rep(target, length(subgroup_row)))
    target_rate <- target[valid_rows, 1]
  } else {
    target = cbind(rep(NA, length(subgroup_row)))
    target_rate <- target[valid_rows, 1]
  }

  # Combine raw data into data frame
  dfraw <- data.frame(subgroup_row,
                      num_count,
                      denom_count, 
                      standard_deviation,
                      annotation_text,
                      benchmark_rate,
                      target_rate,
                      stringsAsFactors = FALSE)

  

  return(list(dfraw, subgroup, subgroup_row))
}