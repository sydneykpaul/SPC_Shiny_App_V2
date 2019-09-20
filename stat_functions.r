############### Stat functions ###############
# chart_list <- c(
#   'run chart', 
#   'p-chart', 
#   'u-chart',
#   'X-bar chart', 
#   'I-MR chart',
#   'g-chart',
#   't-chart',
#   'moving average',
#   'EWMA chart',
#   'CUSUM chart'
# )

## SPC constants 
## source: https://github.com/anhoej/qicharts2/blob/master/R/helper.functions.R
c4 <- function(n) {
  n[n <= 1] <- NA
  sqrt(2 / (n - 1)) * exp(lgamma(n / 2) - lgamma((n - 1) / 2))
}
 
c5 <- function(n) {
  n[n <= 1] <- NA
  sqrt(1 - c4(n) ^ 2)
}

# a3 <- function(n) {
#   n[n <= 1] <- NA
#   #3 / (c4(n) * sqrt(n))
# }

b3 <- function(n) {
  n[n <= 1] <- NA
  pmax(0, 1 - 3 * c5(n) / c4(n))
}

b4 <- function(n) {
  n[n <= 1] <- NA
  1 + 3 * c5(n) / c4(n)
}

## Group raw data
groupData <- function(df) {

# if(rare_event) { #TODO: get inputs into function
    if(F) {
    gdf <- df
    gdf$i <- 1:nrow(gdf)
    gdf <- gdf[rep(1:nrow(gdf), gdf$num_count + 1) , ]
    gdf$dc <- gdf$denom_count / (gdf$num_count + 1)
    gdf$nc <- 0
    newrows <- grep('\\.', rownames(gdf))
    gdf$nc[newrows - 1] <- 1
    gdf$evnt <- cumsum(gdf$nc)
    gdf <- gdf[gdf$evnt > 0, ]

    i <- aggregate(gdf$i, list(gdf$evnt), min)[ , -1]
    num_count <- aggregate(gdf$dc, list(gdf$evnt), sum)[ , -1]
    dfout <- data.frame(
      subgroup_row = df[i, 'subgroup_row'],
      num_count,
      denom_count = 1,
      df[i, c('standard_deviation', 'benchmark_rate', 'target_rate', 'annotation_text')]
    )

  } else {
    aggregate(
      df[ , c('num_count', 'denom_count')],
      by = list(subgroup_row = df$subgroup_row),
      FUN = sum
    )

    aggregate(
      df[ , c('standard_deviation', 'benchmark_rate', 'target_rate')],
      by = list(df$subgroup_row),
      FUN = max
    )

    aggregate(
      df[ , c('standard_deviation', 'benchmark_rate', 'target_rate')],
      by = list(df$subgroup_row),
      FUN = max
    )[ , -1]
    
    annotation_text = aggregate(
      df[ , c('annotation_text')],
      by = list(df$subgroup_row),
      FUN = function(x) {max(as.character(x))}
    )[ , -1]

    dfout <-
    data.frame(
      aggregate(
        df[ , c('num_count', 'denom_count')],
        by = list(subgroup_row = df$subgroup_row),
        FUN = sum
      ),
      aggregate(
        df[ , c('standard_deviation', 'benchmark_rate', 'target_rate')],
        by = list(df$subgroup_row),
        FUN = max
      )[ , -1],
      # annotation_text = aggregate(
      #   annotation_text,
      #   by = list(df$subgroup_row),
      #   FUN = function(x) {max(as.character(x))}
      #   )[, -1]
      annotation_text = annotation_text
    )
  }
  return(dfout)
}

## Group count
groupCount <- function(grp) {
  aggregate(rep(1,length(grp)), by = list(grp = grp), sum)[,-1]
}

## Calculate data group mean
gmean <- function(df, rateMultiplier) {
  sum(df$num_count) / sum(df$denom_count) * rateMultiplier
}

## Calculate data group mrbar
gmrbar <- function(mr) {
  mean(mr[mr<=3.27*mean(cbind(mr), na.rm = T)], na.rm = T)
}

## Calculate data group sbar
gsbar <- function(df) {
  sqrt(sum((df$denom_count - 1) * df$standard_deviation ^ 2) /
         (sum(df$denom_count) - nrow(df)))
}

## Calculate data group sigma_z for Laney adjustment
gsigmaz <- function(df, rateMultiplier) {
  mean(
    abs(c(
      diff(
        (df$rate - df$mean) / (df$sbar / sqrt(df$denom_count / rateMultiplier)))
      , NaN)
    ) , na.rm = T) / 1.128
}

## Calculate breakpoints
findbp <- function(df) {
  brk <- rep(0, nrow(df))
  hcalc <- round(log2(nrow(df)) + 3) #Anh?j rule for longest run
  bp <- tryCatch(
    breakpoints(df$num_count ~ df$denom_count, h = hcalc)$breakpoints,
    error = function(x) NA
  )
  brk[bp] <- 1
  cumsum(brk) - brk
}

# from qcc::qcc.overdispersion.test but edited
overdispersion.test <- function (y, n, chart_type) {
  type <- ifelse(chart_type == 'u-chart' | chart_type == 'u\'-chart', "poisson", "binomial")
  len_y <- length(y)
  obs.var <- var(x = y)
  
  if (type == "binomial") {
    p <- sum(y)/sum(n)
    theor.var <- mean(n) * p * (1 - p)
  }
  else if (type == "poisson") {
    theor.var <- mean(y)
  }
  
  D <- (obs.var * (len_y - 1))/theor.var
  p.value <- 1 - pchisq(D, len_y - 1)
  out <- matrix(c(obs.var/theor.var, D, signif(p.value, 5)), 1, 3)
  
  rownames(out) <- paste(type, "data")
  colnames(out) <- c("Obs.Var/Theor.Var", "Statistic", "p_value")
  names(dimnames(out)) <- c(paste("Overdispersion test"), "")
  
  return(as.data.frame(out))
}

#################################################
