spcData <- function(df, chart_params, break_points) {
  rateMultiplier <- chart_params[["rateMultiplier"]]
  chart_type <- chart_params[["chart_caption"]]
  cc <- chart_params[["cc"]]
  bpbool <- chart_params[["bpbool"]]
  
  dfrows <- nrow(df) 
  grp <- rep(0, dfrows)

  if (bpbool) {
    grp <- findbp(df, break_points)
  }
  
  gcount <- groupCount(grp)
  
  rate <- (df$num_count / df$denom_count) * rateMultiplier
  mean <- rep(sapply(split(df, grp), gmean, rateMultiplier), gcount)
  
  sbar <- calculateSbar(chart_type, df, grp, gsbar, gcount, mean)
  sbar_adj <- sqrt(df$denom_count/rateMultiplier)
  
  df <- data.frame(sg = 1:nrow(df), 
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
                   ma = NaN)
  
  # TODO: prevent LCL going negative
  uclmin <- ifelse (sum(df$rate < 0) > 0, -Inf, 0)
  uclmax <- Inf
  
  if (chart_type == 'run chart') {
    sigma <- rep(NaN, dfrows)
    df$mean <- rep(sapply(split(df, grp), gmedian, rateMultiplier), gcount)
  }
  
  else if (chart_type == 'p-chart' | chart_type == 'p\'-chart') {
    sigma_z <- 1
    if (chart_type == 'p\'-chart' | chart_type == 'u\'-chart') {
      sigma_z <- rep(sapply(split(df, grp), gsigmaz, rateMultiplier), gcount)
    }
    sigma <- sigma_z * df$sbar / sbar_adj
    uclmin <- 0
    uclmax <- ifelse (rateMultiplier == 100, 100, 1)
  }
  
  else if (chart_type == 'u-chart' | chart_type == 'u\'-chart') {
    sigma_z <- 1
    if (chart_type == 'p\'-chart' | chart_type == 'u\'-chart') {
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
  
  return(df)
}