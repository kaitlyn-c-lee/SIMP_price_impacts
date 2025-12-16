#######################################################################################################  
###################################  Impact Estimation and Plotting ###################################
# This function inputs a forecast with confidence intervals, compares to observed data, 
# and calculates the difference between them with confidence intervals on the difference. 
# Quantity-weighting across time steps (vector of length T) is available. 
# Plots are then made for time series of forecasts and observed series
#######################################################################################################
ImpactEstimates <-function(y, point.fcast, simulations, fitted.vals, dates,
                      int.date, weights=NULL, alpha=0.05){
  
  ### param checks
  if(class(y) != "ts" & !is.numeric(y)) stop("y must be numeric or ts")
  if(!is.ts(y)){
    y <- ts(y, frequency = findfrequency(y))
  }
  if(!any(class(dates) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`dates` must be a vector of class Date")
  if(length(dates) != length(y)) stop("length(dates) != length(y)")
  if(length(int.date) != 1 || !any(class(dates) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`int.date` must be a Date of length 1")

  ### STEP 1. Subsetting the data: before and after the intervention date
  ind<-dates>=int.date
  y.00<-ts(y[!ind], frequency = frequency(y))
  y.01<-ts(y[ind], frequency = frequency(y))
  
  
  ### STEP 2. Estimate impact, CI, and calculate p-values
  diff <- y.01 - point.fcast
  
  estimate <- weighted.mean(diff, weights)
  tau.dist <- as.numeric(y.01) - simulations
  
  # Calculate weighted avg. bootstrapped CI
  avg.tau.dist <- as.matrix(apply(tau.dist, 2, FUN = function(x)(sum(x*weights)/sum(weights))))
  
  avg.tau.low <- as.matrix(apply(avg.tau.dist, 2, FUN = function(x)(quantile(x,prob=alpha/2))))
  avg.tau.high <- as.matrix(apply(avg.tau.dist, 2, FUN = function(x)(quantile(x,prob=1-(alpha/2)))))
  p.val <- as.matrix(apply(avg.tau.dist, 2, FUN = function(x)(2-2*max(mean(x < 0), mean(x > 0)))))
  
  # impacts by month
  monthly_low <- as.matrix(apply(tau.dist, 1, FUN = function(x)(quantile(x,prob=alpha/2))))
  monthly_high <- as.matrix(apply(tau.dist, 1, FUN = function(x)(quantile(x,prob=1-(alpha/2)))))
  
  # Bootstrapped interval by month
  boot.monthly_low <- as.matrix(apply(simulations, 1, FUN = function(x)(quantile(x,prob=alpha/2))))
  boot.monthly_high <- as.matrix(apply(simulations, 1, FUN = function(x)(quantile(x,prob=1-(alpha/2)))))
  
  
  ### STEP 3. Plot time series
  # Settings
  observed <- na.omit(y)
  fitted <- fitted.vals
  forecasted <- na.omit(c(fitted, point.fcast))

  forecasted_up<-forecasted_inf<-rep(NA, length(fitted[!is.na(fitted)]))
  forecasted_up<-append(forecasted_up, boot.monthly_high)
  forecasted_inf<-append(forecasted_inf, boot.monthly_low)
  
  start <- which(dates == int.date) - round(1 * sum(dates < int.date))
  end <- length(forecasted)
  x <- dates[start:end]
  
  # Plot
  dat <- data.frame(x = x, forecasted.cut = forecasted[start:end], observed.cut = observed[start:end],
                    forecasted_up=forecasted_up[start:end], forecasted_inf=forecasted_inf[start:end])
  ylim <- c(min(dat[, -1], na.rm = T), max(dat[, -1], na.rm = T))
  
  g <- ggplot(data = dat, aes(x = x, colour = "Legend")) +  coord_cartesian(ylim = ylim) +  theme_bw(base_size = 15)+
    labs(title = "Forecasted series", y = "", x = "") +
    scale_colour_manual(values =c("darkblue", "black") ) +
    geom_vline(aes(xintercept = int.date, linetype = paste(int.date)), colour = "darkgrey", size = 0.6) +
    scale_linetype_manual(values = "longdash") +
    labs(color="Time series", linetype="Implementation date") +
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))+
    guides(color=guide_legend(override.aes=list(fill=NA, linetype = c("dashed", "solid"))))+
    geom_lineribbon(aes(y = forecasted.cut, color = "Forecast", ymin = forecasted_inf, ymax = forecasted_up),
                    size = 0.6, linetype ="dashed", fill = "slategray2")+
    geom_line(aes(y = observed.cut, color = "Observed"), size = 0.6) 
  
  
  ### STEP 4. Save outputs as list
  my_list <- list(raw.diff = diff, wtd.est = estimate, wtd.est.dist = tau.dist, avg.tau.dist = avg.tau.dist,
                  avg.tau.low = avg.tau.low, avg.tau.high = avg.tau.high, p.val = as.numeric(p.val), 
                  monthly_low = monthly_low, monthly_high=monthly_high, forecast.plot=g)
  return(my_list)
}

