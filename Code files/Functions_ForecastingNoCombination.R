#######################################################################################################  
###################################  Forecasts ###################################
# This script writes a function that takes a set of candidate models as input, 
# calculates Akaike weights, and generates an average forecast. 
# Bootstrapping is then performed using nboot iterations to create a confidence interval around the forecast
#######################################################################################################
MakeForecasts <-function(y, candidate.models, xreg = NULL, dates,
                      int.date, nboot = NULL, alpha = 0.05){
  
  ### param checks
  if(!is.ts(y)) stop("y must be numeric or ts")
  if(!is.ts(y)){
   y <- ts(y, frequency = findfrequency(y))
  }
  if(!is.data.frame(candidate.models)) stop("candidate.models must be a data.frame from Functions_ModelSelection")
  if(!is.null(xreg)) {
    if(!is.matrix(xreg) && !is.data.frame(xreg) && !is.numeric(xreg))
      stop("`xreg` must be a numeric vector, matrix or data.frame")
    xreg_temp <- xreg
    xreg <- as.matrix(xreg)
    if(nrow(xreg) != length(y)) stop("nrow(xreg) != length(y)")
  }
  if(!any(class(dates) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`dates` must be a vector of class Date")
  if(length(dates) != length(y)) stop("length(dates) != length(y)")
  if(length(int.date) != 1 || !any(class(dates) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`int.date` must be a Date of length 1")
  if(!missing(nboot) && (!is.numeric(nboot) | nboot <= 0)) stop("`nboot` must be a positive numeric value")
  # if(auto && sum(sum(order), sum(seasonal)) > 0){auto <- FALSE}
  
  ### STEP 1. Subsetting the data: before and after the intervention date
  # convert y to log 
  y <- log(y)
  
  # Subsetting
  ind<-dates>=int.date
  y.00<-ts(y[!ind], frequency = frequency(y))
  y.01<-ts(y[ind], frequency = frequency(y))
  
  if(!is.null(xreg)) {
    xreg0<-xreg[!ind,]
    xreg0_temp <- as.data.frame(xreg[!ind,])
    
    xreg1<-xreg[ind,]
    xreg1_temp <- as.data.frame(xreg[ind,])
    
  } else {
    xreg0 <- NULL
    xreg0_temp <- NULL
    
    xreg1 <- NULL
    xreg1_temp <- NULL
  }
  

  ### STEP 2. Calculate Akaike weights
  candidate.models <- candidate.models %>% filter(!is.infinite(delta_i) & abs(delta_i)==0) # only keep ic minimizing model
  candidate.models <- candidate.models %>% mutate(w_i = (exp(-0.5*abs(delta_i))) / sum(exp(-0.5*abs(delta_i))))
  
  ### STEP 3. Generate forecast and bootstrapped interval for each candidate model 
  point_fcast <- list()
  sim_fcast <- list()
  fitted_vals <- list()
  for(i in 1:nrow(candidate.models)){
    set.seed(123)
    candidate.models.use <- candidate.models[i,]
    
    # Pull needed model characteristics from data frame
      if(candidate.models.use$xreg != "NULL"){
        xreg_use <- xreg0_temp %>% select(c(strsplit(candidate.models.use$xreg, ", ")[[1]])) %>% as.matrix # xreg
        xreg1_use <- xreg1_temp %>% select(c(strsplit(candidate.models.use$xreg, ", ")[[1]])) %>% as.matrix
        
      }else{
        xreg_use <- NULL
        xreg1_use <- NULL
      }
    
    
    # order + seasonal
    arima_order_seasonal = rm_between_multiple(candidate.models.use$model, '(', ')', extract=TRUE)
    if(length(arima_order_seasonal)<2){
      arima_order_seasonal[[2]] <- "0,0,0"
    }
    
    if(str_detect(candidate.models.use$model, "with drift")==TRUE & candidate.models.use$xreg == "NULL"){
      drift_ind=TRUE
    }else{
      drift_ind=FALSE
    }
    
    # Use Arima() to fit the model
    mod <- do.call("Arima", c(list(y = y.00),
                             list(order = as.numeric(unlist(strsplit(arima_order_seasonal[[1]],',')))), list(seasonal = as.numeric(unlist(strsplit(arima_order_seasonal[[2]],',')))), list(xreg = xreg_use),
                             list(include.drift=drift_ind), method = c("ML")))
    
    
    # Generate forecast for post-treatment period using pre-treatment y and all X
    h<-length(y.01)
    fcast<-forecast(mod, xreg = xreg1_use, h = h, level = 1-alpha)
    point_fcast[[i]] <- exp(as.numeric(fcast$mean)) * exp(0.5*mod$sigma2) # bias corrected back transform from log to level
    point_fcast[[i]] <- point_fcast[[i]] * (candidate.models.use$w_i)
    
    # Make quantity-weighted fitted vals in pre-treatment period
    fitted_vals[[i]] <- exp(mod$fitted) * exp(0.5*mod$sigma2)
    fitted_vals[[i]] <- fitted_vals[[i]] * (candidate.models.use$w_i )
    
    # Generate bootstrap distribution
    simulated <- matrix(NA, h, nboot)
    for(j in 1:nboot){
      simulated[,j] <- simulate(mod,  future = TRUE, nsim = h, xreg = xreg1_use, bootstrap = TRUE)
    }
    
    # Save each matrix separately
    sim_fcast[[i]] <- exp(simulated) * exp(0.5*mod$sigma2)
    sim_fcast[[i]] <- sim_fcast[[i]]* (candidate.models.use$w_i )
    
  }
  
  ### STEP 4. Sum across weighted forecasts and simulations
  # Forecasts
  point_fcast.combined <- Reduce(`+`, point_fcast)
  point_fcast.combined <- point_fcast.combined / sum(candidate.models$w_i)
  
  # Fitted values
  fitted.vals_combined <- Reduce(`+`, fitted_vals)
  fitted.vals_combined <- fitted.vals_combined / sum(candidate.models$w_i)
  
  # Bootstrapped simulations
  sim_fcast.combined <- Reduce(`+`, sim_fcast)
  sim_fcast.combined <- sim_fcast.combined / sum(candidate.models$w_i)
  
  ### STEP 5. Output aggregated 1) 12x1 vector of weigthed avg. point forecasts and 2) Nx12 matrix of weighted avg. simulated paths  
  my_list <- list(combined.point.fcast = point_fcast.combined, combined.simulations=sim_fcast.combined, fitted.vals=fitted.vals_combined, candidate_models=candidate.models)
  return(my_list)
}