#######################################################################################################  
#########################  Output Likelihoods for model selection / averaging #########################
#######################################################################################################
ModelSelectionManual <-function(y, ic = "aicc", xreg = NULL, dates,
                                int.date, auto.args = list(), n.cores=1){
  
  ### param checks
  if(class(y) != "ts" & !is.numeric(y)) stop("y must be numeric or ts")
  if(!is.ts(y)){
    y <- ts(y, frequency = findfrequency(y))
  }
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
  
  ### STEP 1. Subsetting the data: keep only pre-intervention data
  ind<-dates>=int.date
  
  # log version of outcome
  # log.y.00<- log(ts(y[!ind], frequency = frequency(y))) # log transform 
  
  # level version of outcome
  level.y.00<- (ts(y[!ind], frequency = frequency(y)))
  
  if(!is.null(xreg)) {
    xreg0<-xreg[!ind,]
    xreg0_temp <- as.data.frame(xreg[!ind,])
  } else {
    xreg0 <- NULL
    xreg0_temp <- NULL
  }
  
  ### STEP 2. Model estimation with levels of the outcome y
  
  #### SARIMAX loop over exog vars
  if(!is.null(xreg0_temp)){
    
    # Make list for all possible xreg var combinations
    variables <- colnames(xreg0_temp)
    combo_list <- list()
    
    for (i in 1:length(variables)) {
      combo_list[[i]] <- DescTools::CombSet(variables, i,
                                            repl = FALSE,
                                            ord = FALSE,
                                            as.list = TRUE)
    }
    
    combo_list <- purrr::flatten(combo_list)
    
    my.cluster <- parallel::makeCluster(
      n.cores, 
      type = "PSOCK"
    ) #create the cluster
    
    doParallel::registerDoParallel(cl = my.cluster) #register 
    
    models_results <- foreach (j = 1:length(combo_list), .packages = c("tidyverse", "forecast"), .combine=rbind) %dopar% {
      i = xreg0_temp %>% select(combo_list[[j]]) %>% colnames
      
      # Capture output from trace
      trace <- capture.output({
        mod_temp <- do.call("auto.arima", c(list(y = level.y.00), list(ic = ic),trace=TRUE, list(xreg = (xreg0_temp %>% select({{i}}) %>% as.matrix)), auto.args, method=c("ML"), test=c("adf")))
      })
      con    <- textConnection(trace)
      models <- read.table(con, sep=":")
      close(con)
      
      # Add xreg names
      models <- models %>% rename(model=V1, ic=V2) %>% filter(model!=" Best model") 
      models$xreg <- toString(names(xreg0_temp %>% select({{i}})))
      
      models
    }
    parallel::stopCluster(cl = my.cluster)
    
    # Add SARIMA no exog vars
    trace <- capture.output({
      mod_temp <- do.call("auto.arima", c(list(y = level.y.00), list(ic = ic),trace=TRUE, list(xreg =NULL), auto.args, method=c("ML"), test=c("adf")))
    })
    con    <- textConnection(trace)
    models <- read.table(con, sep=":")
    close(con)
    
    models <- models %>% rename(model=V1, ic=V2) %>% filter(model!=" Best model") 
    models$xreg <- "NULL"
    models_results <- rbind(models_results, models)
    
    # Calculate deltas on full set of candidate models and prepare for output
    models_results$ic <- as.numeric(models_results$ic)
    
    models_results <- models_results %>% arrange(ic) %>% mutate(delta_i= abs(min(ic) - ic))
    
  }else{
    trace <- capture.output({
      mod_temp <- do.call("auto.arima", c(list(y = level.y.00), list(ic = ic),trace=TRUE, list(xreg =NULL), auto.args, method=c("ML"), test=c("adf")))
    })
    con    <- textConnection(trace)
    models <- read.table(con, sep=":")
    close(con)
    
    models <- models %>% rename(model=V1, ic=V2) %>% filter(model!=" Best model") 
    models$xreg <- "NULL"
    models_results <- models
    
    # Calculate deltas on full set of candidate models and prepare for output
    models_results$ic <- as.numeric(models_results$ic)
    
    models_results <- models_results %>% arrange(ic) %>% mutate(delta_i= abs(min(ic) - ic))
  }
  
  return(models_results)
  
  
  
}

