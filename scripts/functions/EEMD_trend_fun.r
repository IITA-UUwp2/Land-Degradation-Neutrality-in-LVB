# +
####### NAME:
# EEMD_trend_fun_at_pixel

####### PURPOSE:
# The function is developed by Dr. Yelong Zeng tried to detect the time-varying trends in vegetation greenness in the Sahel region. 
      
####### Reference:
# Zeng Y., Jia L., Menenti M., Jiang M., Zheng C., Bennour A. and Lv Y.(2024). Regional divergent evolution of vegetation greenness and climatic drivers in the Sahel-Sudan-Guinea region: nonlinearity and explainable machine learning. Frontiers in Forests and Global Change, 2024, 7: 1416373.
# Pan, N. Q., Feng, X. M., Fu, B. J., Wang, S., Ji, F., and Pan, S. F. (2018). Increasing global vegetation browning hidden in overall vegetation greening: insights from time-varying trends. Remote Sens. Environ. 214, 59–72.
# Luukko, P. J. J., Helske, J., and Räsänen, E. (2016). Introducing libeemd: a program package for performing the ensemble empirical mode decomposition. Comput. Stat. 31, 545–557.
# Ji, F., Wu, Z., Huang, J., and Chassignet, E. P. (2014). Evolution of land surface air temperature trend. Nat. Clim. Chang. 4, 462–466.

####### Input:
# indata: raw data with numeric vector
# factor: scale factor for raw data
# start & frequency: parameter for constructing ts data
# ensemble_size: ensemble_size for eemd fun, look Rlibeemd package
# noise_strength：noise_strength for eemd fun, look Rlibeemd package
# S_number：S_number for eemd fun, look Rlibeemd package
# num_siftings：num_siftings for eemd fun, look Rlibeemd package
# fill_value：fill values when raw data containing many NAs or eemd fun failure
# out.list：True for returning a list, False for returning a vector

####### Output:
# Residual: Residual term for raw data
# trend_accumulated: the accumulated variation of a secular trend
# rate_instantaneous: the instantaneous rate at time t of a secular trend
# trend_class: trend types based on the occurrence of extrema within a secular trend, i.e., monotonic decrease [1], monotonic increase[2],increase-to-decrease transition[3],decrease-to-increase transition[4]
# 
# break_time: the time of breakpoint(if exist, otherwise -999)
# trend_accumulated_bf: the accumulated variation before a breakpoint(if exist, otherwise -999)
# trend_accumulated_af: the accumulated variation after a breakpoint(if exist, otherwise -999)
# mean_rate_instantaneous_bf: mean instantaneous rate before a breakpoint(if exist, otherwise -999)
# mean_rate_instantaneous_af: mean instantaneous rate after a breakpoint(if exist, otherwise -999)

####### AUTHOR:
# Dr. Yelong Zeng
# E-mail: zengyl.rs@gmail.com

########The function setting fill values when raw data containing many NAs or eemd fun failure
ignore_eemd_trend_fun_nosig <- function(timeseries,  fill_value , out.list){
  # timeseries：raw data with ts format
  Residu <- as.ts(rep(fill_value, length(timeseries)))
  trend_accumulated <- as.ts(rep(fill_value, length(timeseries)))
  rate_instantaneous <- as.ts(rep(fill_value, length(timeseries)))
  tsp(Residu) <- tsp(trend_accumulated) <- tsp(rate_instantaneous) <- tsp(timeseries)
  
  if (out.list == F) {
    out <- c(Residual = Residu, 
             trend_accumulated = trend_accumulated, 
             rate_instantaneous = rate_instantaneous,
             trend_class = fill_value, 
             break_time = fill_value, 
             trend_accumulated_bf = fill_value,
             trend_accumulated_af = fill_value, 
             mean_rate_instantaneous_bf = fill_value,
             mean_rate_instantaneous_af =  fill_value)
  }else{
    out <- list(trend_sig = cbind(Residual = Residu, 
                                  trend_accumulated = trend_accumulated, 
                                  rate_instantaneous = rate_instantaneous),
                
                trend_BP = c(trend_class = fill_value, 
                             break_time = fill_value, 
                             trend_accumulated_bf = fill_value,
                             trend_accumulated_af = fill_value, 
                             mean_rate_instantaneous_bf = fill_value,
                             mean_rate_instantaneous_af =  fill_value))
  }
  
  return(out)
}

########EEMD fun with Rlibeemd package
eemdfun_nosig <- function(x, ensemble_size, noise_strength, S_number, num_siftings){
  out_eemd <- Rlibeemd::eemd(x, ensemble_size = ensemble_size, noise_strength = noise_strength,
                             S_number = S_number, num_siftings = num_siftings)
  if (!('Residual' %in% colnames(out_eemd))) {
    out <- x
  }else{
    out <- out_eemd[, 'Residual']
  }
  return(out)
}

########EEMD_trend_fun_at_pixel
EEMD_trend_fun_at_pixel <- function(indata, para = list(factor = 1, start = c(2001, 1), frequency = 1, ensemble_size = 100,
                                                 noise_strength = 0.2,  S_number = 4, num_siftings = 50,  fill_value = NA, out.list = F))
{
  ######################Prepare1: get global setting################################
  factor <- para[['factor']]
  start <- para[['start']]
  frequency <- para[['frequency']]
  ensemble_size <- para[['ensemble_size']]
  noise_strength <- para[['noise_strength']]
  S_number <- para[['S_number']]
  num_siftings <- para[['num_siftings']]
  fill_value <- para[['fill_value']]
  out.list <- para[['out.list']]
  
  # get timelength
  timelength <- length(indata)
  ######################Prepare2: set fill values when raw data containing many NAs ################################
  NAnum <- rle(is.na(indata))
  naomit <- as.numeric(na.omit(indata))
  NA_consecutively <-  ifelse(frequency <= 4, round(1 / 4 * timelength), round(3 / 4 * frequency))
  NA_total <- round(1 / 2 * timelength)
  
  if (any(NAnum$lengths[which(NAnum$values == TRUE)] > NA_consecutively | length(naomit) < NA_total | length(unique(naomit)) <= 1 | all(is.na(indata)) | all(is.nan(indata)) ) | length(unique(indata)) == 1) {
    timeseries <- ts(indata, start = start, frequency = frequency)
    out <- ignore_eemd_trend_fun_nosig(timeseries = timeseries,  fill_value = fill_value, out.list = out.list)
  }else {
    #################################MAIN STEPS################################################
    
    #make interpolation when raw data containing some NAs
    ts_na_interpolation <- imputeTS::na_interpolation(indata, option = "linear") * factor
    timeseries <- ts(ts_na_interpolation, start = start, frequency = frequency)
    
    
    ########################1.Get trend term of raw time series [Residual, Trend(t), rate_instant]#################################
    #get Residual term of raw data
    rn <- eemdfun_nosig(timeseries, ensemble_size = ensemble_size, noise_strength = noise_strength,
                        S_number = S_number, num_siftings = num_siftings)
    # V(t)=Rn(t)-Rn(start)
    tre1 <- rn - rn[1]
    # rate_instant=V(t)-Trend(t-1)
    rate_instant <- as.ts(rep(0, timelength))
    for (i in 2:timelength) {
      rate_instant[i] <- tre1[i] - tre1[i - 1]
    }
    tsp(rate_instant) <- tsp(timeseries)
    
    ########################2.Get trend class, breakpoint, time of breakpoint & average change rate before/ after breakpoint#################################
    #number of breakpoint
    tre1_extreme <- EMD::extrema(tre1)
    num_tre1_extreme <- tre1_extreme[['nextreme']]
    
    #return fill_value when num_tre1_extreme > 1
    if (num_tre1_extreme > 1) {
      tmp_out <- ignore_eemd_trend_fun_nosig(timeseries,  fill_value = fill_value, out.list = T)

      trend_symbol <- as.numeric(tmp_out[['trend_BP']]['trend_class'])
      breaktime_trend <- as.numeric(tmp_out[['trend_BP']]['break_time'])
      trend_accumulated_bf_tmp <- as.numeric(tmp_out[['trend_BP']]['trend_accumulated_bf'])
      trend_accumulated_af_tmp <- as.numeric(tmp_out[['trend_BP']]['trend_accumulated_af'])
      avg_rate_instant_bf <- as.numeric(tmp_out[['trend_BP']]['mean_rate_instantaneous_bf'])
      avg_rate_instant_af <- as.numeric(tmp_out[['trend_BP']]['mean_rate_instantaneous_af'])
      
      #Without breakpoint
    }else if (num_tre1_extreme == 0) {
      # trend_symbol: monotonic decrease [1], monotonic increase[2],increase-to-decrease transition[3],decrease-to-increase transition[4]
      trend_symbol <- ifelse((tre1[1] < tre1[timelength]), 2, 1)
      breaktime_trend <- -999
      
      
      trend_accumulated_bf_tmp <- -999
      trend_accumulated_af_tmp <- -999
      
      
      avg_rate_instant_bf <- -999
      avg_rate_instant_af <- -999
      
      # With breakpoint
    } else if (num_tre1_extreme == 1){
      BP_index <- ifelse(is.null(tre1_extreme$maxindex), tre1_extreme$minindex[1], tre1_extreme$maxindex[1])
      
      tre2 <- tre1 - tre1[timelength]
      
      breaktime_trend <- time(timeseries)[BP_index]
      
      trend_symbol <- ifelse((tre1[BP_index - 1] - tre1[1]) > 0, 3, 4)
      
      trend_accumulated_bf_tmp <- tre1[BP_index]
      trend_accumulated_af_tmp <- -tre2[BP_index]
      
      avg_rate_instant_bf <- mean(rate_instant[2:BP_index])
      avg_rate_instant_af <- mean(rate_instant[BP_index:timelength])
    }
    
    ########################3.Output results#################################
    if (out.list == F) {
      out <- c(Residual = rn,
               trend_accumulated = tre1,
               rate_instantaneous = rate_instant,
               
               trend_class = trend_symbol,
               break_time = breaktime_trend,
               trend_accumulated_bf = trend_accumulated_bf_tmp, 
               trend_accumulated_af = trend_accumulated_af_tmp,
               mean_rate_instantaneous_bf = avg_rate_instant_bf,
               mean_rate_instantaneous_af =  avg_rate_instant_af)
    }else{
      out <- list(trend_sig = cbind(Residual = rn, 
                                    trend_accumulated = tre1, 
                                    rate_instantaneous = rate_instant),
                  trend_BP = c(trend_class = trend_symbol, 
                               break_time = breaktime_trend, 
                               trend_accumulated_bf = trend_accumulated_bf_tmp, 
                               trend_accumulated_af = trend_accumulated_af_tmp, 
                               mean_rate_instantaneous_bf = avg_rate_instant_bf,
                               mean_rate_instantaneous_af =  avg_rate_instant_af))
    }
    
  }
  
  return(out)
}
