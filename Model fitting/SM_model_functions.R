#soil moisture spring phenology models


####Water Time (WT_SM)####

WT_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 3){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  F_crit <- par[2]
  SM_base <- par[3]
  
  # simple degree day sum set-up, but for SM
  Rw <- data$SM - SM_base
  Rw[Rw < 0] <- 0
  Rw[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####Photo-water time (PWT_SM)####

PWT_SM <- function(par, data){
  # exit the routine as some parameters are missing
  if (length(par) != 3){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1]) # int
  F_crit <- par[2]
  SM_base <- par[3]
  
  # create SM rate vector
  # forcing
  Rw <- data$SM - SM_base
  Rw[Rw < 0] <- 0
  Rw <- (data$Li / 24) * Rw
  Rw[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####M1 with SM (M1W_SM)####

M1W_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 4){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- par[1]
  k <- par[2]
  F_crit <- par[3]
  SM_base <- par[4]
  
  # create SM rate vector
  Rw <- data$SM - SM_base
  Rw[Rw < 0] <- 0
  Rw <- ((data$Li / 10) ^ k) * Rw
  Rw[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####Sequential (SM then temp) (SQW_SM)####

SQW_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 5){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  SM_base <- par[4]
  SM_req <- par[5]
  
  # SM requirement
  Rw <- data$SM - SM_base
  Rw[Rw < 0] <- 0
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  # SM requirement has to be met before
  # temp accumulation starts (binary choice)
  k <- as.numeric(Sw >= SM_req)
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf <- Rf * k
  Rf[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rf,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}


####Sequential reverse (temp then SM) (SQWr_SM)####

SQWr_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 5){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  T_req <- par[4]
  SM_base <- par[5]
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf[1:t0,] <- 0
  
  Sf <- apply(Rf, 2, cumsum)
  
  # chilling requirement has to be met before
  # accumulation starts (binary choice)
  k <- as.numeric(Sf >= T_req)
  
  # SM requirement
  Rw <- data$SM - SM_base
  Rw[Rw < 0] <- 0
  Rw <- Rw * k
  Rw[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####Parallel (SM and temp) (PAW_SM)####

PAW_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 6){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  SM_base <- par[4]
  SM_req <- par[5]
  SM_ini <- par[6]
  
  # SM requirement
  Rw <- data$SM - SM_base
  Rw[Rw < 0] <- 0
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  ##Determine k
  k <- SM_ini + Sw * (1 - SM_ini)/SM_req
  k[Sw >= SM_req] = 1
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] <- 0
  Rf <- Rf * k
  Rf[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy = apply(Rf,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####Water time sigmoidal (WTs_SM)####

WTs_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 4){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  b <- par[2]
  c <- par[3]
  F_crit <- par[4]
  
  # sigmoid SM response
  Rw <- 1 / (1 + exp(-b * (data$SM - c)))
  Rw[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}


####Photo-water time sigmoidal (PWTs_SM)####

PWTs_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 4){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- par[1]
  b <- par[2]
  c <- par[3]
  F_crit <- par[4]
  
  # create SM rate vector
  Rw <- 1 / (1 + exp(-b * (data$SM - c)))
  Rw <- (data$Li / 24) * Rw
  Rw[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####M1 with SM sigmoidal (M1Ws_SM)####

M1Ws_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 5){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- par[1]
  b <- par[2]
  c <- par[3]
  k <- par[4]
  F_crit <- par[5]
  
  # create SM rate vector
  Rw <- 1 / (1 + exp(-b * (data$SM - c)))
  Rw <- ((data$Li / 10) ^ k) * Rw
  Rw[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####Sequential sigmoidal (SM then temp) (SQWs_SM)####

SQWs_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 6){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  T_base <- par[2]
  b <- par[3]
  c <- par[4]
  F_crit <- par[5]
  SM_req <- par[6]
  
  # sigmoid SM response
  Rw <- 1 / (1 + exp(-b * (data$SM - c)))
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  # SM requirement has to be met before
  # temp accumulation starts (binary choice)
  k <- as.numeric(Sw >= SM_req)
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf <- Rf * k
  Rf[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rf,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####Sequential reverse sigmoidal (temp then SM) (SQWrs_SM)####

SQWrs_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 6){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  T_base <- par[2]
  b <- par[3]
  c <- par[4]
  F_crit <- par[5]
  T_req <- par[6]
  
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf[1:t0,] <- 0
  
  Sf <- apply(Rf, 2, cumsum)
  
  # temp requirement has to be met before
  # SM accumulation starts (binary choice)
  k <- as.numeric(Sf >= T_req)
  
  # sigmoid SM response
  Rw <- 1 / (1 + exp(-b * (data$SM - c)))
  Rw <- Rw * k
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####Parallel sigmoidal (SM and temp) (PAWs_SM)####

PAWs_SM <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 7){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument
  t0 <- round(par[1])
  T_base <- par[2]
  b <- par[3]
  c <- par[4]
  F_crit <- par[5]
  SM_req <- par[6]
  SM_ini <- par[7]
  
  
  # sigmoid SM response
  Rw <- 1 / (1 + exp(-b * (data$SM - c)))
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  ##Determine k
  k <- SM_ini + Sw * (1 - SM_ini)/SM_req
  k[Sw >= SM_req] = 1
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] <- 0
  Rf <- Rf * k
  Rf[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy = apply(Rf,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}




