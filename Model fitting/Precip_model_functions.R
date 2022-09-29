#New precip models

#### Water-time (WT) ####

WT <- function(par, data ){
  
  # exit the routine as some parameters are missing
  if (length(par) != 3){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  F_crit <- par[2]
  P_base<- par[3]
  
  # simple degree day sum set-up, but for precip
  Rw <- data$Pi - P_base
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


####Water-time sigmoidal (WTs)####

WTs <- function(par, data){
  
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
  
  # sigmoid precip response
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
  Rw[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####Photo-water time (PWT)####

PWT <- function(par, data){
  # exit the routine as some parameters are missing
  if (length(par) != 3){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1]) # int
  F_crit <- par[2]
  P_base <- par[3]
  
  # create precip rate vector
  # forcing
  Rw <- data$Pi - P_base
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



####Photo-water time sigmoidal (PWTs)####

PWTs <- function(par, data){
  
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
  
  # create precip rate vector
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
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



####M1 with precip (M1W)####

M1W <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 4){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  k <- par[2]
  F_crit <- par[3]
  P_base <- par[4]
  
  # create precip rate vector
  Rw <- data$Pi - P_base
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



####M1 with precip sigmoidal (M1Ws)####

M1Ws <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 5){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  b <- par[2]
  c <- par[3]
  k <- par[4]
  F_crit <- par[5]
  
  # create precip rate vector
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
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



####Sequential (precip then temp) (SQW)####

SQW <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 5){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  P_base <- par[4]
  P_req <- par[5]
  
  
  # Precip requirement
  Rw <- data$Pi - P_base
  Rw[Rw < 0] <- 0
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  # precip requirement has to be met before
  # temp accumulation starts (binary choice)
  k <- as.numeric(Sw >= P_req)
  
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



####Sequential reverse (temp then precip) (SQWr)####
#r for reverse

SQWr <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 5){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  P_base <- par[4]
  T_req <- par[5]
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf[1:t0,] <- 0
  
  Sf <- apply(Rf, 2, cumsum)
  
  # chilling requirement has to be met before
  # accumulation starts (binary choice)
  k <- as.numeric(Sf >= T_req)
  
  # Precip requirement
  Rw <- data$Pi - P_base
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



####Parallel (precip and temp) (PAW)####

PAW <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 6){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  P_base <- par[4]
  P_ini <- par[5]
  P_req <- par[6]
  
  # Precip requirement
  Rw <- data$Pi - P_base
  Rw[Rw < 0] <- 0
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  ##Determine k
  k <- P_ini + Sw * (1 - P_ini)/P_req
  k[Sw >= P_req] = 1
  
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



####Sequential sigmoidal (precip then temp) (SQWs)####

SQWs <- function(par, data){
  
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
  P_req <- par[6]
  
  # sigmoid precip response
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  # SM requirement has to be met before
  # temp accumulation starts (binary choice)
  k <- as.numeric(Sw >= P_req)
  
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



####Sequential reverse sigmoidal (temp then precip) (SQWrs)####

SQWrs <- function(par, data){
  
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
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
  Rw <- Rw * k
  
  # DOY of budburst criterium
  doy <- apply(Rw,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
}



####Parallel sigmoidal (precip and temp) (PAW)####

PAWs <- function(par, data){
  
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
  P_ini <- par[6]
  P_req <- par[7]
  
  
  # sigmoid SM response
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  ##Determine k
  k <- P_ini + Sw * (1 - P_ini)/P_req
  k[Sw >= P_req] = 1
  
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



####Sequential no P-base (SQW_NoPbase)####

SQW_NoPbase <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 4){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  # extract the parameter values from the
  # par argument for readability
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  P_req <- par[4]
  
  
  # Precip requirement
  Rw <- data$Pi
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  # precip requirement has to be met before
  # temp accumulation starts (binary choice)
  k <- as.numeric(Sw >= P_req)
  
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



####SQW_cdd: precip resets with consecutive dry days (cdd)####

SQW_cdd <- function(par, data){

  # exit the routine as some parameters are missing
  if (length(par) != 5){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  #parameters
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  P_req <- par[4]
  cdd_thres <- round(par[5])
  
  #Precip requirement
  Rw <- data$Pi
  Rw[1:t0,] <- 0
  
  ##Calculate cdd

  #If precip = 0, then assign a "1" to the cell
  k_cdd <- data.frame(ifelse(Rw > 0, 0, 1))
  colnames(k_cdd) <- paste0(data$site, "_", data$year)
  
  #force days before t0 to be 0 so not counted in cdd
  k_cdd[1:t0,] <- 0
  
  #function to add up cdd, resetting when rain occurs
  cdd_func <- function(col){
    as.matrix(with(k_cdd, ave(k_cdd[[col]], cumsum(k_cdd[[col]] == 0), FUN = cumsum)))
  }
  
  #Pull out years
  col_names <- names(k_cdd)
  
  #Make empty matrix
  cdd_matrix <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- cdd_func(col = year)
    cdd_matrix[,i] <- output
    
  }
  
  ##Reset Sw when hit cdd_thres
  
  #change matrices to dataframes
  Rw_df <- data.frame(Rw)
  colnames(Rw_df) <- paste0(data$site, "_", data$year)
  
  cdd_matrix_df <- data.frame(cdd_matrix)
  colnames(cdd_matrix_df) <- paste0(data$site, "_", data$year)
  
  #function to reset Sw when hit cdd_thres
  cumsum_func <- function(col){
    as.matrix(ave(Rw_df[[col]], cumsum(cdd_matrix_df[[col]] == cdd_thres), FUN = cumsum))
  }
  
  #Make empty matrix
  Sw <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- cumsum_func(col = year)
    Sw[,i] <- output
    
  }
  
  ## precip requirement has to be met before temp accumulation
  
  # assign output matrix
  k <- matrix(0, nrow = 365, ncol = length(col_names))
  rows <- nrow(data$Pi)
  cols <- ncol(data$Pi)
  
  # assign value of 1 once precip requirement met
  for (i in 1:cols){
    for (j in 1:rows){
      if(Sw[j,i] >= P_req){
        k[j:rows,i] <- 1
        break
      }
    }
  }
  
  
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


####SQW_Tmin: temp resets when Tmin below threshold####

SQW_Tmin <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 5){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  #parameters
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  P_req <- par[4]
  T_thres <- par[5]


  #Precip requirement
  Rw <- data$Pi
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  # precip requirement has to be met before
  # temp accumulation starts (binary choice)
  k <- as.numeric(Sw >= P_req)
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf <- Rf * k
  Rf[1:t0,] <- 0
  
  #change matrices to dataframes
  Rf_df <- data.frame(Rf)
  colnames(Rf_df) <- paste0(data$site, "_", data$year)
  
  Tmin_df <- data.frame(data$Tmini)
  colnames(Tmin_df) <- paste0(data$site, "_", data$year)
  
  #function to reset Sf when hit T_thres
  T_thres_func <- function(col){
    as.matrix(ave(Rf_df[[col]], cumsum(Tmin_df[[col]] <= T_thres), FUN = cumsum))
  }
  
  #Pull out years
  col_names <- names(Tmin_df)
  
  #Make empty matrix
  Sf <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- T_thres_func(col = year)
    Sf[,i] <- output
    
  }
  
  # DOY of budburst criterium
  doy <- apply(Sf, 2, function(xt){
    data$doy[which(xt >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)
  
}



####SQW_cdd_Tmin: combine cdd & Tmin models####

SQW_cdd_Tmin <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 6){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  P_req <- par[4]
  cdd_thres <- round(par[5])
  T_thres <- par[6]
  
  # Precip requirement
  Rw <- data$Pi
  Rw[1:t0,] <- 0
  
  ##Calculate cdd
  
  #If precip = 0 (a dry day), then assign a "1" to the cell 
  k_cdd <- data.frame(ifelse(Rw > 0, 0, 1))
  colnames(k_cdd)<- paste0(data$site, "_", data$year)
  
  #force days before t0 to be 0 so not counted in cdd
  k_cdd[1:t0,] <- 0
  
  #function to add up cdd, resetting when rain occurs
  cdd_func <- function(col){
    as.matrix(with(k_cdd, ave(k_cdd[[col]], cumsum(k_cdd[[col]] == 0), FUN = cumsum)))
  }
  
  #Pull out years
  col_names <- names(k_cdd)
  
  #Make empty matrix
  cdd_matrix <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- cdd_func(col = year)
    cdd_matrix[,i] <- output
    
  }
  
  
  ##Reset Sw when hit cdd_thres
  
  #change matrices to dataframes
  Rw_df <- data.frame(Rw)
  colnames(Rw_df) <- paste0(data$site, "_", data$year)
  
  cdd_matrix_df <- data.frame(cdd_matrix)
  colnames(cdd_matrix_df) <- paste0(data$site, "_", data$year)
  
  #function to reset Sw when hit cdd_thres
  cumsum_func <- function(col){
    as.matrix(ave(Rw_df[[col]], cumsum(cdd_matrix_df[[col]] == cdd_thres), FUN = cumsum))
  }
  
  #Make empty matrix
  Sw <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- cumsum_func(col = year)
    Sw[,i] <- output
    
  }
  
  
  ##precip requirement has to be met before temp accumulation starts
  
  # assign output matrix
  k <- matrix(0, nrow = 365, ncol = length(col_names))
  rows <- nrow(data$Pi)
  cols <- ncol(data$Pi)
  
  # assign value of 1 once precip requirement met
  for (i in 1:cols){
    for (j in 1:rows){
      if(Sw[j,i] >= P_req){
        k[j:rows,i] <- 1
        break
      }
    }
  }
  
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf <- Rf * k
  Rf[1:t0,] <- 0
  
  #change matrices to dataframes
  Rf_df <- data.frame(Rf)
  colnames(Rf_df) <- paste0(data$site, "_", data$year)
  
  Tmin_df <- data.frame(data$Tmini)
  colnames(Tmin_df) <- paste0(data$site, "_", data$year)
  
  #function to reset Sf when hit T_thres
  T_thres_func <- function(col){
    as.matrix(ave(Rf_df[[col]], cumsum(Tmin_df[[col]] <= T_thres), FUN = cumsum))
  }
  
  #Make empty matrix
  Sf <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- T_thres_func(col = year)
    Sf[,i] <- output
    
  }
  
  # DOY of budburst criterium
  doy <- apply(Sf, 2, function(xt){
    data$doy[which(xt >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer
  # or a vector
  shape_model_output(data = data, doy = doy)

}




####SQW_Pi_Tmin: precip reset with Tmin below threshold####

SQW_Pi_Tmin <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 5){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  #parameters
  t0 <- round(par[1])
  T_base <- par[2]
  F_crit <- par[3]
  P_req <- par[4]
  T_thres <- par[5]
  
  # Precip requirement
  Rw <- data$Pi
  Rw[1:t0,] <- 0
  
  #change matrices to dataframes
  Rw_df <- data.frame(Rw)
  colnames(Rw_df) <- paste0(data$site, "_", data$year)
  
  Tmin_df <- data.frame(data$Tmini)
  colnames(Tmin_df) <- paste0(data$site, "_", data$year)
  
  #function to reset Sw when hit T_thres
  T_thres_func_Sw <- function(col){
    as.matrix(ave(Rw_df[[col]], cumsum(Tmin_df[[col]] <= T_thres), FUN = cumsum))
  }
  
  #Pull out years
  col_names <- names(Tmin_df)
  
  #Make empty matrix
  Sw <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- T_thres_func_Sw(col = year)
    Sw[,i] <- output
    
  }
  
  ## precip requirement has to be met before temp accumulation starts
  
  # assign output matrix
  k <- matrix(0, nrow = 365, ncol = length(col_names))
  rows <- nrow(data$Pi)
  cols <- ncol(data$Pi)
  
  # assign value of 1 once precip requirement met
  for (i in 1:cols){
    for (j in 1:rows){
      if(Sw[j,i] >= P_req){
        k[j:rows,i] <- 1
        break
      }
    }
  }
  
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




####SQWs_cdd####

SQWs_cdd <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 7){
    stop("model parameter(s) out of range (too many, too few)")
  }
    
  #Parameters
  t0 <- round(par[1])
  T_base <- par[2]
  b <- par[3]
  c <- par[4]
  F_crit <- par[5]
  P_req <- par[6]
  cdd_thres <- round(par[7])
  
  #Precip matrix to calculte cdd
  Precip <- data$Pi
  Precip[1:t0,] <- 0
  
  ##Calculate cdd
  
  #If precip = 0(a dry day), then assign a "1" to the cell 
  k_cdd <- data.frame(ifelse(Precip > 0, 0, 1))
  colnames(k_cdd) <- paste0(data$site, "_", data$year)
  
  #force days before t0 to be 0 so not counted in cdd
  k_cdd[1:t0,] <- 0
  
  #function to add up cdd, resetting when rain occurs
  cdd_func <- function(col){
    as.matrix(with(k_cdd, ave(k_cdd[[col]], cumsum(k_cdd[[col]] == 0), FUN = cumsum)))
  }
  
  #Pull out years
  col_names <- names(k_cdd)
  
  #Make empty matrix
  cdd_matrix <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- cdd_func(col = year)
    cdd_matrix[,i] <- output
    
  }
  
  
  #Calculate Rw - sigmoidal precip response
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
  Rw[1:t0,] <- 0
  
  
  ##Reset Sw when hit cdd_thres
  
  #change matrices to dataframes
  Rw_df <- data.frame(Rw)
  colnames(Rw_df) <-  paste0(data$site, "_", data$year)
  
  cdd_matrix_df <- data.frame(cdd_matrix)
  colnames(cdd_matrix_df) <-  paste0(data$site, "_", data$year)
  
  #function to reset Sw when hit cdd_thres
  cumsum_func <- function(col){
    as.matrix(ave(Rw_df[[col]], cumsum(cdd_matrix_df[[col]] == cdd_thres), FUN = cumsum))
  }
  
  #Make empty matrix
  Sw <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- cumsum_func(col = year)
    Sw[,i] <- output
    
  }
  
  # precip requirement has to be met before temp accumulation starts
  
  # assign output matrix
  k <- matrix(0, nrow = 365, ncol = length(col_names))
  rows <- nrow(data$Pi)
  cols <- ncol(data$Pi)
  
  # assign value of 1 once precip requirement met
  for (i in 1:cols){
    for (j in 1:rows){
      if(Sw[j,i] >= P_req){
        k[j:rows,i] <- 1
        break
      }
    }
  }
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf <- Rf * k
  Rf[1:t0,] <- 0
  
  # DOY of budburst criterium
  doy <- apply(Rf,2, function(xt){
    data$doy[which(cumsum(xt) >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer or a vector
  shape_model_output(data = data, doy = doy)
}


####SQWs_Tmin####

SQWs_Tmin <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 7){
    stop("model parameter(s) out of range (too many, too few)")
  }

  #Parameters
  t0 <- round(par[1])
  T_base <- par[2]
  b <- par[3]
  c <- par[4]
  F_crit <- par[5]
  P_req <- par[6]
  T_thres <- par[7]
  
  #Calculate Rw - sigmoid precip response
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
  Rw[1:t0,] <- 0
  
  Sw <- apply(Rw, 2, cumsum)
  
  # precip requirement has to be met before
  # temp accumulation starts (binary choice)
  k <- as.numeric(Sw >= P_req)
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf <- Rf * k
  Rf[1:t0,] <- 0
  
  #change matrices to dataframes
  Rf_df <- data.frame(Rf)
  colnames(Rf_df) <- paste0(data$site, "_", data$year)
  
  Tmin_df <- data.frame(data$Tmini)
  colnames(Tmin_df) <- paste0(data$site, "_", data$year)
  
  
  #function to reset Sf when hit T_thres
  T_thres_func <- function(col){
    as.matrix(ave(Rf_df[[col]], cumsum(Tmin_df[[col]] <= T_thres), FUN = cumsum))
  }
  
  #Pull out years
  col_names <- names(Tmin_df)
  
  #Make empty matrix
  Sf <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- T_thres_func(col = year)
    Sf[,i] <- output
    
  }
  
  
  # DOY of budburst criterium
  doy <- apply(Sf, 2, function(xt){
    data$doy[which(xt >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer or a vector
  shape_model_output(data = data, doy = doy)
  
}



####SQWs_cdd_Tmin####

SQWs_cdd_Tmin <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 8){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  #Parameters
  t0 <- round(par[1])
  T_base <- par[2]
  b <- par[3]
  c <- par[4]
  F_crit <- par[5]
  P_req <- par[6]
  cdd_thres <- round(par[7])
  T_thres <- par[8]
  
  #Precip matrix to calculte cdd
  Precip <- data$Pi
  Precip[1:t0,] <- 0
  
  ##Calculate cdd
  
  #If precip = 0(a dry day), then assign a "1" to the cell 
  k_cdd <- data.frame(ifelse(Precip > 0, 0, 1))
  colnames(k_cdd) <- paste0(data$site, "_", data$year)
  
  #force days before t0 to be 0 so not counted in cdd
  k_cdd[1:t0,] <- 0
  
  #function to add up cdd, resetting when rain occurs
  cdd_func <- function(col){
    as.matrix(with(k_cdd, ave(k_cdd[[col]], cumsum(k_cdd[[col]] == 0), FUN = cumsum)))
  }
  
  #Pull out years
  col_names <- names(k_cdd)
  
  #Make empty matrix
  cdd_matrix <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- cdd_func(col = year)
    cdd_matrix[,i] <- output
    
  }
  
  #Calculate Rw - sigmoidal precip response
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
  Rw[1:t0,] <- 0
  
  ##Reset Sw when hit cdd_thres
  
  #change matrices to dataframes
  Rw_df <- data.frame(Rw)
  colnames(Rw_df) <-  paste0(data$site, "_", data$year)
  
  cdd_matrix_df <- data.frame(cdd_matrix)
  colnames(cdd_matrix_df) <-  paste0(data$site, "_", data$year)
  
  #function to reset Sw when hit cdd_thres
  cumsum_func <- function(col){
    as.matrix(ave(Rw_df[[col]], cumsum(cdd_matrix_df[[col]] == cdd_thres), FUN = cumsum))
  }
  
  #Make empty matrix
  Sw <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- cumsum_func(col = year)
    Sw[,i] <- output
    
  }
  
  # precip requirement has to be met before temp accumulation starts
  
  # assign output matrix
  k <- matrix(0, nrow = 365, ncol = length(col_names))
  rows <- nrow(data$Pi)
  cols <- ncol(data$Pi)
  
  # assign value of 1 once precip requirement met
  for (i in 1:cols){
    for (j in 1:rows){
      if(Sw[j,i] >= P_req){
        k[j:rows,i] <- 1
        break
      }
    }
  }
  
  # forcing
  Rf <- data$Ti - T_base
  Rf[Rf < 0] = 0
  Rf <- Rf * k
  Rf[1:t0,] <- 0
  
  #change matrices to dataframes
  Rf_df <- data.frame(Rf)
  colnames(Rf_df) <- paste0(data$site, "_", data$year)
  
  Tmin_df <- data.frame(data$Tmini)
  colnames(Tmin_df) <- paste0(data$site, "_", data$year)
  
  
  #function to reset Sf when hit T_thres
  T_thres_func <- function(col){
    as.matrix(ave(Rf_df[[col]], cumsum(Tmin_df[[col]] <= T_thres), FUN = cumsum))
  }
  
  #Pull out years
  col_names <- names(Tmin_df)
  
  #Make empty matrix
  Sf <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- T_thres_func(col = year)
    Sf[,i] <- output
    
  }
  
  # DOY of budburst criterium
  doy <- apply(Sf, 2, function(xt){
    data$doy[which(xt >= F_crit)[1]]
  })
  
  # set export format, either a rasterLayer or a vector
  shape_model_output(data = data, doy = doy)
  
}



####SQWs_Pi_Tmin####

SQWs_Pi_Tmin <- function(par, data){
  
  # exit the routine as some parameters are missing
  if (length(par) != 7){
    stop("model parameter(s) out of range (too many, too few)")
  }
  
  #parameters
  t0 <- round(par[1])
  T_base <- par[2]
  b <- par[3]
  c <- par[4]
  F_crit <- par[5]
  P_req <- par[6]
  T_thres <- par[7]
  
  #Calculate Rw - sigmoid precip response
  Rw <- 1 / (1 + exp(-b * (data$Pi - c)))
  Rw[1:t0,] <- 0
  
  #change matrices to dataframes
  Rw_df <- data.frame(Rw)
  colnames(Rw_df) <- paste0(data$site, "_", data$year)
  
  Tmin_df <- data.frame(data$Tmini)
  colnames(Tmin_df) <- paste0(data$site, "_", data$year)
  
  #function to reset Sw when hit T_thres
  T_thres_func_Sw <- function(col){
    as.matrix(ave(Rw_df[[col]], cumsum(Tmin_df[[col]] <= T_thres), FUN = cumsum))
  }
  
  #Pull out years
  col_names <- names(Tmin_df)
  
  #Make empty matrix
  Sw <- matrix(NA, nrow = 365, ncol = length(col_names))
  
  #loop years (columns) through function
  for (i in 1:length(col_names)) {
    
    year <- col_names[i]
    output <- T_thres_func_Sw(col = year)
    Sw[,i] <- output
    
  }
  
  ## precip requirement has to be met before temp accumulation starts
  
  # assign output matrix
  k <- matrix(0, nrow = 365, ncol = length(col_names))
  rows <- nrow(data$Pi)
  cols <- ncol(data$Pi)
  
  # assign value of 1 once precip requirement met
  for (i in 1:cols){
    for (j in 1:rows){
      if(Sw[j,i] >= P_req){
        k[j:rows,i] <- 1
        break
      }
    }
  }
  
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



