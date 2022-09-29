### predict future SOS dates
#Repeat for each different site
#This examples uses "ibp" as the site

library(phenor)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(plyr)
library(gridExtra)
library(lmodel2)
library(lubridate)
library(tidyr)
library(tidyverse)
library(zoo)

####Format data####

#identify GCM (change each time)
data_source = "MPI-ESM-MR.CRCM5-UQAM"

#set working directory to location of raw climate data
setwd(paste0("G:/My Drive/Project/Model edits/R files/Published code/Future projections/Data/", data_source, "/ibp/"))


##load raw data files (repeat for each one)

# #MPI-ESM_LR.CRCM5-UQAM
# Precip_ibp <- read.csv("prec_prec.rcp85.MPI-ESM-LR.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.MPI-ESM-LR.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.MPI-ESM-LR.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")

#CanESM2.CanRCM4
# Precip_ibp <- read.csv("prec_prec.rcp85.CanESM2.CanRCM4.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.CanESM2.CanRCM4.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.CanESM2.CanRCM4.day.NAM-22i.mbcn-Daymet.csv")

#HadGEM2-ES.RegCM4
# Precip_ibp <- read.csv("prec_prec.rcp85.HadGEM2-ES.RegCM4.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.HadGEM2-ES.RegCM4.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.HadGEM2-ES.RegCM4.day.NAM-22i.mbcn-Daymet.csv")

#MPI-ESM-LR.RegCM4
# Precip_ibp <- read.csv("prec_prec.rcp85.MPI-ESM-LR.RegCM4.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.MPI-ESM-LR.RegCM4.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.MPI-ESM-LR.RegCM4.day.NAM-22i.mbcn-Daymet.csv")

#CanESM2.CRCM5-UQAM
# Precip_ibp <- read.csv("prec_prec.rcp85.CanESM2.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.CanESM2.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.CanESM2.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")

#GEMatm-Can.CRCM5-UQAM
# Precip_ibp <- read.csv("prec_prec.rcp85.GEMatm-Can.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.GEMatm-Can.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.GEMatm-Can.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")

#GFDL-ESM2M.WRF
# Precip_ibp <- read.csv("prec_prec.rcp85.GFDL-ESM2M.WRF.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.GFDL-ESM2M.WRF.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.GFDL-ESM2M.WRF.day.NAM-22i.mbcn-Daymet.csv")

#GFDL-ESM2M.RegCM4
# Precip_ibp <- read.csv("prec_prec.rcp85.GFDL-ESM2M.RegCM4.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.GFDL-ESM2M.RegCM4.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.GFDL-ESM2M.RegCM4.day.NAM-22i.mbcn-Daymet.csv")

#GEMatm-MPI.CRCM5-UQAM
# Precip_ibp <- read.csv("prec_prec.rcp85.GEMatm-MPI.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.GEMatm-MPI.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.GEMatm-MPI.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")

#MPI-ESM-LR.WRF
# Precip_ibp <- read.csv("prec_prec.rcp85.MPI-ESM-LR.WRF.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.MPI-ESM-LR.WRF.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.MPI-ESM-LR.WRF.day.NAM-22i.mbcn-Daymet.csv")

#HadGEM2-ES.WRF
# Precip_ibp <- read.csv("prec_prec.rcp85.HadGEM2-ES.WRF.day.NAM-22i.mbcn-Daymet.csv")
# Tmin_ibp <- read.csv("tmin_tmin.rcp85.HadGEM2-ES.WRF.day.NAM-22i.mbcn-Daymet.csv")
# Tmax_ibp <- read.csv("tmax_tmax.rcp85.HadGEM2-ES.WRF.day.NAM-22i.mbcn-Daymet.csv")

#MPI-ESM-MR.CRCM5-UQAM
Precip_ibp <- read.csv("prec_prec.rcp85.MPI-ESM-MR.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
Tmin_ibp <- read.csv("tmin_tmin.rcp85.MPI-ESM-MR.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")
Tmax_ibp <- read.csv("tmax_tmax.rcp85.MPI-ESM-MR.CRCM5-UQAM.day.NAM-22i.mbcn-Daymet.csv")


##Precip##
#Each data set needs different processing
#only use the code segments below that each one needs

#remove & rename columns
Precip_ibp$station <- NULL
colnames(Precip_ibp) <- c("time", "lat", "long", "var")

#format and extract dates, add doy
Precip_ibp$date=as.Date(substr(Precip_ibp$time,1,10))

#If NA present, remove them
#Precip_ibp <- Precip_ibp[!is.na(Precip_ibp$date), ]

#fill in missing dates
# Precip_ibp <- Precip_ibp %>%
# complete(date = seq.Date(min(date), max(date), by="day"))

#fill in added dates with precip = 0 mm 
#Precip_ibp$var[is.na(Precip_ibp$var)] <- 0

#extract other date elements
Precip_ibp$month=month(Precip_ibp$date)
Precip_ibp$day=day(Precip_ibp$date)
Precip_ibp$year=year(Precip_ibp$date)
Precip_ibp$doy=yday(Precip_ibp$date)
#Precip_ibp$month_day = paste0(Precip_ibp$month, Precip_ibp$day)


#check # days in each year to see if leap day included
DOY_count <- Precip_ibp %>% group_by(year) %>%tally()
DOY_count <- as.data.frame(DOY_count)
DOY_count

#remove Dec 31 if leap day included
Precip_ibp=Precip_ibp[!Precip_ibp$doy == 366,]

#remove incomplete years
#Precip_ibp= Precip_ibp[!Precip_ibp$year == 2099,]

#remove leap day
#Precip_ibp=Precip_ibp[!Precip_ibp$month_day == 229 ,]


#Add Dec 31, 2100 if missing
Dec31 <- data.frame(time = NA, 
                    lat = 39.056,
                    long = -95.19,
                    var = 0,
                    date = NA,
                    month = 12,
                    day = 31,
                    year = 2100,
                    doy = 365)

Precip_ibp <- rbind(Precip_ibp, Dec31)

#if leap day already excluded, check DOY labels and re-number if necessary
Precip_ibp$doy <- rep(1:365,95)

#check # days in each year to see if leap day included
DOY_count <- Precip_ibp %>% group_by(year) %>%tally()
DOY_count <- as.data.frame(DOY_count)
DOY_count


##Tmin##
#Repeat for Tmin

#remove & rename columns
Tmin_ibp$station <- NULL
colnames(Tmin_ibp) <- c("time", "lat", "long", "var")

#format and extract dates, add doy
Tmin_ibp$date=as.Date(substr(Tmin_ibp$time,1,10))

#If NA present, remove them
#Tmin_ibp <- Tmin_ibp[!is.na(Tmin_ibp$date), ]

#fill in missing dates
# Tmin_ibp <- Tmin_ibp %>%
#   complete(date = seq.Date(min(date), max(date), by="day"))

#fill in added dates with mean of day before and after
#Tmin_ibp$var <- na.approx(Tmin_ibp$var)

#extract other date elements
Tmin_ibp$month=month(Tmin_ibp$date)
Tmin_ibp$day=day(Tmin_ibp$date)
Tmin_ibp$year=year(Tmin_ibp$date)
Tmin_ibp$doy=yday(Tmin_ibp$date)
#Tmin_ibp$month_day = paste0(Tmin_ibp$month, Tmin_ibp$day)


#check # days in each year to see if leap day included
DOY_count <- Tmin_ibp %>% group_by(year) %>%tally()
DOY_count <- as.data.frame(DOY_count)
DOY_count

#remove Dec 31 if leap day included
Tmin_ibp=Tmin_ibp[!Tmin_ibp$doy == 366,]

#remove incomplete years
#Tmin_ibp= Tmin_ibp[!Tmin_ibp$year == 2099,]


#Add Dec 31, 2100 if missing
Dec31 <- data.frame(time = NA, 
                    lat = 39.056,
                    long = -95.19,
                    var = NA,
                    date = NA,
                    month = 12,
                    day = 31,
                    year = 2100,
                    doy = 365)

Tmin_ibp <- rbind(Tmin_ibp, Dec31)

#if leap day already excluded, check DOY labels and re-number if necessary
Tmin_ibp$doy <- rep(1:365,95)

#check # days in each year to see if leap day included
DOY_count <- Tmin_ibp %>% group_by(year) %>%tally()
DOY_count <- as.data.frame(DOY_count)
DOY_count



##Tmax##
#Repeat for Tmax

#remove & rename columns
Tmax_ibp$station <- NULL
colnames(Tmax_ibp) <- c("time", "lat", "long", "var")

#format and extract dates, add doy
Tmax_ibp$date=as.Date(substr(Tmax_ibp$time,1,10))

#If NA present, remove them
#Tmax_ibp <- Tmax_ibp[!is.na(Tmax_ibp$date), ]

#fill in missing dates
# Tmax_ibp <- Tmax_ibp %>%
#   complete(date = seq.Date(min(date), max(date), by="day"))

#fill in added dates with mean of day before and after
#Tmax_ibp$var <- na.approx(Tmax_ibp$var)

#extract other date elements
Tmax_ibp$month=month(Tmax_ibp$date)
Tmax_ibp$day=day(Tmax_ibp$date)
Tmax_ibp$year=year(Tmax_ibp$date)
Tmax_ibp$doy=yday(Tmax_ibp$date)
#Tmax_ibp$month_day = paste0(Tmax_ibp$month, Tmax_ibp$day)


#check # days in each year to see if leap day included
DOY_count <- Tmax_ibp %>% group_by(year) %>%tally()
DOY_count <- as.data.frame(DOY_count)
DOY_count

#remove Dec 31 if leap day included
Tmax_ibp=Tmax_ibp[!Tmax_ibp$doy == 366,]

#remove incomplete years
#Tmax_ibp= Tmax_ibp[!Tmax_ibp$year == 2099,]

#Add Dec 31, 2100 if missing
Dec31 <- data.frame(time = NA, 
                    lat = 39.056,
                    long = -95.19,
                    var = NA,
                    date = NA,
                    month = 12,
                    day = 31,
                    year = 2100,
                    doy = 365)

Tmax_ibp <- rbind(Tmax_ibp, Dec31)

#if leap day already excluded, check DOY labels and re-number if necessary
Tmax_ibp$doy <- rep(1:365,95)


#check # days in each year to see if leap day included
DOY_count <- Tmax_ibp %>% group_by(year) %>%tally()
DOY_count <- as.data.frame(DOY_count)
DOY_count




####format data for phenor####

#Upload formatted data for AllSites
AllSites <- readRDS("G:/My Drive/Project/Model edits/R files/Published code/Model fitting/input/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")

#Extract single site
trans_dates <- AllSites$ibp

#Add dummy dates
trans_dates$year=2007:2100
trans_dates$transition_dates=rep(120,94)


##Made function to add data to dataset

add_data <- function(data, offset = 264){
  
  #identify years with transition dates
  years <- unique(trans_dates$year)
  
  #make empty matrix to hold soil moisture data
  df <- matrix(NA,
                  nrow = DOY_count[,2],
                  ncol = length(years))
  
  
  #Adjust DOY to start Sept 21 of previous year
  for (j in 1:length(years)) {
    
    if (offset < 365) {
      
      loc <- c(which((data$year == (years[j] - 1) &
                        data$doy >= offset)), 
               which((data$year == years[j] &
                        data$doy < offset)))
    } else {
      
      loc <- which(data$year == years[j])
      
    }
    
    df[, j] <- data$var[loc]

  }
  
  as.matrix(df)
  
}

#test function
test <- add_data(Precip_ibp)

#add data to dataset
trans_dates$Pi <- add_data(Precip_ibp)
trans_dates$Tmaxi <- add_data(Tmax_ibp)
trans_dates$Tmini <- add_data(Tmin_ibp)

#calculate avg temp
list <- list(trans_dates$Tmaxi,trans_dates$Tmini)
trans_dates$Ti <- Reduce("+", list) / length(list)


#save RDS
saveRDS(trans_dates, paste0(data_source, "_dataset.RDS"))


###load parameters from phenor runs

#load models
#upload precip models
source("G:/My Drive/Project/Model edits/R files/Published code/Model fitting/Precip_model_functions.R")

#Read in best parameters for Koppen-Geiger "B" group
B_BestRuns <- readRDS("G:/My Drive/Project/Model edits/R files/Published code/Model fitting/output/KG_B_BestRuns.RDS")

#Extract parameters for top model (SQWs_Tmin)
par <- B_BestRuns$SQWs_Tmin$par
par

#use fitted model to predict future trans dates
predict <- pr_predict(data = trans_dates, par = par, model = SQWs_Tmin)
predict

#Extract years
years <- unique(trans_dates$year)

#Make into matrix
ibp_predict <- data.frame(cbind(year = years, predicted = predict))

#remove years that did not converge
ibp_predict <- subset(ibp_predict, predicted < 1000) 

#save results
write.csv(ibp_predict,paste0(data_source,'_ibp_future.csv'), row.names = FALSE)

