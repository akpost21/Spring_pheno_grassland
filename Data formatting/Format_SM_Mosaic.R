#Add NLDAS soil moisture data to dataset

#load packages
library(plyr)
library(dplyr)
library(readr)
library(reshape2)

#set working directory
setwd("G:/My Drive/Project/Model edits/R files/Published code/Data formatting")

#read in phenocam data 
#(product of "download_format_pheno_files.R")
phen_dates_combined=readRDS("./output/All_sites_trans50.rds")


####Combine and format NLDAS SM files####
#Daily soil moisture was extracted for each site location as a table

##Read in all SM files
mydir <- "G:/My Drive/Project/Model edits/R files/Published code/Data formatting/Mosaic_SM"
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles

#combine data from all files into large list
SM_combined <- sapply(myfiles, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

#Remove unnecessary columns
SM_combined[,1] <- NULL
SM_combined$long <- NULL 
SM_combined$lat <- NULL

#rename site column
cols <- colnames(SM_combined)
cols <- cols[-1]
colnames(SM_combined) <- c("Site", cols)

#Transpose data to long format
SM_long <- melt(SM_combined, id.vars=c("Site", "year"))

#rename columns (specify dpylr package since rename also in "reshape" package)
SM_long <- dplyr::rename(SM_long, DOY = variable)
SM_long <- dplyr::rename(SM_long, soilM = value)

#check structure
str(SM_long)

# get rid of Dec 31 on leap years
SM_long$DOY <- as.numeric(SM_long$DOY)
SM_long<- SM_long[!SM_long$DOY == "366",]
range(SM_long$DOY)

#Divide by site
SM_split <- split(SM_long, SM_long$Site)

##save SM file as RDS
saveRDS(SM_split, file = "./output/SM_all_GR_sites_SplitbySite_Mosaic_0-10.rds")


####add soil moisture to each site in original data file####

#Extract site names, save as vector
sites <- names(phen_dates_combined)

#function to extract correct years for each site and 
#adjust for spring offset (default offset = DOY 264)
#save soil moisture (SM) in a matrix (soilM)

add_SM <- function(site, offset = 264){
  
  #identify years with transition dates
  years <- unique(phen_dates_combined[[site]]$year)
  
  #make empty matrix to hold soil moisture data
  soilM <- matrix(NA,
                  nrow = 365,
                  ncol = length(years))
  
  
  #Adjust DOY to start Sept 21 of previous year
  for (j in 1:length(years)) {
    
    if (offset < 365) {
      
      loc <- c(which((SM_split[[site]]$year == (years[j] - 1) &
                        SM_split[[site]]$DOY >= offset)), 
               which((SM_split[[site]]$year == years[j] &
                        SM_split[[site]]$DOY < offset)))
    } else {
      
      loc <- which(SM_split[[site]]$year == years[j])
      
    }
    
    soilM[, j] <- SM_split[[site]]$soilM[loc]
    #colnames(soilM)[1:length(years)] <- years
    
  }
  
  as.matrix(soilM)
  
}

# Cycle through all sites, run function "add_SM" to return soilM matrix
# Then add that matrix to the current phen_dates_combined file

for (i in 1:length(sites)){
  
  site <- sites[i]
  phen_dates_combined[[site]]$SM <- add_SM(site)
  
}


##save file with SM added for all sites
saveRDS(phen_dates_combined, file = "./output/All_sites_trans50_SM_Mosaic_0-10.rds")



