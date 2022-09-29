##Split sites by Koppen-Geiger region

#load packages
library(plyr)
library(dplyr)
library(readr)
library(reshape2)
library(stringr)
library(tidyverse)

#set working directory
setwd("G:/My Drive/Project/Model edits/R files/Published code/Data formatting")

#read in phenocam data 
All_sites = readRDS("./output/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")

#Import site classifications
metadata <- read.csv("./grassland_sites.CSV")

#Select relevant columns
metadata2 <- data.frame(metadata[,1], metadata[,12])

#Rename columns
colnames(metadata2) <- c("site", "KG_class")

#Remove duplicate entries
metadata3 <- distinct(metadata2)

#first 2 letters
KG_div <- as.matrix(substr(metadata3$KG_class, start = 1, stop = 2))
colnames(KG_div) <- "KG_class2"
metadata4 <- cbind(metadata3, KG_div)

#combine B groups
metadata4$KG_class2[metadata4$KG_class2 == "BS"] <- "B"
metadata4$KG_class2[metadata4$KG_class2 == "BW"] <- "B"



###Split by 2-letter KG classification

#See # of sites for each KG group
table(metadata4['KG_class2'])


#identify sites not in this group
no_B_sites <- subset(metadata4, (KG_class2 != "B"))
no_B_sites <- no_B_sites$site
no_B_sites

#delete sites not in this KG group
for (i in 1:length(no_B_sites)){
  
  site <- no_B_sites[i]
  All_sites[[site]] <- NULL
  
}

#save dataset for this KG group
saveRDS(All_sites, file = "./output/KG_B_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")




##Cf sites

#re-upload original phen_dates_combined file!
All_sites = readRDS("./output/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")

#identify sites not in this group
no_Cf_sites <- subset(metadata4, (KG_class2 != "Cf"))
no_Cf_sites <- no_Cf_sites$site
no_Cf_sites

#delete sites not in this KG group
for (i in 1:length(no_Cf_sites)){
  
  site <- no_Cf_sites[i]
  All_sites[[site]] <- NULL
  
}

#save dataset for this KG group
saveRDS(All_sites, file = "./output/KG_Cf_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")



##Cs sites

#re-upload original phen_dates_combined file!
All_sites = readRDS("./output/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")

#identify sites not in this group
no_Cs_sites <- subset(metadata4, (KG_class2 != "Cs"))
no_Cs_sites <- no_Cs_sites$site
no_Cs_sites

#delete sites not in this KG group
for (i in 1:length(no_Cs_sites)){
  
  site <- no_Cs_sites[i]
  All_sites[[site]] <- NULL
  
}

#save dataset for this KG group
saveRDS(All_sites, file = "./output/KG_Cs_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")



#Df sites

#re-upload original phen_dates_combined file!
All_sites = readRDS("./output/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")

#identify sites not in this group
no_Df_sites <- subset(metadata4, (KG_class2 != "Df"))
no_Df_sites <- no_Df_sites$site
no_Df_sites

#delete sites not in this KG group
for (i in 1:length(no_Df_sites)){
  
  site <- no_Df_sites[i]
  All_sites[[site]] <- NULL
  
}

#save dataset for this KG group
saveRDS(All_sites, file = "./output/KG_Df_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
