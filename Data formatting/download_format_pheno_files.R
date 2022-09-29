#download spring transition date files for all grassland sites

# required libraries
library(phenor)
library(zoo)
library(phenocamr)
library(ggplot2)
library(stringi)



####Download & process data for each site#### 

setwd("G:/My Drive/Project/Model edits/R files/Published code/Data formatting")

#upload list of sites
site_table <- read.table("./grassland_sites.CSV",sep = ",", header = TRUE)

#Find table length (# of sites)
total_num <- dim(site_table)[1]
total_num


# set working directory (to location to store raw data files)
setwd("G:/My Drive/Project/Model edits/R files/Published code/Data formatting/raw_data")
getwd()

#loop through sites to download and process data

for (i in 1:total_num){
  
  site = site_table[i,1]
  roi = site_table[i,3]
  
  #use 3-day data, don't process yet
  download_phenocam(site = site,
                  veg_type = "GR",
                  roi_id = roi,
                  frequency = 3,
                  outlier_detection = FALSE, 
                  smooth = FALSE, 
                  phenophase = FALSE, 
                  out_dir = ".")
                  
  file <- paste0("./", site, "_GR_", roi, "_", '3day.csv')
   
  #end at year 2020, then process (remove outliers & smooth)              
  process_phenocam(file.path(file),
                  truncate = 2020, 
                  out_dir = ".")            
              
}

###Extra manual steps###

##remove ibp0 site files from folder (data quality not good)
#remove segabluechute sites 1-8 files from folder (experimental plots)


###Combine all sites and format data for modeling with phenor####

#make sure only files you want to combine are in the working directory folder
#Normal offset = 264 (water year starts Sept 21)
#threshold = 50% green-up threshold
#direction = rising means spring transition dates
#gcc_90 = use 90th percentile of Pixel GCC values


phenocam_transition_dates <- pr_fm_phenocam(
  path = ".",
  direction = "rising",
  gcc_value = "gcc_90",
  threshold = 50,
  offset = 264,
  internal = TRUE
)




#Replace datasets for sites with more than 1 annual green-up peak (tonzi, vaira, forbes).
#Use the transition date files provided in the "2 annual peak sites" folder for these sites instead.
#These have been edited to include the correct "water years" for each transition date so 
#the correct environmental data is downloaded. 
#We will process these separately and over-write the existing entries in the larger dataset


#Call adapted phenor function that recognizes years with 2 transition dates
#process sites with 2 green-up peaks separately (tonzi, vaira, forbes)

source("G:/My Drive/Project/Model edits/R files/Published code/Data formatting/pr_fm_phenocam_date.R")


#set working directory to folder with files for just the 2 green-up peak sites
setwd("G:/My Drive/Project/Model edits/R files/Published code/Data formatting/raw_data/2 annual peak sites/")

CA_sites <- pr_fm_phenocam_date(
  path = ".",
  direction = "rising",
  gcc_value = "gcc_90",
  threshold = 50,
  offset = 264,
  internal = TRUE
)



#Replace transition dates with estimated 2nd peak transition dates
CA_sites$vaira$transition_dates <- c(67, 53, 63, 23, 27, 35, 58, 70)
CA_sites$tonzi$transition_dates <- c(99, 84, 84, 53, 64, 91, 90, 77)
CA_sites$forbes$transition_dates <- c(3, 33, 69)


#overwrite entries in large dataset
phenocam_transition_dates$vaira <- CA_sites$vaira
phenocam_transition_dates$tonzi <- CA_sites$tonzi
phenocam_transition_dates$forbes <- CA_sites$forbes

#save phenocam data file
saveRDS(phenocam_transition_dates, "G:/My Drive/Project/Model edits/R files/Published code/Data formatting/output/All_sites_trans50.rds")



