##Code to loop through fitting all models - for Monsoon (supercomputer)
#Repeat separately for All Sites and each KG group dataset

#Read command line input from batch file (used as iteration number)
args = commandArgs(trailingOnly=TRUE)

#Set up working directory

#setwd("G:/My Drive/Resources/Monsoon/Test files")
#project_dir <- getwd()


#Create object with path to Grassland Model folder (folder with data files)
project_dir <- '/scratch/ap3379/Grassland_models/'


#set input and output directories
input_dir <- project_dir
output_dir <- paste0(project_dir, 'output/')
source_dir <- paste0(project_dir, 'source/')


#create files
#Specify dataset as "input_file" (All Sites or KG group)
params_file <- paste0(project_dir, 'parameter_ranges_paper.csv')
input_file <- paste0(input_dir, 'All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds')
output_file <- paste0(output_dir, 'output_run_', args, '.RDS')


#show progress
cat(as.character(Sys.time()), 'input_file = ', input_file, '\n')
cat(as.character(Sys.time()), 'output_file = ', output_file, '\n')
cat(as.character(Sys.time()), 'params_file = ', params_file, '\n')


#upload phenor package
#if(!require(devtools)){install.packages("devtools")}
#devtools::install_github("bluegreen-labs/phenor")
library(phenor)


#upload precip models
source(paste0(source_dir, "Precip_model_functions.R"))


#input data
input_data <- try(readRDS(input_file))

#show progress
cat(as.character(Sys.time()), 'model_fits started', '\n')


#List all models
models = c("LIN", "TT","TTs", "PTT", "PTTs", "M1", "M1s", "AT", "SQ", "SQb", "SM1", 
          "SM1b","PA", "PAb", "PM1", "PM1b", "UM1", "SGSI", "AGSI", "GRP",
          "M1W", "M1Ws", "PWT", "PWTs", "WT", "WTs", "PAW", "PAWs", "SQW", "SQWs", "SQWr", "SQWrs", 
          "SQW_NoPbase", "SQWs_cdd", "SQWs_Tmin", "SQWs_cdd_Tmin", "SQWs_Pi_Tmin",
          "SQW_cdd","SQW_Tmin","SQW_cdd_Tmin", "SQW_Pi_Tmin")


##Run all models together

#Write new function
pr_fit_new <- function(model){
  Fit <- pr_fit(model = model,
                data = input_data,
                method = "GenSA",
                control = list(max.call = 100000), 
                par_ranges = file.path(params_file))
  return(Fit)
}


#Apply new function to all models
model_fits <- mapply(pr_fit_new, model= models, SIMPLIFY = FALSE)

#show progress
cat(as.character(Sys.time()), 'model_fits finished', '\n')
cat(as.character(Sys.time()),'saving the data ...', '\n')


#Save data
saveRDS(model_fits, file = output_file)

#show progress
cat(as.character(Sys.time()), 'done!', '\n')







