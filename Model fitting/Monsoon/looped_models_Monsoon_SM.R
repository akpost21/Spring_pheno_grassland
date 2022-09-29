##Code to loop through fitting all SM models - for Monsoon (supercomputer)
#Repeat separately for All Sites and each KG group dataset

#Read command line input from batch file (used as iteration number)
args = commandArgs(trailingOnly=TRUE)

#Set up working directory

#setwd("G:/My Drive/Resources/Monsoon/Test files")
#project_dir <- getwd()

project_dir <- '/scratch/ap3379/Grassland_models/'


#set input and output directories
input_dir <- project_dir
output_dir <- paste0(project_dir, 'output/')
source_dir <- paste0(project_dir, 'source/')


#create files
#Specify dataset as "input_file" (All Sites or KG group)
params_file <- paste0(project_dir, 'parameter_ranges_paper.csv')
input_file <- paste0(input_dir, 'All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds')
output_file <- paste0(output_dir, 'SM_output_run_', args, '.RDS')


#show progress
cat(as.character(Sys.time()), 'input_file = ', input_file, '\n')
cat(as.character(Sys.time()), 'output_file = ', output_file, '\n')
cat(as.character(Sys.time()), 'params_file = ', params_file, '\n')


#upload phenor package
#if(!require(devtools)){install.packages("devtools")}
#devtools::install_github("bluegreen-labs/phenor")
library(phenor)

#Read in edited functions (pr_flatten_SM, pr_fit_SM): 
source(paste0(source_dir, "pr_flatten_SM.R"))
source(paste0(source_dir, "pr_fit_SM.R"))

#upload SM models
source(paste0(source_dir, "SM_model_functions.R"))


#input data
input_data <- try(readRDS(input_file))

#show progress
cat(as.character(Sys.time()), 'model_fits started', '\n')


#All SM models
models = c("GRP_SM_1", "M1W_SM", "M1Ws_SM", "PAW_SM", "PAWs_SM", "PM1_SM",
           "PWT_SM", "PWTs_SM", "SM1_SM", "SM1r_SM", "SQW_SM", "SQWs_SM", "SQWr_SM",
           "SQWrs_SM", "WT_SM", "WTs_SM")


##Run all models together

#Write new function
pr_fit_new <- function(model){
  Fit <- pr_fit_SM(model = model,
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

cat(as.character(Sys.time()), 'done!', '\n')




