#"Leave-one-out" model validation code for Monsoon

#upload phenor
library(phenor)

#Read command line input from batch file (used as iteration number)
args = commandArgs(trailingOnly=TRUE)

#Create object with path to Grassland Model folder
project_dir <- '/scratch/ap3379/Grassland_models/'


#set input and output directories
input_dir <- project_dir
output_dir <- paste0(project_dir, 'output_val/')
source_dir <- paste0(project_dir, 'source/')


#create files
#Specify dataset as "input_file" (All Sites or KG group)
input_file <- paste0(input_dir, 'All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds')
params_file <- paste0(project_dir, 'parameter_ranges_paper.csv')
output_file <- paste0(output_dir, 'output_val_', args, '.RDS')


#show progress
cat(as.character(Sys.time()), 'input_file = ', input_file, '\n')
cat(as.character(Sys.time()), 'output_file = ', output_file, '\n')
cat(as.character(Sys.time()), 'params_file = ', params_file, '\n')


#upload precip models
source(paste0(source_dir, "Precip_model_functions.R"))


####Create model-fitting function####

fit_models <- function(data, model, iterations){

  pr_fit_new <- function(model){
     Fit <- pr_fit(model = model,
                data = data,
                method = "GenSA",
                control = list(max.call = iterations), 
                par_ranges = file.path(params_file))
    return(Fit)
  }
  
    mapply(pr_fit_new, model= model, SIMPLIFY = FALSE)

}



####Create validation function####

val_models <- function(site, data, model, iter){
  
  #re-upload all sites
  data_input <- data
  
  #exclude 1 site
  data_input[[site]] <- NULL
  
  #Run models
  model_fits <- fit_models(data = data_input, model = model, iterations = iter)
  
  #predict excluded site
  predict <- pr_predict(data = data[[site]], par = model_fits[[model]]$par, model = model)
  
  
  #Make results into table
  All_trans_dates <-  do.call(rbind, Map(data.frame, location = lapply(data_input, '[', 1), 
                                         observed = lapply(data_input, '[[', 5), 
                                         year = lapply(data_input, '[[', 7)))
  
  #Make data frame of all observed vs predicted
  results <- do.call(rbind, Map(data.frame, model = rep(unlist(lapply(model_fits, '[', 1)), each = length(All_trans_dates$year)),
                                predicted = unlist(lapply(model_fits, '[[', 4)), 
                                observed = unlist(lapply(model_fits, '[[', 3))))
  
  
  #Use automatic alphabetical order to add location names to results df
  #number of sites
  s <- data.frame(All_trans_dates$site, All_trans_dates$year)
  #number of models
  n <- length(model)
  #Replicate site names based on number of models
  Site_names <- do.call("rbind", replicate(n, s, simplify = FALSE))
  
  
  #Combine site names with results data frame, rename columns
  results2 <- cbind(results, Site_names)
  names(results2)[names(results2) == 'All_trans_dates.site'] <- 'site'
  names(results2)[names(results2) == 'All_trans_dates.year'] <- 'year'
  
  #pull out model parameters
  par = model_fits[[model]]$par
  rmse = model_fits[[model]]$rmse
  
  #save results in nested list
  out <- list(par = par, rmse = rmse, model_output = results2, predict = predict)
  
}



####Run function as loop####

#input data
data_set <- try(readRDS(input_file))

site_list <- names(data_set)

#show progress
cat(as.character(Sys.time()), 'validation started', '\n')


#create empty list
val_results <- list()


#loop function for each site
##change model and iteration number here!##

for (i in 1:length(site_list)){
  
  site <- site_list[i]
  
  output <- val_models(data = data_set, site = site, model = "SQWs_Tmin", iter = 100000)
  
  val_results[[i]] <- output
  
}


#rename nested list
names(val_results) <- site_list

#show progress
cat(as.character(Sys.time()), 'validation finished', '\n')
cat(as.character(Sys.time()),'saving the data ...', '\n')


#Save data
saveRDS(val_results, file = output_file)

cat(as.character(Sys.time()), 'done!', '\n')

