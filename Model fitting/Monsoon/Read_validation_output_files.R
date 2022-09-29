#Combine and summarize validation output files

#load packages
library(dplyr)
library(ggpubr)
library(plotly)
library(tidyr)
library(ggplot2)
library(tidyverse)


##Repeat code separately for each AllSite/KG Group validation run


##Read in all output files for a single model
mydir <- "G:/My Drive/Project/Model edits/R files/Monsoon/output_val/AllSites_SQWs_Tmin_100000"
myfiles <- list.files(path=mydir, pattern="*.RDS", full.names=TRUE)
myfiles

#read in all files
list <- c(1:25)

for (i in list) {
  filename <- paste0("output_val_", i)
  wd <- paste0(mydir, "/output_val_", i, ".RDS")
  assign(filename, readRDS(wd))
}


###upload & designate appropriate files

#upload datasets
setwd("G:/My Drive/Project/Model edits/R files/Published code/Model fitting")

All_sites <- readRDS("./input/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_B_sites <- readRDS("./input/KG_B_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cf_sites <- readRDS("./input/KG_Cf_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cs_sites <- readRDS("./input/KG_Cs_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Df_sites <- readRDS("./input/KG_Df_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")


#site list of dataset and validation results need to be in same order!
#arrange both alphabetically

#Make sure sites in data set are in alphabetical order
All_sites = All_sites[order(names(All_sites))]

#Designate dataset used for validation run
data_set <- All_sites
site_list <- names(data_set)


###Adapt AIC function from phenor
AIC_pheno <- function(measured, predicted, k){
  
  # calculate number of observations
  n <- length(measured)
  
  # calculate residual sum of squares
  RSS <- sum((measured - predicted)^2)
  
  # AIC
  AIC <- 2*k + n * log(RSS/n)

  # return both AIC
  return(AIC)
}


#custom RMSE function used by phenor
rmse <- function(measured, predicted){
  
  return(sqrt(mean((measured - predicted) ^ 2, na.rm = T)))
  
}


####Pull out stats from all runs####


#Make function to extract data from each run
read_stats <- function (data){
  
  #Pull data from 1 run
  val_results <- data
  
  #observed vs predicted for validation site
  #predicted = predicted trans dates when that site not included in the model fitting
  val_data <-  do.call(rbind, Map(data.frame, location = site_list, 
                                  year = lapply(data_set, '[', 7),
                                  predicted = lapply(val_results, '[[', 4), 
                                  observed = lapply(data_set, '[[', 5))) 
  
  
  #calculate difference between observed & predicted
  val_data$diff <- val_data$predicted - val_data$observed
  
  #calculate stats
  RMSE <- rmse(val_data$observed, val_data$predicted)

  AIC <- AIC_pheno(val_data$observed, val_data$predicted, k =length(val_results[[1]]$par))
  
  out <- list(data = val_data, RMSE = RMSE, AIC = AIC)
  
  return(out)
  
}

##test function on 1 run

#arrange sites alphabetically
output_val_1 <- output_val_1[order(names(output_val_1))]

test <- read_stats(output_val_1)


###combine all output runs into 1 list
list_runs =lapply(ls(pattern="output"),get)

#list run names
list_names <- sapply(ls(pattern = "output"), as.name)

#Add run names to output list
names(list_runs) <- list_names

#Alphabetize sites within each output 
list_runs <- lapply(list_runs, function(x) x[order(names(x))])

#apply "read_stats" function to all runs
list_stats <- lapply(list_runs, read_stats)


#Make table of all validation AIC & RMSE values for each run
stats_table <- do.call(rbind, Map(data.frame,
                                  AIC = unlist(lapply(list_stats, '[[', 3)),
                                  RMSE = unlist(lapply(list_stats, '[[', 2))))

#order validation runs by lowest RMSE
stats_table <- stats_table[order(stats_table$RMSE),]
stats_table

#Remove runs that with outliers
stats_table2 <- stats_table[-c(24,25),]

#RMSE stats
min(stats_table2$RMSE)
mean(stats_table2$RMSE)
sd(stats_table2$RMSE)

#save stats table
write.csv(stats_table, "G:/My Drive/Project/Model edits/R files/Monsoon/output_val/SQWs_Tmin_AllSites_val_table.csv")


#average RMSE of fitted models (with all sites except excluded one)
#listed as the results for the run when that site was excluded

#function to pull RMSE from each run
Avg_model_RMSE <- function (data){
  
  #Pull data from 1 run
  val_results <- data

  #Extract RMSE
  RMSE_table <- t(as.data.frame(lapply(val_results, '[[', 2)))
  
  RMSE_mean <- mean(unlist(lapply(val_results, '[[', 2)))
  
  return(RMSE_mean)

}


#apply function to all runs
avg_RMSE <- data.frame(sapply(list_runs, Avg_model_RMSE))

colnames(avg_RMSE) <- "mean_RMSE"

avg_RMSE


#Mean & SD RMSE
min(avg_RMSE$mean_RMSE)
mean(avg_RMSE$mean_RMSE)
sd(avg_RMSE$mean_RMSE)


###Make table of all output together

All_trans_dates <-  do.call(rbind, Map(data.frame, site = lapply(data_set, '[', 1), 
                                       observed = lapply(data_set, '[[', 5), 
                                       year = lapply(data_set, '[[', 7)))


results <- do.call(rbind, Map(data.frame, Run = rep(names(list_stats), each = length(All_trans_dates$year)),
                              site = unlist(lapply(list_stats, function(x) x$data[1])), 
                              year = unlist(lapply(list_stats, function(x) x$data[2])),
                              predicted = unlist(lapply(list_stats, function(x) x$data[3])),
                              observed = unlist(lapply(list_stats, function(x) x$data[4])),
                              diff = unlist(lapply(list_stats, function(x) x$data[5]))))
  

##Extract and save data from best run
#save one each time you run the above code for a KG group validation

AllSites_best_val <- filter(results, Run == "output_val_1")
#KG_B_best_val <- filter(results, Run == "output_val_3")
#KG_Cf_best_val <- filter(results, Run == "output_val_8")
#KG_Cs_best_val <- filter(results, Run == "output_val_15")
#KG_Df_best_val <- filter(results, Run == "output_val_1")



###Graph results of all validations runs
All <- ggplot(data = results, mapping = aes(x = observed, y = predicted ))+
  geom_point(aes(col = site))+
  geom_smooth(method= "lm", se = FALSE, fullrange = F, col = "black")+
  geom_abline(intercept = 0, slope = 1, col = "dark grey")+
  facet_wrap(facets = vars(Run), scales = "free")+
  theme(aspect.ratio=1)+
  stat_cor(aes(label =..rr.label..))
All + labs(title="Model Fits", y="Predicted", x = "Observed")


#Make graphs interactive
ggplotly(All)


#histogram of validation results
breaks_fun <- function(x) {
  seq(floor(min(x)), floor(min(x) + 5), 1)
}

limits_fun <- function(x) {
  c((min(x)), min(x) + 5)
}

stats_table %>%
  ggplot(aes(x= RMSE)) +
  geom_histogram(binwidth = 1, center = 0) + 
  scale_x_continuous(breaks = breaks_fun, limits = limits_fun)+
  ylab("# Runs") +
  xlab("RMSE (days)")+
  ggtitle("Validation: SQWs_Tmin")


####Determine AIC when best validation runs for each KG group is combined####
#Run above code for each KG group & save best validation run

#Combine validation results for all KG groups

val_combo <- rbind(KG_B_best_val, KG_Cf_best_val, KG_Cs_best_val, KG_Df_best_val)

#find RMSE of entire validation dataset
rmse(val_combo$observed, val_combo$predicted)

#save val combo
write.csv(val_combo, "G:/My Drive/Project/Model edits/R files/Monsoon/output_val/SQWs_Tmin_KGcombo_val.csv")




##Extra

####Function to extract model parameters for each site from every run####
#Example of par_num for SQW: t0 = 1, T-base = 2, F_crit = 3, P_base = 4, P_req = 5

par_extract <- function(data, par_num){
  
  #Pull data from 1 run
  val_results <- data
  
  output <- list()
  
  for (i in 1:length(site_list)){
    
    site <- site_list[i]
    
    output[i] <- val_results[[site]]$par[par_num]
    
  }
  
  output <- unlist(output)
  output_list <- data.frame(cbind(site_list, output))
  output_list$output <- as.numeric(output_list$output)
  
  mean = mean(output_list[,2])
  sd = sd(output_list[,2])
  range = range(output_list[,2])
  
  out <-list(par = as.matrix(output_list), par_stats = c(mean = mean, sd = sd, min = range[1], max = range[2]))
  
  return(out)
  
}

#test function
test <- par_extract(data = output_val_1, par_num = 1)


#apply function to all runs
param_list <- lapply(list_runs, par_extract, par_num = 1)


#Extract selected parameter for each run
param_table <- as.data.frame(lapply(param_list, function(x) x$par[,2]))
rownames(param_table) <- site_list

#Extract parameter summary statistics
param_sumstats_table <- t(as.data.frame(lapply(param_list, function(x) x$par_stats)))


