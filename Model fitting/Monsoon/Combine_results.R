#Pull out best run for each model
#combine KG region results & calculate AIC & RMSE of combined dataset
#calculate residuals to use for dendrogram

#load packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(dendextend)

#upload datasets
setwd("G:/My Drive/Project/Model edits/R files/Published code/Model fitting")

All_sites <- readRDS("./input/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_B_sites <- readRDS("./input/KG_B_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cf_sites <- readRDS("./input/KG_Cf_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cs_sites <- readRDS("./input/KG_Cs_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Df_sites <- readRDS("./input/KG_Df_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")


##Read in all output files for one group 
##repeat this code section separately for All Sites and each KG group
##Also repeat for soil moisture model runs with All Sites

mydir <- "G:/My Drive/Project/Model edits/R files/Monsoon/output/AllSites_output_Feb4_100000"
myfiles <- list.files(path=mydir, pattern="*.RDS", full.names=TRUE)
myfiles

#read in all files
list <- c(1:25)

for (i in list) {
  filename <- paste0("output_run_", i)
  wd <- paste0(mydir, "/output_run_", i, ".RDS")
  assign(filename, readRDS(wd))
}



####Pull out stats from all runs####

#Make function to extract data from each run
read_stats <- function (data){
  
  #Pull data from 1 run
  model_fits <- data
  filename <- paste0("stats_", i)
  
  #Pull data from each run
  #Extract elements (rmse = 5, AIC = 7) from nested lists ([[ = 2nd level list)
  models <- names(model_fits)
  RMSE <- lapply(model_fits, '[[', 5)
  AIC_list <- lapply(model_fits, '[[', 7)
  AIC <- lapply(AIC_list, '[[', 1)
  
  diag <- do.call(rbind, Map(data.frame, model = models, AIC = AIC, RMSE = RMSE))
  
  #order models by lowest AIC
  stats <- diag[order(diag$AIC),]
  
  return(stats)
  
}


#combine all output runs into 1 list
list_runs =lapply(ls(pattern="output"),get)

#list run names
list_names <- sapply(ls(pattern = "output"), as.name)

#Add run names to output list
names(list_runs) <- list_names

#apply "read_stats" function to all runs
list_stats <- lapply(list_runs, read_stats)


#Make table of all AIC & RMSE values for each run/model
#each = number of models

stats_table <- do.call(rbind, Map(data.frame, Run = rep(names(list_stats), each = 41),
                                  model = unlist(lapply(list_stats, '[[', 1)), 
                                  AIC = unlist(lapply(list_stats, '[[', 2)),
                                  RMSE = unlist(lapply(list_stats, '[[', 3))))



#Pull out best run for each model
best_runs <- stats_table %>% 
  group_by(model) %>% 
  slice(which.min(AIC))

#order by lowest AIC & save as dataframe
best_runs <- data.frame(best_runs[order(best_runs$AIC),])

#View data
data.frame(best_runs)

#Save as csv
write.csv(best_runs, paste0(mydir, "/best_runs.csv"))



##pull data for best run of each model, calculate residuals

extract_data <- function (data, results, model){
  
  #Extract data from model fit
  obs <- as.matrix(list_runs[[results]][[model]]$measured)
  pred <- as.matrix(list_runs[[results]][[model]]$predicted)
  diff <- obs-pred
  
  full_results <- cbind(obs, pred, diff)
  colnames(full_results) <- c("observed", "predicted", "diff")
  
  
  #Add site names & years
  All_trans_dates <-  do.call(rbind, Map(data.frame, location = lapply(data, '[', 1), 
                                         year = lapply(data, '[[', 7)))
  
  formatted_results <- cbind(All_trans_dates, full_results)
  
  return(formatted_results)
  
}


#test extract 1 model
test <- extract_data(All_sites, "output_run_15", "SQWs")


####Make list of best run for each model####

#function to extract the best run for each model
make_list <- function (run, model){
  
  #Extract data from model fit
  out <- list_runs[[run]][[model]]
  
  return(out)
}

test2 <- make_list("output_run_1", "SQW")


#list of best runs for each model
run_list <- best_runs[,1]
run_list

#make empty list
best_run_list <- list()


#loop through data
for (i in 1:length(run_list)){
  
  run = best_runs[i,1]
  model = best_runs[i,2]
  
  out <- make_list(run = run, model = model)
  
  best_run_list[[i]] <- out
  
}


#Add run names
names(best_run_list) <- best_runs[,2]

#rename and save best runs 
#repeat for All Sites and each KG group

AllSites_BestRuns <- best_run_list

saveRDS(AllSites_BestRuns, paste0(mydir, "/AllSites_BestRuns.RDS"))



####Repeat code above for SM model output (with AllSites)####
#then combine best runs of all models together for AllSites

#Read in SM best runs
SM_BestRuns <- readRDS("./output/AllSites_SM_BestRuns.RDS")
AllSites_BestRuns <- readRDS("./output/AllSites_BestRuns.RDS")

#combine best runs for original/precip models with SM models into 1 dataset
BestRuns_combo <- c(AllSites_BestRuns, SM_BestRuns)

#save datafile with best runs for all models (including SM) together
saveRDS(BestRuns_combo, "./output/AllSites_SM_combo_BestRuns.RDS")



####Extract residuals####

#Read in "BestRuns" files for All Sites and each KG group (created above)
#Repeat code for each

#All models (including SM models)
AllSites_combo_BestRuns <- readRDS("./output/AllSites_SM_combo_BestRuns.RDS")

#Separate KG groups
KG_B_BestRuns <- readRDS("./output/KG_B_BestRuns.RDS")
KG_Cf_BestRuns <- readRDS("./output/KG_Cf_BestRuns.RDS")
KG_Cs_BestRuns <- readRDS("./output/KG_Cs_BestRuns.RDS")
KG_Df_BestRuns <- readRDS("./output/KG_Df_BestRuns.RDS")


#designate dataset and model fits(one of "BestRuns" files loaded above)
BestRuns <- AllSites_combo_BestRuns
Dataset <- All_sites

#make list of model names
model_names <- names(BestRuns)
model_names

#get number of models
num_models <- length(model_names)
num_models

#Find # of site-years
num_data <- length(BestRuns$LIN$measured)
num_data


##New function to extract residuals##

extract_resid <- function (data, model){
  
  #Extract data from model fit
  obs <- as.matrix(BestRuns[[model]]$measured)
  pred <- as.matrix(BestRuns[[model]]$predicted)
  diff <- obs-pred
  
  #Add site names & years
  All_trans_dates <-  do.call(rbind, Map(data.frame, location = lapply(data, '[', 1), 
                                         year = lapply(data, '[[', 7)))
  
  formatted_results <- cbind(All_trans_dates, diff)
  
  return(formatted_results)
  
}


#test extract 1 model residuals
test <- extract_resid(Dataset, "SQWs")


####loop through to grab resid (diff) from the best run of each model####

# assign output matrix
#nrow = # of site-years
resid_table <- matrix(0, nrow = num_data, ncol = num_models)

#loop through data
for (i in 1:num_models){
  
  model = model_names[i]
  
  data <- extract_resid(Dataset, model)
  
  resid_table[,i] <- data$diff
  
}

#Add model names (columns)
colnames(resid_table) <- model_names

#Add site-year names (rows)
site_years <- paste0(test$site, "_", test$year)
rownames(resid_table) <- site_years

#Save as csv
write.csv(resid_table, "./output/resid_table_combo.csv")


###############################################################################

##For each model, combine best run for all KG groups

#Read in "BestRuns" files for All Sites and each KG group (made above)

AllSites_BestRuns <- readRDS("./output/AllSites_BestRuns.RDS")
KG_B_BestRuns <- readRDS("./output/KG_B_BestRuns.RDS")
KG_Cf_BestRuns <- readRDS("./output/KG_Cf_BestRuns.RDS")
KG_Cs_BestRuns <- readRDS("./output/KG_Cs_BestRuns.RDS")
KG_Df_BestRuns <- readRDS("./output/KG_Df_BestRuns.RDS")



###Make function to pull out results

extract_data_2 <- function (data, results){
  
  #Extract data from model fit
  obs <- as.matrix(results$measured)
  pred <- as.matrix(results$predicted)
  
  full_results <- cbind(obs, pred)
  colnames(full_results) <- c("observed", "predicted")
  
  #Add site names & years
  All_trans_dates <-  do.call(rbind, Map(data.frame, location = lapply(data, '[', 1), 
                                         year = lapply(data, '[[', 7)))
  
  formatted_results <- cbind(All_trans_dates, full_results)
  
  return(formatted_results)
  
}



#Adapt AIC function from phenor

AIC_pheno <- function(measured, predicted, k){
  
  # calculate number of observations
  n <- length(measured)
  
  # calculatue residual sum of squares
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


###Use function to calculate AIC & RMSE

#Run extract function for each region
#specify model

Results_1 <-  extract_data_2(KG_B_sites, KG_B_BestRuns$SQWs_Tmin)

Results_2 <-  extract_data_2(KG_Cf_sites, KG_Cf_BestRuns$SQWs_Tmin)

Results_3 <-  extract_data_2(KG_Cs_sites,KG_Cs_BestRuns$SQWs_Tmin)

Results_4 <-  extract_data_2(KG_Df_sites, KG_Df_BestRuns$SQWs_Tmin)

Results_All <-  extract_data_2(All_sites, AllSites_BestRuns$SQWs_Tmin)


#combine KG group results
full <- rbind(Results_1, Results_2, Results_3, Results_4)

#Calculate AIC of entire model run (results from all regions combined)
#specify number of parameters
AIC_pheno(full$observed, full$predicted, k = 7)

#Calculate RMSE of entire model run
rmse(full$observed, full$predicted)



####Add KG groups to results & graph

###Create metadata of KG groups
#Import site classifications
metadata <- read.csv("./grassland_sites.CSV")

#Select relevant columns
metadata2 <- data.frame(metadata[,1], metadata[,12])

#Rename columns
colnames(metadata2) <- c("site", "KG_class")

#Remove duplicate entries
metadata3 <- distinct(metadata2)

#pull out first 2 KG letters
KG_div <- as.matrix(substr(metadata3$KG_class, start = 1, stop = 2))
colnames(KG_div) <- "KG_class2"
metadata4 <- cbind(metadata3, KG_div)

#combine B groups
metadata4$KG_class2[metadata4$KG_class2 == "BS"] <- "B"
metadata4$KG_class2[metadata4$KG_class2 == "BW"] <- "B"


###merge results with KG groupings
results_KG <- merge(full, metadata4, by = "site")
results_AllSites <- merge(Results_All, metadata4, by = "site")


###Graph results all together
#Note this is a Model I regression (should use Model II for final figure)

KG_graph <- ggplot(data = results_KG, mapping = aes(x = observed, y = predicted ))+
  geom_point(aes(col = KG_class2))+
  geom_smooth(method= "lm", se = FALSE, fullrange = F, col = "black")+
  geom_abline(intercept = 0, slope = 1, col = "dark grey")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250))+
  scale_x_continuous(expand = c(0,0))+
  theme(aspect.ratio=1)+
  stat_cor(aes(label =..rr.label..), size = 8)+
  geom_hline(yintercept=0) + labs(title="SQWs_Tmin", y="Predicted (DOY)", x = "Observed (DOY)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title = element_text(size=28, face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=16))+
  theme(axis.title.x = element_text(margin=margin(t=15)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r=15))) #add margin to y-axis title

KG_graph + theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), axis.line = element_line(colour = "black"))+
  labs(color='K-G Climate Zone')+
  scale_color_manual(labels = c("B: Arid", "Cf: Temperate" , "Cs: Mediterranean", "Df: Continental"), 
                     values = c("red2", "blue2", "goldenrod3", "green3"))+
  theme(axis.ticks.length=unit(-0.2, "cm"))


