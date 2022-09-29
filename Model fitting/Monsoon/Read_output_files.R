#read and combine all output files from 25 monsoon runs
#graph results

#load packages
library(dplyr)
library(ggpubr)
library(plotly)
library(tidyr)
library(ggplot2)
library(tidyverse)


##Read in all output files
mydir <- "G:/My Drive/Project/Model edits/R files/Monsoon/output/AllSites_output_Feb4_100000/"
myfiles <- list.files(path=mydir, pattern="*.RDS", full.names=TRUE)
myfiles

#read in all files
#Add "SM_" in front of "output_run_" for SM model output files

list <- c(1:25)

for (i in list) {
  filename <- paste0("output_run_", i)
  wd <- paste0(mydir, "output_run_", i, ".RDS")
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

#test function on 1 run
read_stats(output_run_1)


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

#save as table
write.csv(stats_table, paste0(mydir, "/stats_table.csv") , row.names = FALSE)


#Determine RMSE range for each model
min_RMSE <- stats_table %>%
  group_by(model) %>%
  summarise(
    min = min(RMSE),
    max = max(RMSE), 
    diff = max-min
  ) %>%
  arrange(model)

#Determine AIC range for each model
min_AIC<- stats_table %>%
  group_by(model) %>%
  summarise(
    min = min(AIC),
    max = max(AIC), 
    diff = max-min
  ) %>%
  arrange(model)

#save as tables
write.csv(min_RMSE, paste0(mydir, "/min_RMSE.csv") , row.names = FALSE)
write.csv(min_AIC, paste0(mydir, "/min_AIC.csv") , row.names = FALSE)


###Make into wide format

#Remove RMSE
stats_table_AIC <- stats_table
stats_table_AIC$RMSE <- NULL

#Remove AIC
stats_table_RMSE <- stats_table
stats_table_RMSE$AIC <- NULL

#convert both to wide format
stats_table_AIC_wide <- spread(stats_table_AIC, Run, AIC)
stats_table_RMSE_wide <- spread(stats_table_RMSE, Run, RMSE)

#save tables
write.csv(stats_table_AIC_wide, paste0(mydir, "/stats_table_AIC.csv") , row.names = FALSE)
write.csv(stats_table_RMSE_wide, paste0(mydir, "/stats_table_RMSE.csv") , row.names = FALSE)


###Plot density graph of results

#Density plot of RMSE for each model
#ordered by lowest-highest RMSE median value
stats_table %>%
  mutate(model = fct_reorder(model, RMSE, .fun='median')) %>%
  ggplot(aes(x=reorder(model, RMSE), y=RMSE, color = model)) +
  geom_point(position = position_dodge(width = 0.4))+
  xlab("Model") +
  theme(legend.position="none")+
  coord_flip()

#Density plot of AIC for each model
#ordered by lowest-highest AIC median value
stats_table %>%
  mutate(model = fct_reorder(model, AIC, .fun='median')) %>%
  ggplot(aes(x=reorder(model, AIC), y=AIC, color = model)) +
  geom_point(position = position_dodge(width = 0.4))+
  xlab("Model") +
  theme(legend.position="none")+
  coord_flip()


#Boxplot of RMSE for each model
#ordered by lowest-highest RMSE median value
stats_table %>%
  mutate(model = fct_reorder(model, RMSE, .fun='median')) %>%
  ggplot( aes(x=reorder(model, RMSE), y=RMSE, fill=model)) + 
  geom_boxplot() +
  xlab("model") +
  theme(legend.position="none")+
  coord_flip()



##Plot multiple histograms at once

#remove LIN model (no variation)
stats_table_NoLIN <- subset(stats_table, model != "LIN")


#plot histograms with models as facets
stats_table_NoLIN %>%
  mutate(model = fct_reorder(model, RMSE, .fun='min')) %>%
  ggplot(aes(x= RMSE)) +
  geom_histogram(bins = 5) + 
  facet_wrap(~ model, scales = "free")


#Graph all histograms with separate bins for each integer
#x-axis shows min RMSE + 5 (so larger values missed) for each model
breaks_fun <- function(x) {
  seq(floor(min(x)), floor(min(x) + 5), 1)
}

limits_fun <- function(x) {
  c((min(x)), min(x) + 5)
}

stats_table %>%
  mutate(model = fct_reorder(model, RMSE, .fun='min')) %>%
  ggplot(aes(x= RMSE)) +
  geom_histogram(binwidth = 1, center = 0) + 
  facet_wrap(~ model, scales = "free")+
  scale_x_continuous(breaks = breaks_fun, limits = limits_fun)+
  ylab("# Runs") +
  xlab("RMSE (days)")

