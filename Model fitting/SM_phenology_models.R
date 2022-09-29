#Run new models with soil moisture (SM)

#load packages
library(phenor)
library(ggplot2)
library(ggpubr)
library(beepr)
library(plotly)


#Upload datasets
setwd("G:/My Drive/Project/Model edits/R files/Published code/Model fitting")

All_sites <- readRDS("./input/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_B_sites <- readRDS("./input/KG_B_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cf_sites <- readRDS("./input/KG_Cf_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cs_sites <- readRDS("./input/KG_Cs_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Df_sites <- readRDS("./input/KG_Df_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")

#Need to edit some functions in phenor for models to work with SM
#Read in edited functions (pr_flatten_SM, pr_fit_SM): 
source("./pr_flatten_SM.R")
source("./pr_fit_SM.R")

#upload SM models
source("./SM_model_functions.R")

#upload parameter file
params_file <- "./parameter_ranges_paper.csv"


####Test single model####

test_model = pr_fit_SM(
  data = All_sites,
  model = "SQWs_SM",
  method = "GenSA",
  control = list(max.call = 100),
)

#See model parameters
test_model$par

#See observed vs predicted of single model (combined data)
plot(test_model$measured, test_model$predicted, ylim = c(0,300), ylab = "Predicted", xlab = "Observed")
abline(lm(test_model$measured ~ test_model$predicted), col = "red") #regression line



####Run multiple models together####


#include names of all models you want to test in this list
models = c("M1W_SM", "M1Ws_SM", "PAW_SM", "PAWs_SM","PWT_SM", "PWTs_SM", "SQW_SM", 
           "SQWs_SM", "SQWr_SM","SQWrs_SM", "WT_SM", "WTs_SM")


#specify dataset
data_site = All_sites

##Fit models
#max.call = # of iterations
#specify parameter file

pr_fit_new <- function(model){
  Fit <- pr_fit_SM(model = model,
                data = data_site,
                method = "GenSA",
                control = list(max.call = 100000), 
                par_ranges = file.path(params_file))
  return(Fit)
}


model_fits <- mapply(pr_fit_new, model= models, SIMPLIFY = FALSE)
beep()


####Extract AIC & RMSE####

#Extract elements (rmse = 5, AIC = 7) from nested lists ([[ = 2nd level list)
RMSE <- lapply(model_fits, '[[', 5)
AIC_list <- lapply(model_fits, '[[', 7)
AIC <- lapply(AIC_list, '[[', 1)

diag <- do.call(rbind, Map(data.frame, AIC = AIC, RMSE = RMSE))


#order models by lowest AIC
diag[order(diag$AIC),]



####Assign location to each value & graph####

All_trans_dates <-  do.call(rbind, Map(data.frame, location = lapply(data_site, '[', 1), 
                                       observed = lapply(data_site, '[[', 5), 
                                       year = lapply(data_site, '[[', 7)))

#Make data frame of all observed vs predicted
#each = # of data years (data points)

results <- do.call(rbind, Map(data.frame, model = rep(unlist(lapply(model_fits, '[', 1)), each = length(All_trans_dates$year)),
                              predicted = unlist(lapply(model_fits, '[[', 4)), 
                              observed = unlist(lapply(model_fits, '[[', 3))))


##Use automatic alphabetical order to add location names to results df

#number of sites
s <- data.frame(All_trans_dates$site, All_trans_dates$year)
#number of models
n <- length(diag$AIC)
#Replicate site names based on number of models
Site_names <- do.call("rbind", replicate(n, s, simplify = FALSE))


#Combine site names with results data frame, rename columns
results2 <- cbind(results, Site_names)
names(results2)[names(results2) == 'All_trans_dates.site'] <- 'site'
names(results2)[names(results2) == 'All_trans_dates.year'] <- 'year'

##See all model fits as regressions
All <- ggplot(data = results2, mapping = aes(x = observed, y = predicted ))+
  geom_point(aes(col = site))+
  geom_smooth(method= "lm", se = FALSE, fullrange = F, col = "black")+
  facet_wrap(facets = vars(model)) + coord_cartesian(ylim = c(0,250))+
  theme(aspect.ratio=1)+
  stat_cor(aes(label = ..rr.label..))
All + geom_hline(yintercept=0) + labs(title="Model Fits", y="Predicted", x = "Observed")

#Make graphs interactive
ggplotly(All)


###Optional to save results:

# Save model fits
saveRDS(model_fits, file = "./output/AllSites_SM_model_results.RDS")

#Save results table
write.csv(results2,"./output/AllSites_SM_models_table.csv", row.names = FALSE)

