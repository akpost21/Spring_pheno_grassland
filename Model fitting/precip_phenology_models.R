#Run new models with precip

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


#upload precip models
source("./Precip_model_functions.R")

#upload parameter file
params_file <- "./parameter_ranges_paper.csv"

####Test single model####

test_model = pr_fit(
  data = All_sites,
  model = "SQW",
  method = "GenSA",
  control = list(max.call = 100),
  par_ranges = file.path(params_file)
)


#See observed vs predicted of single model (combined data)
plot(test_model$predicted, test_model$measured, ylim = c(0,240), ylab = "Measured", xlab = "Predicted")
abline(lm(test_model$measured ~ test_model$predicted), col = "red") #regression line



####Run multiple models together####

#include names of all models you want to test in this list
models = c("LIN", "TT","TTs", "PTT", "PTTs", "M1", "M1s", "AT", "SQ", "SQb", "SM1", 
           "SM1b","PA", "PAb", "PM1", "PM1b", "UM1", "SGSI", "AGSI", "GRP",
           "M1W", "M1Ws", "PWT", "PWTs", "WT", "WTs", "PAW", "PAWs", "SQW", "SQWs", "SQWr", "SQWrs")


#specify dataset
data_site = All_sites

##Fit models
#max.call = # of iterations
#specify parameter file

pr_fit_new <- function(model){
  Fit <- pr_fit(model = model,
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


#####Assign location to each value & graph####

#Make list of all observed transition dates
All_trans_dates <-  do.call(rbind, Map(data.frame, site = lapply(data_site, '[', 1), 
                                     observed = lapply(data_site, '[[', 5), 
                                      year = lapply(data_site, '[[', 7)))

#Make data frame of all observed and predicted dates
#each = # of site-years of data

results <- do.call(rbind, Map(data.frame, model = rep(unlist(lapply(model_fits, '[', 1)), each = length(All_trans_dates$year)),
                              predicted = unlist(lapply(model_fits, '[[', 4)), 
                              observed = unlist(lapply(model_fits, '[[', 3))))


##Use automatic alphabetical order to add location names to results data frame

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
  geom_abline(intercept = 0, slope = 1, col = "dark grey")+
  facet_wrap(facets = vars(model), scales = "free") +
  theme(aspect.ratio=1)+
  stat_cor(aes(label =..rr.label..))
All + labs(title="Model Fits", y="Predicted", x = "Observed")

#Make graphs interactive
ggplotly(All)


###Optional to save results: 

#save model fits
saveRDS(model_fits, file = "./output/Allsites_precip_model_results.rds")

#Save results table
write.csv(results2,"./output/AllSites_precip_models_table.csv", row.names = FALSE)

