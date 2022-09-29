###Write my own code to make plots (edited from Koen's)
#Use "model_fits" data format

#load packages
library(phenor)

#Upload datasets
setwd("G:/My Drive/Project/Model edits/R files/Published code/Model fitting")

All_sites <- readRDS("./input/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_B_sites <- readRDS("./input/KG_B_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cf_sites <- readRDS("./input/KG_Cf_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cs_sites <- readRDS("./input/KG_Cs_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Df_sites <- readRDS("./input/KG_Df_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")

#Read in model fits
AllSites_BestRuns <- readRDS("G:/My Drive/Project/Model edits/R files/Monsoon/output/AllSites_output_Feb4_100000/AllSites_BestRuns.RDS")


#This code works for the "model_fits" output, as shown below
#You can also read in a model fit you've already saved as a ".rds" file


#Example model fits

#upload parameter file
params_file <- "./parameter_ranges_paper.csv"

#upload precip models
source("./Precip_model_functions.R")

#Add models you want to fit to the data here (can be more than 2)
models = c("SQW", "SQWs")

#Input dataset
data_site = All_sites

#fit models
pr_fit_new <- function(model){
  Fit <- pr_fit(model = model,
                data = data_site,
                method = "GenSA",
                control = list(max.call = 1000),
                par_ranges = file.path(params_file)) 
               
  return(Fit)
}


model_fits <- mapply(pr_fit_new, model= models, SIMPLIFY = FALSE)



#data = "model_fits" output format
#lwd = change thickness of arrows
#length = change size of arrow head


pr_plot_arrows_new = function(
  data,
  models = NULL,
  lwd = 2,
  length = 0.05
){
  
    # sanity check, as no defaults are provided
    if (missing(data)){
      stop("no input data provided")
    }
    
    # check if there are models to compare
    if(length(names(data)) < 2){
      stop("only one model found in the data file, check your input")
    }
    
    if (is.null(models)){
      # grab the site names, only select the first two
      # for comparison (by default, and if available)
      cat("no models specified for comparison, first two are selected \n")
      model_names = names(data)[1:2]
      model_col = c(1,2)
    } else {
      if (length(models)!=2){
        stop("you can only plot the comparison of two models at a time")
      }
      model_col= c(match(models[1],names(data)),
                   match(models[2],names(data)))
      if ( NA %in% model_col){
        stop("the specified models are not in the provided dataset")
      }
      model_names = models
    }
    
  
  #pull out measured from 1st model (all the same)
  measured <- unlist(lapply(data, '[[', 3)[1])
  
  #Pull our predicted values
  predicted_values <- t(do.call("rbind", lapply(data, '[[', 4)))[,model_col]
  
  # calculate locations which do not change
  loc = which(apply(predicted_values,1,diff) == 0)
  
  # calculate ylim ranges, provide some padding to let the
  # plot breath a bit
  max_pred = max(apply(predicted_values,1,max)) + 10
  min_pred = min(apply(predicted_values,1,min)) - 10
  
  # provide the baseline plot (don't plot values)
  graphics::plot(measured,
                 predicted_values[,1],
                 main = sprintf("Directional change from model: %s to %s",
                                model_names[1],model_names[2]),
                 type = "n",
                 ylab = "Predicted values (DOY)",
                 xlab = "Measured values (DOY)",
                 ylim = c(min_pred, max_pred),
                 tck = 0.02)
  
  # plot a 1:1 line
  graphics::abline(0,1,lty = 2)
  
  # calculate the colours to assign to arrows
  # rising arrows are blue, falling arrows are red
  col = apply(predicted_values,1,diff)
  col = ifelse(col >= 0,
               "#167fdb",
               "#e8301c")
 
  
  # set unchanged points colours to transparent
  col[loc] = grDevices::rgb(0,0,0,0)
  
  # plot points which haven't changed
  graphics::points(measured[loc],
                   predicted_values[loc,1],
                   col = "#0ad13f",
                   cex = 0.8,
                   pch = 16)
  
  # plot the arrows with the correct colors
  # limit the length of the arrow head
  # suppress warnings on arrows of zero length
  suppressWarnings(
    graphics::arrows(
      x0 = measured,
      y0 = predicted_values[,1],
      x1 = measured,
      y1 = predicted_values[,2],
      length = length,
      col = col,
      lwd = lwd)
  )

}


####Test new function
#List the 2 models you want to compare
#if no models listed, will default to first 2 models

pr_plot_arrows_new(data = AllSites_BestRuns, models = c("SQW", "SQWs"))



