#Create figures used in paper

#load packages
library(ggplot2)
library(ggpubr)
library(dplyr)
library(egg)
library(pals) #for color palette
library(lmodel2)
library(broom)


#Upload datasets
setwd("G:/My Drive/Project/Model edits/R files/Published code/Model fitting")

All_sites <- readRDS("./input/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_B_sites <- readRDS("./input/KG_B_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cf_sites <- readRDS("./input/KG_Cf_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Cs_sites <- readRDS("./input/KG_Cs_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")
KG_Df_sites <- readRDS("./input/KG_Df_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")

#Upload results from Monsoon runs

AllSites_BestRuns <- readRDS("./output/AllSites_BestRuns.RDS")
KG_B_BestRuns <- readRDS("./output/KG_B_BestRuns.RDS")
KG_Cf_BestRuns <- readRDS("./output/KG_Cf_BestRuns.RDS")
KG_Cs_BestRuns <- readRDS("./output/KG_Cs_BestRuns.RDS")
KG_Df_BestRuns <- readRDS("./output/KG_Df_BestRuns.RDS")



###Make function to extract data

extract_data <- function (data, results){
  
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

Results_1 <-  extract_data(KG_B_sites, KG_B_BestRuns$SQWs_Tmin)

Results_2 <-  extract_data(KG_Cf_sites, KG_Cf_BestRuns$SQWs_Tmin)

Results_3 <-  extract_data(KG_Cs_sites,KG_Cs_BestRuns$SQWs_Tmin)

Results_4 <-  extract_data(KG_Df_sites, KG_Df_BestRuns$SQWs_Tmin)

#combine results
full <- rbind(Results_1, Results_2, Results_3, Results_4)

#Extract function from All Sites

AllSites <-  extract_data(All_sites, AllSites_BestRuns$SQWs_Tmin)

All_TT <- extract_data(All_sites, AllSites_BestRuns$TT)
All_SQ <- extract_data(All_sites, AllSites_BestRuns$SQ)
All_AGSI <- extract_data(All_sites, AllSites_BestRuns$AGSI)
All_GRP <- extract_data(All_sites, AllSites_BestRuns$GRP)


####Add grouping to results & graph

###Create metadata of KG groups
#Import site classifications
metadata <- read.csv("./grassland_sites.CSV")

#Select relevant columns
metadata2 <- data.frame(metadata[,1], metadata[,12])

#Rename columns
colnames(metadata2) <- c("site", "KG_class")

#Remove duplicate entries
metadata3 <- distinct(metadata2)

#first 2 letters
KG_div <- as.matrix(substr(metadata3$KG_class, start = 1, stop = 2))
colnames(KG_div) <- "KG_class2"
metadata4 <- cbind(metadata3, KG_div)

#combine B groups
metadata4$KG_class2[metadata4$KG_class2 == "BS"] <- "B"
metadata4$KG_class2[metadata4$KG_class2 == "BW"] <- "B"


###merge results with KG groupings
results_KG <- merge(full, metadata4, by = "site")
results_AllSites <- merge(AllSites, metadata4, by = "site")


###Graph results all together (with formatting)
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



###########Graph best fit model##########

#Model II regressions


#info about different model II regressions
#vignette('mod2user')

#fit model II regression to combined results data
fit <- lmodel2(predicted ~ observed, results_AllSites)
summary(fit)

#see summary of results
glance(fit)

###plot single model
#Get Coefficients - only want the results of the regressions saved as reg
reg <- fit$regression.results

# Rename columns in reg so they're easy to use
names(reg) <- c("method", "intercept", "slope", "angle", "p-value")

# Check that the regressions look like so we know what will be plots
print(reg)

#pull out just 1 of the regression fits (MA)
reg2 <- reg[2,]

#pull out R2 value
R2 <- round(fit$rsquare, digits = 2)
R2

R2_text = expression(R^2 == 0.85)


annotate(geom = "text", x = 30, y = 3,
         label = "Some text\nSome more text",
         hjust = 0)

###Graph results all together with Model II regression
All <- ggplot()+
  geom_point(data = results_AllSites, aes(x = observed, y = predicted, col = KG_class2))+
  geom_abline(data = reg2, size = 1,linetype = 1, aes(intercept = intercept, slope = slope))+
  geom_abline(intercept = 0, slope = 1, col = "dark grey", linetype = 2)+
  annotate(geom="text", x=25, y=230, label = R2_text, size = 6.5, hjust = 0)+
  annotate(geom="text", x=25, y=210, label = "RMSE = 16.0", size = 6.5, hjust = 0)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250))+
  scale_x_continuous(expand = c(0,0), limits = c(0,250))+ #Make origin at corner
  theme(aspect.ratio=1)+
  labs(y="Predicted (DOY)", x = "Observed (DOY)")+
  theme(axis.text=element_text(size=16,  colour = "black"), axis.title=element_text(size=20),
        legend.title=element_text(size=14), legend.text=element_text(size=14))+
  theme(legend.position = c(0.76, 0.19))+ 
  theme(legend.key=element_blank())+ #remove grey boxes around legend symbols
  theme(axis.title.x = element_text(margin=margin(t=10)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r=10)))+ #add margin to y-axis title
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), axis.line = element_line(colour = "black"))+
  labs(color='Climate Zone')+
  scale_color_manual(labels = c("B:  Arid", "Cf: Temperate" , "Cs: Mediterranean", "Df: Continental"), 
                     values = c("red2", "blue2", "goldenrod3", "green3"))+
  theme(axis.ticks.length=unit(-0.2, "cm"))

All


##Repeat for KG groups

#fit model II regression to combined results data
fit2 <- lmodel2(predicted ~ observed, results_KG)
summary(fit)

#see summary of results
glance(fit)

###plot single model
#Get Coefficients - only want the results of the regressions saved as reg
reg2 <- fit2$regression.results

# Rename columns in reg so they're easy to use
names(reg2) <- c("method", "intercept", "slope", "angle", "p-value")

# Check that the regressions look like so we know what will be plots
print(reg2)

#pull out just 1 of the regression fits (MA)
reg2 <- reg2[2,]

#pull out R2 value
R2_2 <- round(fit2$rsquare, digits = 2)
R2_2

R2_2_text = expression(R^2 == 0.93)

###Graph results all together with Model II regression
KG_graph <- ggplot()+
  geom_point(data = results_KG, aes(x = observed, y = predicted, col = KG_class2))+
  geom_abline(data = reg2, size = 1,linetype = 1, aes(intercept = intercept, slope = slope))+
  geom_abline(intercept = 0, slope = 1, col = "dark grey", linetype = 2)+
  annotate(geom="text", x=25, y=230, label= R2_2_text, size = 6.5, hjust = 0)+
  annotate(geom="text", x=25, y=210, label = "RMSE = 10.4", size = 6.5, hjust = 0)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250))+
  scale_x_continuous(expand = c(0,0), limits = c(0, 250))+ #Make origin at corner
  theme(aspect.ratio=1)+
  labs(x = "Observed (DOY)")+ 
  ylab(NULL)+ theme(axis.text.y = element_blank())+
  theme(axis.text=element_text(size=16, colour = "black"), axis.title=element_text(size=20))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(margin=margin(t=10)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r=10)))+ #add margin to y-axis title
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), axis.line = element_line(colour = "black"))+
  labs(color='K-G Climate Zone')+
  scale_color_manual(labels = c("B:Arid", "Cf: Temperate" , "Cs: Mediterranean", "Df: Continental"), 
                     values = c("red2", "blue2", "goldenrod3", "green3"))+
  theme(axis.ticks.length=unit(-0.2, "cm"))+
  theme(plot.margin = margin(10, 50, 10, 30))#avoid x-axis getting cut off with panels

KG_graph

#Save to computer as png
png("G:/My Drive/Project/Model edits/R files/Published code/Data visualization/output/SQWs_Tmin_regressions.png", width = 11.5, height = 8, units = 'in', res = 300)

#Use "egg" package to arrange graphs into panels
ggarrange(All, KG_graph, nrow=1)

dev.off()




#########Graph original models############

results_All_TT <- merge(All_TT, metadata4, by = "site")
results_All_SQ <- merge(All_SQ, metadata4, by = "site")
results_All_AGSI <- merge(All_AGSI, metadata4, by = "site")
results_All_GRP <- merge(All_GRP, metadata4, by = "site")

#fit model II regression to combined results data
fit_TT <- lmodel2(predicted ~ observed, results_All_TT)
fit_SQ <- lmodel2(predicted ~ observed, results_All_SQ)
fit_AGSI <- lmodel2(predicted ~ observed, results_All_AGSI)
fit_GRP <- lmodel2(predicted ~ observed, results_All_GRP)

###plot single model
#Get Coefficients - only want the results of the regressions saved as reg
reg_TT <- fit_TT$regression.results
reg_SQ <- fit_SQ$regression.results
reg_AGSI <- fit_AGSI$regression.results
reg_GRP <- fit_GRP$regression.results

# Rename columns in reg so they're easy to use
names(reg_TT) <- c("method", "intercept", "slope", "angle", "p-value")
names(reg_SQ) <- c("method", "intercept", "slope", "angle", "p-value")
names(reg_AGSI) <- c("method", "intercept", "slope", "angle", "p-value")
names(reg_GRP) <- c("method", "intercept", "slope", "angle", "p-value")

#pull out just 1 of the regression fits (MA)
reg_TT2 <- reg_TT[2,]
reg_SQ2 <- reg_SQ[2,]
reg_AGSI2 <- reg_AGSI[2,]
reg_GRP2 <- reg_GRP[2,]

#R2 text
R2_TT = expression(R^2 == 0.18)
R2_SQ= expression(R^2 == 0.18)
R2_AGSI = expression(R^2 == 0.53)
R2_GRP = expression(R^2 == 0.17)


###Graph results all together with Model II regression

TT_graph <- ggplot()+
  geom_point(data = results_All_TT, aes(x = observed, y = predicted, col = KG_class2))+
  geom_abline(data = reg_TT2, size = 1,linetype = 1, aes(intercept = intercept, slope = slope))+
  geom_abline(intercept = 0, slope = 1, col = "dark grey", linetype = 2)+
  annotate(geom="text", x=25, y=230, label = R2_TT, size = 5, hjust = 0)+
  annotate(geom="text", x=25, y=210, label = "RMSE = 35.3", size = 5, hjust = 0)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250))+
  scale_x_continuous(expand = c(0,0), limits = c(0,250))+ #Make origin at corner
  theme(aspect.ratio=1)+
  labs(title="TT", y="Predicted (DOY)")+
  xlab(NULL)+ theme(axis.text.x = element_blank())+
  theme(axis.text=element_text(size=14,  colour = "black"), axis.title=element_text(size=16),
        plot.title = element_text(size=20, face="bold"),
        legend.title=element_text(size=10.5), legend.text=element_text(size=10.5),
        legend.key.size = unit(0.1, "cm"))+ #change spacing of legend rows
  theme(legend.position = c(0.74, 0.22))+ 
  theme(legend.key=element_blank())+ #remove grey boxes around legend symbols
  theme(axis.title.x = element_text(margin=margin(t=10)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r=10)))+ #add margin to y-axis title
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), axis.line = element_line(colour = "black"))+
  labs(color='Climate Zone')+
  scale_color_manual(labels = c("B:  Arid", "Cf: Temperate" , "Cs: Mediterranean", "Df: Continental"), 
                     values = c("red2", "blue2", "goldenrod3", "green3"))+
  theme(axis.ticks.length=unit(-0.2, "cm"))

TT_graph



SQ_graph <- ggplot()+
  geom_point(data = results_All_SQ, aes(x = observed, y = predicted, col = KG_class2))+
  geom_abline(data = reg_SQ2, size = 1,linetype = 1, aes(intercept = intercept, slope = slope))+
  geom_abline(intercept = 0, slope = 1, col = "dark grey", linetype = 2)+
  annotate(geom="text", x=25, y=230, label = R2_TT, size = 5, hjust = 0)+
  annotate(geom="text", x=25, y=210, label = "RMSE = 35.3", size = 5, hjust = 0)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250))+
  scale_x_continuous(expand = c(0,0), limits = c(0,250))+ #Make origin at corner
  theme(aspect.ratio=1)+
  labs(title="SQ")+
  xlab(NULL)+ theme(axis.text.x = element_blank())+
  ylab(NULL)+ theme(axis.text.y = element_blank())+
  theme(axis.text=element_text(size=14,  colour = "black"), axis.title=element_text(size=16),
        plot.title = element_text(size=20, face="bold"))+
  theme(legend.position = "none")+ 
  theme(legend.key=element_blank())+ #remove grey boxes around legend symbols
  theme(axis.title.x = element_text(margin=margin(t=10)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r=10)))+ #add margin to y-axis title
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), axis.line = element_line(colour = "black"))+
  labs(color='Climate Zone')+
  scale_color_manual(labels = c("B:  Arid", "Cf: Temperate" , "Cs: Mediterranean", "Df: Continental"), 
                     values = c("red2", "blue2", "goldenrod3", "green3"))+
  theme(axis.ticks.length=unit(-0.2, "cm"))

SQ_graph



AGSI_graph <- ggplot()+
  geom_point(data = results_All_AGSI, aes(x = observed, y = predicted, col = KG_class2))+
  geom_abline(data = reg_AGSI2, size = 1,linetype = 1, aes(intercept = intercept, slope = slope))+
  geom_abline(intercept = 0, slope = 1, col = "dark grey", linetype = 2)+
  annotate(geom="text", x=25, y=230, label= R2_AGSI, size = 5, hjust = 0)+
  annotate(geom="text", x=25, y=210, label = "RMSE = 26.9", size = 5, hjust = 0)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250))+
  scale_x_continuous(expand = c(0,0), limits = c(0, 250))+ #Make origin at corner
  theme(aspect.ratio=1)+
  labs(title="AGSI", x = "Observed (DOY)")+ 
  ylab(NULL)+ theme(axis.text.y = element_blank())+
  theme(axis.text=element_text(size=14, colour = "black"), axis.title=element_text(size=16),
        plot.title = element_text(size=20, face="bold"))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(margin=margin(t=10)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r=10)))+ #add margin to y-axis title
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), axis.line = element_line(colour = "black"))+
  labs(color='K-G Climate Zone')+
  scale_color_manual(labels = c("B:Arid", "Cf: Temperate" , "Cs: Mediterranean", "Df: Continental"), 
                     values = c("red2", "blue2", "goldenrod3", "green3"))+
  theme(axis.ticks.length=unit(-0.2, "cm"))+
  theme(plot.margin = margin(10, 50, 10, 20))#avoid x-axis getting cut off with panels

AGSI_graph


GRP_graph <- ggplot()+
  geom_point(data = results_All_GRP, aes(x = observed, y = predicted, col = KG_class2))+
  geom_abline(data = reg_GRP2, size = 1,linetype = 1, aes(intercept = intercept, slope = slope))+
  geom_abline(intercept = 0, slope = 1, col = "dark grey", linetype = 2)+
  annotate(geom="text", x=25, y=230, label= R2_GRP, size = 5, hjust = 0)+
  annotate(geom="text", x=25, y=210, label = "RMSE = 40.3", size = 5, hjust = 0)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250))+
  scale_x_continuous(expand = c(0,0), limits = c(0, 250))+ #Make origin at corner
  theme(aspect.ratio=1)+
  labs(title="GRP", y = "Predicted (DOY)", x = "Observed (DOY)")+ 
  theme(axis.text=element_text(size=14, colour = "black"), axis.title=element_text(size=16),
        plot.title = element_text(size=20, face="bold"))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(margin=margin(t=10)), #add margin to x-axis title
        axis.title.y = element_text(margin=margin(r=10)))+ #add margin to y-axis title
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA), axis.line = element_line(colour = "black"))+
  labs(color='K-G Climate Zone')+
  scale_color_manual(labels = c("B:Arid", "Cf: Temperate" , "Cs: Mediterranean", "Df: Continental"), 
                     values = c("red2", "blue2", "goldenrod3", "green3"))+
  theme(axis.ticks.length=unit(-0.2, "cm"))+
  theme(plot.margin = margin(10, 10, 10, 20))#avoid x-axis getting cut off with panels

GRP_graph


#Save to computer as png
png("G:/My Drive/Project/Model edits/R files/Published code/Data visualization/output/Original_regressions.png", width = 11.5, height = 8, units = 'in', res = 300)

#4 panels
ggarrange(TT_graph, SQ_graph, GRP_graph, AGSI_graph, nrow = 2)

dev.off()



##########precip response curves##############

curves <- read.csv("G:/My Drive/Project/Model edits/R files/Published code/Data visualization/SQWs_Tmin_precip_curves.csv")

#set device to save png as file (specify file path)
#comment out to see graphs in RStudio viewer
png("G:/My Drive/Project/Model edits/R files/Published code/Data visualization/output/accum_graphs.png", width = 9.5, height = 8, units = 'in', res = 300)

#Set up panels
par(mfrow=c(3,1))

#Set margins
par(mar=c(4,20,3,20))
    
#plot
matplot(curves[,1],curves[,2:6], lty = 1, lwd = 3, type = "l",
        xlim = c(0,20), ylim = c(0, 1.1), xaxs = "i", yaxs = "i",
        cex.lab=2, cex.axis=1.5, tck = 0.02, las = 1,
        col = c("darkgrey","red2", "blue2", "goldenrod3", "green3"), 
        xlab = "" , ylab = "")
        title(xlab = "Rain event size (mm)", cex.lab= 1.8)
        title(ylab = "Event Weight", line = 4.5, cex.lab = 1.8)
        mtext("a", side = 3, line = 0.5, adj = 0, cex = 2)

#Add legend
legend(x = 11.2, y = 0.8, cex = 1.3, bty="n",lty = 1,lwd = 3,
       legend = c("All Sites", "B: Arid", "Cf: Temperate", "Cs: Mediterranean", "Df: Continental"),
       col = c("darkgrey", "red2", "blue2", "goldenrod3", "green3"))


###########accumulation curves#############

#Extract data from a single site

data_site <- "uiefprairie"

DOY <- data.frame(All_sites[[data_site]]$doy)
colnames(DOY) <- "DOY"

data_Ti <- All_sites[[data_site]]$Ti
colnames(data_Ti) <-  All_sites[[data_site]]$year

data_Tmin <- All_sites[[data_site]]$Tmini

data_Pi <- All_sites[[data_site]]$Pi
colnames(data_Pi) <- All_sites[[data_site]]$year


##SQWs_Tmin##

#get parameters
par <- AllSites_BestRuns$SQWs_Tmin$par

#Parameters
t0 <- round(par[1])
T_base <- par[2]
b <- par[3]
c <- par[4]
F_crit <- par[5]
P_req <- par[6]
T_thres <- par[7]

#Calculate Rw - sigmoid precip response
Rw <- 1 / (1 + exp(-b * (data_Pi - c)))
Rw[1:t0,] <- 0

Sw <- apply(Rw, 2, cumsum)

# precip requirement has to be met before
# temp accumulation starts (binary choice)
k <- as.numeric(Sw >= P_req)

# forcing
Rf <- data_Ti - T_base
Rf[Rf < 0] = 0
Rf <- Rf * k
Rf[1:t0,] <- 0

#change matrices to dataframes
Rf_df <- data.frame(Rf)
colnames(Rf_df) <- paste0(All_sites[[data_site]]$site, "_", All_sites[[data_site]]$year)

Tmin_df <- data.frame(data_Tmin)
colnames(Tmin_df) <- paste0(All_sites[[data_site]]$site, "_", All_sites[[data_site]]$year)


#function to reset Sf when hit T_thres
T_thres_func <- function(col){
  as.matrix(ave(Rf_df[[col]], cumsum(Tmin_df[[col]] <= T_thres), FUN = cumsum))
}

#Pull out years
col_names <- names(Tmin_df)

#Make empty matrix
Sf <- matrix(NA, nrow = 365, ncol = length(col_names))

#loop years (columns) through function
for (i in 1:length(col_names)) {
  
  year <- col_names[i]
  output <- T_thres_func(col = year)
  Sf[,i] <- output
  
}


# DOY of budburst criterium
doy <- apply(Sf,2, function(xt){
  All_sites[[data_site]]$doy[which(xt >= F_crit)[1]]
})

doy

#Add DOY to accumulation data
Sw <- cbind(DOY, Sw)
Sf <- cbind(DOY, Sf)


#Graph results
num = length(doy)+1
num


#Plot Sw (accumulated precip) by year
matplot(Sw[,1],Sw[,2:num], lty = 1, lwd = 2, type = "l", col = glasbey(num), 
        ylim = c(0,50), xlim = c(-100,100),
        cex.axis=1.5, tck = 0.02, las = 1,
        xlab = "" , ylab = "")
title(xlab = "DOY", cex.lab = 1.8)
title(ylab = "Accumulated Precip", line = 4.5, cex.lab = 1.8)
text(90, 23, "P-req", cex = 1.5)
abline(h = P_req, lty = 2)
legend("topleft", lty = 1, lwd = 2, colnames(Rf[,1:num-1]),cex = 1.2, col = glasbey(num), ncol = 2, bty = "n")
mtext("b", side = 3, line = 0.5, adj = 0, cex = 2)


#Plot Sf (accumulated temp) by year
matplot(Sf[,1],Sf[,2:num], lty = 1, lwd = 2, type = "l", col = glasbey(num), 
        ylim = c(0, 400), xlim = c(-10, 150),
        cex.axis=1.5, tck = 0.02, las = 1,
        xlab = "" , ylab = "")
        title(xlab = "DOY", cex.lab = 1.8)
        title(ylab = "Accumulated Temp (°C)", line = 4.5, cex.lab = 1.8)
        text(142, 215, "F-crit", cex = 1.5)
        abline(h = F_crit, lty = 2)
        mtext("c", side = 3, line = 0.5, adj = 0, cex = 2)
        
        

        #text(48, 270, "uiefprairie", cex = 2.8)
        #legend("topleft", colnames(Rf[,1:num-1]),cex=1.3,fill= glasbey(num))

#View color palette options
#pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome, 
#          stepped, tol, watlington,
#          show.names=FALSE)

##Try in ggplot instead?

#stop writing png        
dev.off()


#############Arrow plots#######################

#reset par to default
dev.off()

#Save to computer as png
png("G:/My Drive/Project/Model edits/R files/Published code/Data visualization/output/ArrowPlots_2.png", width = 11.5, height = 8, units = 'in', res = 300)


#Set up panels
par(mfrow=c(2,1))

#set margins
par(mai=c(0.3,2.2,1.2,2.2))

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
  
  # provide the baseline plot (don't plot values)
  graphics::plot(measured,
                 predicted_values[,1],
                 type = "n",
                 ylab = "",
                 ylim = c(0, 250), xlim = c(0,250),
                 xaxs = "i", yaxs = "i",
                 axes = FALSE)
  
  title(main = sprintf("%s to %s",
                       model_names[1],model_names[2]),
        cex.main = 1.5, 
        line = 1)
  
  title(ylab = "Predicted (DOY)", line = 3.5, cex.lab = 1.5)
  
  Axis(side=1, labels=FALSE, tck = 0.02, las = 1)
  Axis(side=2, labels=TRUE, tck = 0.02, las = 1, cex.axis = 1.2, cex.lab = 1.5)
  Axis(side=3, labels=FALSE, ticks = FALSE, col.ticks = alpha("white", 0.1))
  Axis(side=4, labels=FALSE, ticks = FALSE, col.ticks = alpha("white", 0.1))
  
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
  
  # plot the arrows with the correct colours
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


#Test new function
pr_plot_arrows_new(data = AllSites_BestRuns, models = c("SQW", "SQWs"))
text(200, 35, "RMSE: 18.38 to 16.25", cex = 1.3)
text(15, 220, "a", cex = 3)

##2nd plot

#set new margins
par(mai=c(1,2.2,0.5,2.2))

#new function
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
  
  # provide the baseline plot (don't plot values)
  graphics::plot(measured,
                 predicted_values[,1],
                 type = "n",
                 ylab = "",
                 xlab = "Observed (DOY)",
                 ylim = c(0, 250), xlim = c(0,250),
                 xaxs = "i", yaxs = "i",
                 tck = 0.02, las = 1, pty = "s",
                 cex.lab = 1.5, cex.axis = 1.2)
  
  title(main = sprintf("%s to %s",
                       model_names[1],model_names[2]),
        cex.main = 1.5, 
        line = 1)
  
  title(ylab = "Predicted (DOY)", line = 3.5, cex.lab = 1.5)
  
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
  
  # plot the arrows with the correct colours
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


#Test new function
pr_plot_arrows_new(data = AllSites_BestRuns, models = c("SQWs", "SQWs_Tmin"))
text(200, 35, "RMSE: 16.25 to 15.98", cex = 1.3)
text(15, 220, "b", cex = 3)

#stop making png
dev.off()





