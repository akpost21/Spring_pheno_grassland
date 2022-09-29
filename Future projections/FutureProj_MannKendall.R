#packages
library(dplyr)
library(stringr)
library(zyp)
library(Kendall)
library(ggplot2)


#Read in file with all future projection results
All <- read.csv("G:/My Drive/Project/Model edits/R files/Published code/Future projections/output/All_FutureProj_combo.csv")


####Calculate Mann-Kendall stats####

#Calculate median values
All_med <- All %>%
  group_by(site,year) %>%
  summarise_at(vars(predicted), list(predicted = median))

#convert to data frame
All_med <- data.frame(All_med)
str(All_med)

#Divide by KG cliamte group
All_med_Cf <- subset(All_med, site == 'kansas')
All_med_Cs <- subset(All_med, site == 'vaira')
All_med_Df <- subset(All_med, site == 'oakville')
All_med_B <- subset(All_med, site == 'ibp')

#Mann-Kendall test
MannKendall(All_med_Cf$predicted)
MannKendall(All_med_Cs$predicted)
MannKendall(All_med_Df$predicted)
MannKendall(All_med_B$predicted)


####Determine Sen's slope for each future projection####

#Create unique ID to split by model & site
All$model_site <- paste0(All$model, "_", All$site)

#Pull out ID names
names <- unique(All$model_site)
names


###Loop to calculate Sen's slope for each ID (model/site combo)

#make empty matrix to hold Sen's slope results
MK_slopes <- matrix(NA,
                nrow = length(names),
                ncol = 2)

#loop through model_sites
for (i in 1:length(names)) {
  data <- subset(All, model_site == names[i])
  
  test_sen <- zyp.sen(predicted~year,data)
  
  #record slope, intercept
  MK_slopes[i,1] <- test_sen$coefficients[2]
  MK_slopes[i,2] <- test_sen$coefficients[1]}


#combine with model_site ID
MK_slopes_2 <- cbind(names, MK_slopes)


#Divide results by site

#convert to data frame
MK_slopes_2 <- data.frame(MK_slopes_2)
str(MK_slopes_2)

#Divide ID into model & site
split <- str_split_fixed(MK_slopes_2$names, "_", n = 2)

#merge with results
MK_slopes_3 <-cbind(split, MK_slopes_2)
colnames(MK_slopes_3) <- c("model", "site", "ID", "slope", "intercept")

#convert results to numeric
MK_slopes_3$slope <- as.numeric(MK_slopes_3$slope)
MK_slopes_3$intercept <- as.numeric(MK_slopes_3$intercept)
str(MK_slopes_3)


#summarize by site (mean & SD of slope for each site)
Site_avg <- MK_slopes_3 %>%
  group_by(site) %>%
  summarise_at(vars(slope, intercept), list(mean = mean, sd = sd ))

str(Site_avg)
Site_avg <- data.frame(Site_avg)

#save csv
write.csv(Site_avg, "G:/My Drive/Project/Model edits/R files/Published code/Future projections/output/Mann_Kendall_results.csv")


####graph results####

#Change labels for facets (by changing names in data)
All_2 <- All %>%
  mutate(site = recode(site, "ibp" = "B: Jornada", "kansas" = "Cf: Kansas", "oakville" = "Df: Oakville", "vaira" = "Cs: Vaira"))

#re-order factors for facets order
All_2$site <- as.factor(All_2$site)
str(All_2)

All_2$site <- factor(All_2$site, levels=c("Df: Oakville", "Cf: Kansas","vaira" = "Cs: Vaira","B: Jornada"))
All_2$site

#Create median line
All_med <- All_2 %>%
  group_by(site,year) %>%
  summarise_at(vars(predicted), list(predicted = median))

#convert to data frame
All_med <- data.frame(All_med)
str(All_med)

#Get names of sites
Sens_slopes_names <- data.frame(site = unique(All_2$site))
Sens_slopes_names

#Combine all slopes
Sens_slopes_all <- cbind(Sens_slopes_names, Site_avg$intercept_mean, Site_avg$slope_mean)
colnames(Sens_slopes_all) <- c("site", "intercept", "slope")

#collect text labels with p-val and slope
slope_labels <- data.frame(Sens_slopes_names, slope = c("0.035 ± 0.080 ", "-0.047 ± 0.023", "-0.111 ± 0.027", "-0.123 ± 0.060"),
                           x = c(2075, 2075, 2075, 2075), y = c(285, 147, 152, 110))

p_labels <- data.frame(Sens_slopes_names, p_val = c( "p = 0.776","p < 0.001","p < 0.001","p < 0.001"),
                       x = c(2025, 2025, 2025, 2025), y = c(285, 147, 152, 110))


#plot results

#Create dummy data point to adjust y-axis for ibp facet
dummy <- data.frame(model = "dummy",
                    site = c("B: Jornada","Cf: Kansas","Df: Oakville","Cs: Vaira"),
                    year = 2020,
                    predicted = c(300, 145, 155, 115))

dummy$site <- factor(dummy$site, levels=c("Df: Oakville", "Cf: Kansas", "vaira" = "Cs: Vaira","B: Jornada"))
dummy$site


#set device to save png as file (specify file path)
#comment out to see graphs in RStudio viewer
png("G:/My Drive/Project/Model edits/R files/Published code/Future projections/output/future_proj_Sens.png", width = 10.5, height = 6.75, units = 'in', res = 300)

#line graph
g <- ggplot(data = All_2, mapping = aes(x = year, y = predicted))+
  geom_point(aes(group = model), color = "grey", alpha=0.65,size=0.6)+
  geom_line(aes(group = model), color = "grey", alpha=0.65)+
  facet_wrap(facets = vars(site), scales = "free")+
  labs(title="Future Projections", y="SOS (DOY)", x ="Year")+
  theme_classic()+
  theme(strip.text = element_text(size = 15), axis.text = element_text(size = 11),
        axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5, size = 20),
        panel.border = element_rect(color = "black", fill = NA, size = 1))
g


#Add median and Sens slope lines
g <- g + geom_line(data = All_med, aes(group = site), colour="black", size=1.5)
g <- g + geom_abline(data = Sens_slopes_all, size = 1, aes(intercept = intercept, slope = slope), col = "red")
g <- g + geom_text(data = slope_labels, mapping = aes(x=x, y=y, label = slope), col = "red", cex = 5)
g <- g + geom_text(data = p_labels, mapping = aes(x=x, y=y, label = p_val), cex = 5)
g <- g + geom_blank(data = dummy) #add dummy point to adjust y-axis for ibp
g


#stop writing png        
dev.off()
