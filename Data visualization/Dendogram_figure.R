####cluster analysis & dendrogram####

#load packages
library(dendextend)

#Can repeat for All Sites and each KG group
#read in resid table for desired dataset

#Upload datasets
setwd("G:/My Drive/Project/Model edits/R files/Published code/Model fitting")

#All models (including SM models)
AllSites_combo_resid <- read.csv("./output/resid_table_combo.csv")

#Just original & precip models for each KG group
KG_B_sites_resid <- read.csv("./output/KG_B_resid_table.csv")
KG_Cf_sites_resid <- read.csv("./output/KG_Cf_resid_table.csv")
KG_Cs_sites_resid <- read.csv("./output/KG_Cs_resid_table.csv")
KG_Df_sites_resid <- read.csv("./output/KG_Df_resid_table.csv")

#Specify which dataset to use
resid_table <- AllSites_combo_resid

#rotate table
resid_table2 <- t(resid_table)
resid_table2 <- resid_table2[-1,]

#Find distance matrix
dist_mat <- dist(resid_table2, method = 'euclidean')


#fit hierarchical clustering model to data
#cluster method = average, distance = euclidean
set.seed(250)
hier_clus <- hclust(dist_mat, method = "average")
hier_clus

#try other clustering methods
#ward.D, median, centroid, mcquitty, complete (others available)
#plot(hclust(dist_mat, method = "complete"))

#plot dendrogram
#x-axis = distance matrix, y-axis = height (euclidean distance)
plot(hier_clus)

#cut tree by # of clusters
#lists which cluster each model belong to- can specify # of groups
fit <- cutree(hier_clus, k = 5)
fit

#show # of models in each cluster
table(fit)

#Make border around the different clusters on dendrogram
rect.hclust(hier_clus, k = 9, border = "coral3")


#Make final tree using dendextend package

#set plot margins
par(mar=c(8,4.1,4.1,2.1))

#Make dendrogram object
dend <- as.dendrogram(hclust(dist_mat, method = "average"))
plot(dend)


#make leaves hang at uneven heights
dend <- hang.dendrogram(dend, hang = 0.1)
plot(dend)

#color labels by group
dend <- color_labels(dend, 9, col = c("cornflowerblue", "mediumseagreen", "cornflowerblue", 
                                      "cornflowerblue", "mediumseagreen", "cornflowerblue", 
                                      "black", "black", "black"))

#change label size
dend <- set(dend, "labels_cex", 0.9)
plot(dend)

#add clustering borders
dend %>% rect.dendrogram(k=9, border = 2)


#unique color for each group
# dend2 %>% set("labels_col", k = 9) %>%
#   plot()
