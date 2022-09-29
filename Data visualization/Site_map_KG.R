##Make site map with 2-letter KG classfication as background

#Based on Bijan's code
#multiple packages will be retired at the end of 2023!

library(maps)
library(maptools)
library(mapdata)
library(rgdal)
library(data.table)
library(raster)
library(rgeos)
library(sp)
library(GISTools)
library(scales)#make points transparent

#set working directory
setwd("G:/My Drive/Project/Model edits/R files/Published code/Data visualization")

#read in KG raster
r <- raster("./KG_layer/KG_1986-2010.grd")

# Select region (US)
x1=-130; x2=-68; y1=24; y2=50.75; xat=5; yat=5

r <- crop(r, extent(x1, x2, y1, y2))


#basic plot
plot(r)

#Set colors to use
climate_colors_2 <- c("#960000","#CC8D14", "#FFCC00",
                      "#005000","#96FF00","#00AA00",
                    "#820082", "#C800C8", "#FF6EFF",
                     "#8C8C8C", "#F5FFFF")


#plot color ramp
barplot(rep(1, 11), axes = FALSE, space = 0, col = climate_colors_2)


##prep other layers
#country outlines
countries = maps::map('worldHires',c('Canada','USA','Mexico'),
                      fill=TRUE,
                      plot = FALSE)

#transform to EPSG2163
proj <- crs(r)

#grab IDs
IDs = sapply(strsplit(countries$names, ":"), function(x) x[1])

#re-project
countries = map2SpatialLines(countries, IDs = IDs, proj4string = proj)
countries <- spTransform(countries, proj4string(r)) 

countries <- crop(countries, extent(x1, x2, y1, y2))


#read in site locations
GR_loc_df <- read.csv("./grassland_sites_coordinates.CSV")

#convert to spatial object with same projection
xy <- GR_loc_df[,c(2,3)]
GR_pts <- SpatialPointsDataFrame(coords = xy, data = GR_loc_df,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

#pull out # data category for each site
data_cex <- as.vector(GR_loc_df[,6])

##Combine everything
#change plot margins

par(mar = c(4, 2, 4, 10))

plot(r, 
     breaks = c(1,4.9,6.9,8.9,11.9,14.9,17.9,21.9,25.9,29.9,31.9), 
     col = climate_colors_2,
     axes=FALSE, 
     box = FALSE,
     legend = FALSE, 
     alpha = 0.6)

title("PhenoCam Grassland Sites", adj = 0.5, line = -2.8, cex.main = 2)


# country outlines
lines(countries,col="black", lwd = 2)

# Add USA sates outline
maps::map("state", add = TRUE, col = "black")

#Add points to map
#points scaled to # data year bins
#alpha changes point transparency
points(GR_pts, pch=21, cex = data_cex , bg = alpha("white", 0.8), col = "black", lwd = 2)

#Add legends
legend_cols <-c("#960000","#CC8D14", "#FFCC00", 
                "#005000","#96FF00","#00AA00",  
                "#820082", "#C800C8", "#FF6EFF", 
                "#8C8C8C")

legend("topright",
       inset = c(-0.28, 0.15),
       title = "Köppen-Geiger Zone",
       legend = c("A","BS", "BW", "Cf", "Cs", "Cw" , "Df", "Ds", "Dw", "E"),
       pch = 22,
       cex = 1.2,
       pt.bg = alpha(legend_cols, 0.6),
       pt.cex = 2,
       bty='n',
       xpd = TRUE)



legend("bottomright",
       inset = c(-0.22,.1),
       title = "# Data Years",
       x.intersp = 1,
       legend= c("1-5", "6-10", "11-15", "16-20"),
       pch=21,
       cex = 1.2,
       pt.cex = c(1,1.5,2,2.5),
       col="black",
       pt.bg= "white",
       pt.lwd=2,
       bty='n',
       xpd = TRUE)

north.arrow(xb=-72, yb=28, len= 0.75, lab="N") 



##Save map directly to png file

# set device
png("./output/site_map.png", width = 11, height = 8.4, units = 'in', res = 300)

par(mar = c(4, 2, 4, 10))

plot(r, 
     breaks = c(1,4.9,6.9,8.9,11.9,14.9,17.9,21.9,25.9,29.9,31.9), 
     col = climate_colors_2,
     axes=FALSE, 
     box = FALSE,
     legend = FALSE, 
     alpha = 0.60)

title("PhenoCam Grassland Sites", adj = 0.5, line = -3.5, cex.main = 2)


# country outlines
lines(countries,col="black", lwd = 2)

# Add USA sates outline
maps::map("state", add = TRUE, col = "black")

#Add points to map
#points scaled to # data year bins
#alpha changes point transparency
points(GR_pts, pch=21, cex = data_cex , bg = alpha("white", 0.8), col = "black", lwd = 2)


#Add legends
legend_cols <-c("#960000","#CC8D14", "#FFCC00", 
                "#005000","#96FF00","#00AA00",  
                "#820082", "#C800C8", "#FF6EFF", 
                "#8C8C8C")

legend("topright",
       inset = c(-0.28, 0.15),
       title = "Köppen-Geiger Zone",
       legend = c("A","BS", "BW", "Cf", "Cs", "Cw" , "Df", "Ds", "Dw", "E"),
       pch = 22,
       cex = 1.2,
       pt.bg = alpha(legend_cols, 0.6),
       pt.cex = 2,
       bty='n',
       xpd = TRUE)


legend("bottomright",
       inset = c(-0.22,.2),
       title = "# Data Years",
       x.intersp = 1,
       legend= c("1-5", "6-10", "11-15", "16-20"),
       pch=21,
       cex = 1.2,
       pt.cex = c(1.5,2,2.5,3),
       col="black",
       pt.bg= "white",
       pt.lwd=2,
       bty='n',
       xpd = TRUE)

north.arrow(xb=-72, yb=28, len= 0.75, lab="N") 

#Stop creating png figure
dev.off()
