#Replace Daymet precip with Ameriflux or SEGA precip for Southwest sites

#load packages
library("readxl")

#set working directory
setwd("G:/My Drive/Project/Model edits/R files/Published code/Data formatting")

#read in phenocam data  with SM added
phen_dates_combined = readRDS("./output/All_sites_trans50_SM_Mosaic_0-10.rds")


#Replace sites with SEGA precip

ArbMead_sega <- read_excel("./SEGA & Ameriflux precip/ArbMeadowDaily.xlsx", sheet = "Precip_formatted data")

phen_dates_combined$segaarboretummeadow$Pi<- NULL
phen_dates_combined$segaarboretummeadow$Pi <- as.matrix(ArbMead_sega)


Bluechute_sega <- read_excel("./SEGA & Ameriflux precip/blueChuteDaily.xlsx", sheet = "Precip_formatted data")

phen_dates_combined$segabluechute$Pi<- NULL
phen_dates_combined$segabluechute$Pi <- as.matrix(Bluechute_sega)


Hartprairie_sega <-  read_excel("./SEGA & Ameriflux precip/HartPrairieDaily.xlsx", sheet = "Precip_formatted data")

phen_dates_combined$segahartprairie$Pi<- NULL
phen_dates_combined$segahartprairie$Pi <- as.matrix(Hartprairie_sega)


WhitePockets_sega <-  read_excel("./SEGA & Ameriflux precip/whitePocketsDaily.xlsx", sheet = "Precip_formatted data")

phen_dates_combined$segawhitepockets$Pi<- NULL
phen_dates_combined$segawhitepockets$Pi <- as.matrix(WhitePockets_sega)


blackpoint_sega <-  read_excel("./SEGA & Ameriflux precip/BlackPointDaily.xlsx", sheet = "Precip_formatted data")

phen_dates_combined$segablackpoint$Pi<- NULL
phen_dates_combined$segablackpoint$Pi <- as.matrix(blackpoint_sega)



#Replace sites with Ameriflux precip

kendall_AF <- read_excel("./SEGA & Ameriflux precip/Kendall_precip.xlsx", sheet = "kendall_Ameriflux")

phen_dates_combined$kendall$Pi<- NULL
phen_dates_combined$kendall$Pi <- as.matrix(kendall_AF)


NEON_MOAB_AF <- read_excel("./SEGA & Ameriflux precip/NEON-MOAB_precip.xlsx", sheet = "NEON_MOAB_Ameriflux")

phen_dates_combined$NEON.D13.MOAB.DP1.00033$Pi<- NULL
phen_dates_combined$NEON.D13.MOAB.DP1.00033$Pi <- as.matrix(NEON_MOAB_AF)


sevnewgrass_AF <- read_excel("./SEGA & Ameriflux precip/sevilletanewgrass_precip.xlsx", sheet = "sevilletanewgrass_Ameriflux")

phen_dates_combined$sevilletanewgrass$Pi<- NULL
phen_dates_combined$sevilletanewgrass$Pi <- as.matrix(sevnewgrass_AF)


srm_AF <- read_excel("./SEGA & Ameriflux precip/srm_precip.xlsx", sheet = "srm_Ameriflux")

phen_dates_combined$srm$Pi<- NULL
phen_dates_combined$srm$Pi <- as.matrix(srm_AF)


#Save dataset
saveRDS(phen_dates_combined, "./output/All_sites_trans50_SM_Mosaic_0-10_Ameriflux.rds")

