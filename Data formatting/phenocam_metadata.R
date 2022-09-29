#Download metadata for all grassland sites

# installing the package from the GitHub repo
if(!require(devtools)) install.packages('devtools')
devtools::install_github('bnasr/phenocamapi')

# loading the package
library(phenocamapi)

#Get metadata table
phenos_table <- get_phenos()

#see column names
colnames(phenos_table)

#Filter GR sites
pheno_GR <- phenos_table[primary_veg_type=="GR"]

#Remove "list" type variable in tables
str(phenos_table)
phenos_table$flux_networks <- NULL

str(pheno_GR)
pheno_GR$flux_networks <- NULL

#Save tables
write.csv(phenos_table,"G:/My Drive/Project/Model edits/csv files/phenocam_site_metadata.CSV", row.names= FALSE)

write.csv(pheno_GR,"G:/My Drive/Project/Model edits/csv files/grassland_site_metadata.CSV", row.names= FALSE)



