#===================================================================================#
# NOTES: Script for downloading NLCD data
#
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-01-29                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
# install.packages('nhdplusTools')
library(nhdplusTools)
# install.packages('raster')
library(raster)
#install.packages("devtools")
#devtools::install_github("ropensci/FedData")
library(FedData)
#####
#===================================================================================#


site_info <- read_csv('01_fetch/out/site_list_220128.csv') 

for(i in 1:nrow(site_info)){
nldi_site <- findNLDI(nwis = site_info$site_no[i], find = c('basin'), no_sf = FALSE)
#nldi_nwis <- list(featureSource = "nwissite", featureID = paste0("USGS-",site_info$site_no[i]))

#basin <- get_nldi_basin(nldi_feature = nldi_nwis)

test_nlcd <- get_nlcd(template = nldi_site$basin, label = site_info$site_no[i], year = 2016)

basin.rpj <- st_transform(nldi_site$basin, crs = st_crs(test_nlcd))

nlcd_mask <- mask(test_nlcd, basin.rpj, maskValue = NA)

#make sure the clipping worked correctly
# mapview(nlcd_mask)+
#   mapview(nldi_site$basin)

summary <- nlcd_mask@data@values %>% 
  table()/cellStats(nlcd_mask, stat = 'countNA')
  
summary_tib <- tibble(cat = names(summary), value = unname(summary))
  
write.table(summary_tib, paste0('land_cover_',site_info$site_no[i],'.csv'), row.names = FALSE)
}

