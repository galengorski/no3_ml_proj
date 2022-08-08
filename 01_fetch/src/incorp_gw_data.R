#===================================================================================#
# NOTES: incorporating gw data from 
# https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019WR026724 
# data: https://water.usgs.gov/GIS/metadata/usgswrd/XML/zell2020_wrr.xml #
# https://water.usgs.gov/GIS/dsdl/gwmodels/zell2020_wrr/readme.txt
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# ggorski@usgs.gov                                                                  #
# 2022-05-06                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
# install.packages('raster')
library(raster)
# install.packages('rgdal')
library(rgdal)
# install.packages('sf')
library(sf)
# install.packages('nhdplusTools')
library(nhdplusTools)
#####
#===================================================================================#


site_info <- read_csv('01_fetch/out/site_list_220507.csv', col_types = cols(site_no = col_character())) 

unsat_tt <- raster('01_fetch/in/Zell_Sanford/Output_CONUS_unsat_traveltime/conus_MF6_SS_Unconfined_250_tt_mid.tif')
dtw <- raster('01_fetch/in/Zell_Sanford/Output_CONUS_trans_dtw/conus_MF6_SS_Unconfined_250_dtw.tif')
trans <- raster('01_fetch/in/Zell_Sanford/Output_CONUS_trans_dtw/conus_MF6_SS_Unconfined_250_trans.tif')
wc <- raster('01_fetch/in/Zell_Sanford/Output_CONUS_unsat_watercontent/conus_MF6_SS_Unconfined_250_wc_avg.tif')
# read in the states boundaries "no water" polygon to clip to
boundary <- shapefile("01_fetch/in/National_NO3/Inputs/states_merged/states_boundaries_1m_nowater.shp")
dom_no3 <- raster('01_fetch/in/National_NO3/Outputs/published_pred_nitrate_rasters/no3_doms.asc')
pub_no3 <- raster('01_fetch/in/National_NO3/Outputs/published_pred_nitrate_rasters/no3_pubs.asc')
crs(dom_no3) <- crs(boundary)
crs(pub_no3) <- crs(boundary)

gw_df <- data.frame()

for (i in 1:nrow(site_info)){

  nldi_nwis <- list(featureSource = "nwissite", featureID = paste0("USGS-",site_info$site_no[i]))
  
  basin <- get_nldi_basin(nldi_feature = nldi_nwis)
  
  basin.rpj <- st_transform(basin, crs = st_crs(unsat_tt))
  
  unsat_tt_cropped <- raster::crop(unsat_tt, basin.rpj)
  unsat_tt_mask <- raster::mask(unsat_tt_cropped, basin.rpj)
  unsat_tt_mask@data@values[!is.finite(unsat_tt_mask@data@values)] <- NA
  
  dtw_cropped <- raster::crop(dtw, basin.rpj)
  dtw_mask <- raster::mask(dtw_cropped, basin.rpj)
  dtw_mask@data@values[!is.finite(dtw_mask@data@values)] <- NA
  
  trans_cropped <- raster::crop(trans, basin.rpj)
  trans_mask <- raster::mask(trans_cropped, basin.rpj)
  trans_mask@data@values[!is.finite(trans_mask@data@values)] <- NA
  
  wc_cropped <- raster::crop(wc, basin.rpj)
  wc_mask <- raster::mask(wc_cropped, basin.rpj)
  wc_mask@data@values[!is.finite(wc_mask@data@values)] <- NA
  
  basin.rpj <- st_transform(basin, crs = st_crs(dom_no3))
  
  dom_no3_cropped <- raster::crop(dom_no3, basin.rpj)
  dom_no3_mask <- raster::mask(dom_no3_cropped, basin.rpj)
  dom_no3_mask@data@values[!is.finite(dom_no3_mask@data@values)] <- NA
  
  pub_no3_cropped <- raster::crop(pub_no3, basin.rpj)
  pub_no3_mask <- raster::mask(pub_no3_cropped, basin.rpj)
  pub_no3_mask@data@values[!is.finite(pub_no3_mask@data@values)] <- NA
  
  characteristic_id <- c('UNSAT_TT','DTW','TRANSM','WCON','NO3_DOM','NO3_PUB')
  characteristic_value <- c(cellStats(unsat_tt_mask, stat = 'mean', na.rm = TRUE), cellStats(dtw_mask, stat = 'mean', na.rm = TRUE), 
                            cellStats(trans_mask, stat = 'mean', na.rm = TRUE), cellStats(wc_mask, stat = 'mean', na.rm = TRUE),
                            cellStats(dom_no3_mask, stat = 'mean', na.rm = TRUE), cellStats(pub_no3_mask, stat = 'mean', na.rm = TRUE))
  percent_nodata <- rep(NA, 6)
  site_no <- rep(site_info$site_no[i], 6)
  gw_site_df <- data.frame(site_no = site_no,
                      characteristic_id = characteristic_id,
                      characteristic_value = characteristic_value,
                      percent_nodata = percent_nodata)
  
  gw_df <- rbind(gw_df, gw_site_df)
}

write_csv(gw_df, '01_fetch/out/gw_char.csv')

# gw_df$long_name <- NA
# gw_df[gw_df$characteristic_id == 'DTW',]$long_name <- 'Depth to water (m)'
# gw_df[gw_df$characteristic_id == 'TRANSM',]$long_name <- 'Aquifer transmissivity (m2/day)'
# gw_df[gw_df$characteristic_id == 'UNSAT_TT',]$long_name <- 'Travel time in unsaturated zone (years)'
# gw_df[gw_df$characteristic_id == 'WCON',]$long_name <- 'Avg water content in unsaturated zone (volumetric fraction)'
# 
# ggplot(gw_df, aes(x = characteristic_value)) +
#   geom_histogram() +
#   facet_wrap(.~long_name, scales = 'free_x')