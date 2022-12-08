library(dataRetrieval) # The star of the show!
library(dplyr)         # Data frame manipulation
library(ggplot2)
library(mapview)
library(tidyverse)
library(sf)
library(dataRetrieval)
library(nhdplusTools)
library(raster)
library(lubridate)
#remotes::install_github("mikejohnson51/climateR")
library(climateR)
install.packages('prettymapr')
install.packages('rosm')
library(prettymapr)
library(rosm)
install.packages('tmap')
library(tmap)


site_data <- read_csv('01_fetch/out/site_list_220128.csv')
test <- findNLDI(nwis = site_data$site_no[i], nav = c('UM', 'UT'), find = c('nwis','basin'))#,'flowlines'), no_sf = FALSE)
basin_char <- get_nldi_characteristics(list(featuresSource = 'nwissite', featureID = paste0('USGS-',site_data$site_no[i])), type = 'total')
basin_char_comid <- get_nldi_characteristics(list(featureSource = 'comid', featureID =  test$origin$comid))

us_states_map = tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)
tmap_mode('plot')
tm_basemap('Esri.WorldTopoMap')+
  tm_shape(test$basin) +
  tm_polygons() +
  tm_shape(test$origin)+
  tm_dots(col = 'red', size = 0.5)


prettymap(title = paste("Map"), 
          scale.label.cex = 2, scale.padin = c(0.25, 0.25),
          drawarrow = TRUE, arrow.scale = 2,
          mai = c(0.5, 0, 0, 0), { # margin for legend 
            osm.plot(st_bbox(test$basin), type = "cartolight", quiet = TRUE, progress = "none")
            plot(test$basin, lwd = 1, col = 'blue' , border = "black", add = TRUE)
            plot(test$origin, col = "black", bg = "lightgrey", pch = 24, add = TRUE)})


char_meta <- discover_nldi_characteristics()
get_nldi_sources()

bfi <- read.table('~/Downloads/BFI_CONUS.txt', sep = ',', header = TRUE)
contact <- read.table('~/Downloads/CONTACT_CONUS.txt', sep = ',', header = TRUE)
contact_flowlines <- contact[contact$COMID %in% test$UT_flowlines$nhdplus_comid,]
test$origin$comid

bfi_flowlines <- bfi[bfi$COMID %in% test$UT_flowlines$nhdplus_comid,]

bfi_test <- merge(test$UT_flowlines, bfi_flowlines, by.x = 'nhdplus_comid', by.y = 'COMID')
contact_test <- merge(test$UT_flowlines, contact_flowlines, by.x = 'nhdplus_comid', by.y = 'COMID')

(mapview(test$basin) +
  mapview(contact_test, zcol ='TOT_CONTACT') +
  #mapview(test$UM_flowlines)
  mapview(test$UT_nwissite)
)

nhdp <- subset_nhdplus(test$nhdplus_comid, 
                       "nhdp_subset.gpkg", 
                       "download")

nhdp <- subset_nhdplus(comids = test$UM_nwissite$comid,
               output_file = 'nhdplut_output.gpkg',
               nhdplus_data = "download",
               overwrite = TRUE,
               status = TRUE, flowline_only = FALSE)

mapview(nhdp$CatchmentSP)


site <- list(featuresSource = 'nwissite', featureID = paste0('USGS-',site_data$site_no[i]))
site <- navigate_nldi(site, 'UT', 'nwissite')
output_file <- tempfile(fileext = ".gpkg")

nhdp <- subset_nhdplus(site$UT_nwissite$comid, 
                       output_file, 
                       "download", overwrite = TRUE, return_data = TRUE)
mapview(nhdp)
sf::st_layers(output_file)
sf::read_sf(output_file, "CatchmentSP")
catchment <- sf::read_sf(output_file, "CatchmentSP")





download_sb_file <- function(sb_id, file_name, out_dir){
  #' @description Function to download file from ScienceBase
  #' @param sb_id string - the id of the science base item
  #' @param file_name string - the name of the file in the science base item to download
  #' @param out_dir string - the directory where you want the file downloaded to
  #' @value string the out_path
  out_path = file.path(out_dir, file_name)
  # Get the data from ScienceBase:
  sbtools::item_file_download(sb_id = sb_id,
                              names = file_name,
                              destinations = out_path,
                              overwrite_file = TRUE)
  
  return(out_path)
  
}
#all data
item_file_download('5669a79ee4b08895842a1d47', dest_dir=tempdir())
#BFI
item_file_download('5669a8e3e4b08895842a1d4f', dest_dir='~/Google Drive/My Drive/ESDL Postdoc/02_Projects/no3_ml_proj/01_fetch/in/')
#management
list <- item_list_children('56fd600fe4b022712b81bf9a')
item_file_download(list[[2]]$id, dest_dir='~/Google Drive/My Drive/ESDL Postdoc/02_Projects/no3_ml_proj/01_fetch/in/')

full_release <- item_list_children('5669a79ee4b08895842a1d47')
i = 2
full_release[[i]]
item_list_children(full_release[[i]]$id)



gw_no3 <- raster('~/Downloads/National_NO3/Outputs/published_pred_nitrate_rasters/no3_doms.asc')
plot(gw_no3)
