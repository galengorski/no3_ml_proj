#===================================================================================#
# NOTES: script to incorporate tile drainage data from: 
# Valayamkunnath, P., Barlage, M., Chen, F. et al. 
# Mapping of 30-meter resolution tile-drained croplands using a geospatial 
# modeling approach. Sci Data 7, 257 (2020). 
# https://doi.org/10.1038/s41597-020-00596-x                                                                            #
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-08-08                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
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
#site_info$site_no <- str_pad(site_info$site_no, width = 8, pad = 0)
#write_csv(site_info, '01_fetch/out/site_list_220507.csv')

tile_df <- data.frame()
tile_raster <- raster('01_fetch/in/Tile_Drains/AgTile-US-TIFF/AgTile-US.tif')
for (i in 1:nrow(site_info)){
  print(paste0(i,' of 46'))
  print(site_info$station_nm[i])
  
  site_NLDI <- findNLDI(nwis = site_info$site_no[i], nav = c('UM', 'UT'), find = 'basin', no_sf = FALSE)
  
  basin.rpj <- st_transform(site_NLDI$basin, crs = st_crs(tile_raster))
  
  tile_raster_cropped <- raster::crop(tile_raster, basin.rpj)
  tile_raster_mask <- raster::mask(tile_raster_cropped, basin.rpj)
  freq_table <- freq(tile_raster_mask)
  #1 is tile drained, 0 is not
  #get the fraction of the contributing area that is tile drained
  characteristic_value <- freq_table[,'count'][2]/sum(freq_table[,'count'][1:2])
  
  tile_site_df <- data.frame(site_no = site_info$site_no[i],
                           characteristic_id = 'TILE_DRAIN',
                           characteristic_value = characteristic_value,
                           percent_nodata = 0)
  
  tile_df <- rbind(tile_df, tile_site_df)
  
}

write_csv(tile_df, '01_fetch/out/tile_drain_char.csv')
  
  