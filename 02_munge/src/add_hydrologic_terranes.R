#===================================================================================#
# NOTES: script for adding hydrologic terranes to site_info data
#
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-05-13                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
library(sp)
library(rgdal)
library(sf)
#####
#===================================================================================#
#Terranes were downloaded from:
#https://www.sciencebase.gov/catalog/item/5a3827eae4b0d05ee8b21321

gdb <- path.expand('01_fetch/in/Haj_Terranes/HydrogeologicTerranes.gdb')
ogrListLayers(gdb)
ht <- readOGR(gdb, "HydrogeologicTerranes_poly")
ht.rpj <- spTransform(ht, CRS("+init=epsg:4326"))
site_info <- read_csv('04_analysis/out/basin_char_w_clusters_6.csv')

pal <- colorBin("Spectral", site_info$cluster, 8, pretty = TRUE, reverse = TRUE)

leaflet(site_info) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(-84.699, 37.5, zoom=5) %>%
  addPolygons(data = ht.rpj, label = ht.rpj$Terrane) %>%
  addCircleMarkers( lng = ~long, lat = ~lat,
                   fillColor = ~pal(cluster), stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 1,
                   radius = 6, color = 'black', label = site_info$station_nm)

#conert to spdf
site_info_spdf = SpatialPointsDataFrame(coords = site_info[,c('long','lat')], proj4string = CRS('EPSG:4326'), data = site_df)

site_info$hydro_terrane <- over(xy,ht.rpj[1])$Terrane
site_info[is.na(site_info$hydro_terrane),]$hydro_terrane <- '00'

write_csv(site_info, '04_analysis/out/basin_char_w_clusters_6_hydro_terranes.csv')
