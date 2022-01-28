#from: http://usgs-r.github.io/dataRetrieval/articles/nldi.html
library(dataRetrieval) # The star of the show!
library(dplyr)         # Data frame manipulation
library(ggplot2)
library(mapview)
library(tidyverse)
library(sf)
library(nhdplusTools)
library(raster)
library(lubridate)
#remotes::install_github("mikejohnson51/climateR")
library(climateR)


site_data <- read_csv('out/site_list_220128.csv')
all_data <- read_csv('out/hydro_filled_220128.csv')

i = 24

for (i in 50:nrow(site_data)){
iter_start <- Sys.time()
single_site <- all_data %>%
  filter(site_no == site_data$site_no[i])


#### DRAINAGE AREA
startTime <- Sys.time()
mapviewOptions(basemaps = c("Esri.WorldImagery"))
test <- findNLDI(nwis = site_data$site_no[i], nav = c('UM', 'UT'), find = c('nwis','basin','flowlines'), no_sf = FALSE)
m <- mapview(test$basin, layer.name = 'Catchment Area', col.regions = 'goldenrod', alpha.regions = 0.5)+
  mapview(test$origin, layer.name = site_data$station_nm[i], col.regions = 'cyan', alpha.regions = 1)+
  mapview(test$UT_flowlines, layer.name = 'Tributaries', color = 'blue', legend = NULL, label = TRUE)+
  mapview(test$UM_flowlines, layer.name = 'Main Stem', legend = NULL, color = 'red')

mapshot(m, url = paste0('out/watersheds/',site_data$station_nm[i],'_',site_data$site_no[i],'.html'))
endTime <- Sys.time()  
drainage_area_time = endTime-startTime

print(paste0('Site ',i,' of ',nrow(site_data), '| Drainage area data saved: '))
print(drainage_area_time)

#### BASIN CHARACTERISTICS
startTime <- Sys.time()
#get basin characteristics
basin_char <- get_nldi_characteristics(list(featuresSource = 'nwissite', featureID = paste0('USGS-',site_data$site_no[i])))
write_csv(basin_char$local,paste0('out/basin_char/',site_data$site_no[i],'_basin_char.csv'))

#for meta data
#char_meta <- discover_nldi_characteristics()
endTime <- Sys.time()  
basin_char_time = endTime-startTime

print(paste0('Site ',i,' of ',nrow(site_data), '| Basin char data saved: '))
print(basin_char_time)

#### MET DATA
#GridMET data
startTime <- Sys.time()
gm_extent <- getGridMET(test$basin, param = c('prcp','tmin','tmax'), startDate = single_site$Date[1], endDate = single_site$Date[nrow(single_site)])

gm_brick <- brick(gm_extent)

gm_basin_mask <- mask(gm_brick, test$basin, maskValue = NA)

n <- cellStats(gm_basin_mask[[names(gm_basin_mask)]], mean)

mean_vals <- tibble(date = names(n), value = unname(n)) %>%
  separate(date, c('year','month','day','variable')) %>%
  mutate(year = gsub("X", "", year)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::select(date, value, variable) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(prcp = `1`, tmin = `2`, tmax = `3`) %>%
  dplyr::select(date, prcp, tmin, tmax)

write_csv(mean_vals, paste0('out/met_data/',site_data$site_no[i],'_met_data.csv'))

endTime <- Sys.time()  

met_time = endTime-startTime

print(paste0('Site ',i,' of ',nrow(site_data), '| Met data saved: '))
print(met_time)

iter_end <- Sys.time()
iter_time = iter_end-iter_start

print(paste0('Site ',i,' of ',nrow(site_data), '| Total time: '))
print(iter_time)

}