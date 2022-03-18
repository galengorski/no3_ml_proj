#from: http://usgs-r.github.io/dataRetrieval/articles/nldi.html
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


site_data <- read_csv('01_fetch/out/site_list_220128.csv')
all_data <- read_csv('01_fetch/out/hydro_filled_220128.csv')
fertilizer_data <- read.table('~/Google Drive/My Drive/ESDL Postdoc/02_Projects/Midwest_USGS/02_Spatial_Data/Fertilizer_Data/2012_catchment_fert_use_estimates_N_P.txt', header = TRUE, sep = ',')

i = 48

for (i in 1:nrow(site_data)){
  iter_start <- Sys.time()
  single_site <- all_data %>%
    filter(site_no == site_data$site_no[i])
  
  
  #### DRAINAGE AREA
  startTime <- Sys.time()
  mapviewOptions(basemaps = c("Esri.WorldImagery"))
  #download basin geometry 
  test <- findNLDI(nwis = site_data$site_no[i], nav = c('UM', 'UT'), find = c('nwis','basin'))#,'flowlines'), no_sf = FALSE)
  # #create map
  # m <- mapview(test$basin, layer.name = 'Catchment Area', col.regions = 'goldenrod', alpha.regions = 0.5)+
  #   mapview(test$origin, layer.name = site_data$station_nm[i], col.regions = 'cyan', alpha.regions = 1)+
  #   mapview(test$UT_flowlines, layer.name = 'Tributaries', color = 'blue', legend = NULL, label = TRUE)+
  #   mapview(test$UM_flowlines, layer.name = 'Main Stem', legend = NULL, color = 'red')
  # #save map as html
  # mapshot(m, url = paste0('01_fetch/out/watersheds/',site_data$station_nm[i],'_',site_data$site_no[i],'.html'))
  # #calculate time
  # endTime <- Sys.time()  
  # drainage_area_time = endTime-startTime
  # #print time
  # print(paste0('Site ',i,' of ',nrow(site_data), '| Drainage area data saved: '))
  # print(drainage_area_time)
  # 
  # #### BASIN CHARACTERISTICS
  # startTime <- Sys.time()
  # #get basin characteristics
  # basin_char <- get_nldi_characteristics(list(featuresSource = 'nwissite', featureID = paste0('USGS-',site_data$site_no[i])))
  # 
  # #Fertilizer use from https://www.sciencebase.gov/catalog/item/5b9059b4e4b0702d0e80788f
  # #we'll use the unconditinal estimates fert_uN_mt see intro to report paragraph 5
  # fert_temp <- fertilizer_data %>% 
  #   filter(COMID == as.numeric(test$origin$comid))
  # 
  # #add fertilizer to basin char
  # basin_char$local <- rbind(basin_char$local, data.frame(characteristic_id = c('fert_uN_mt', 'fert_kN_mt'), 
  #                                                        characteristic_value = c(fert_temp[1,]$uN_mt ,fert_temp[1,]$kN_mt),
  #                                                        percent_nodata = c(NA,NA)))
  # #write to file
  # write_csv(basin_char$local,paste0('01_fetch/out/basin_char/',site_data$site_no[i],'_basin_char.csv'))
  # 
  # #for meta data
  # #char_meta <- discover_nldi_characteristics()
  # #calculate time
  # endTime <- Sys.time()  
  # basin_char_time = endTime-startTime
  # #print time
  # print(paste0('Site ',i,' of ',nrow(site_data), '| Basin char data saved: '))
  # print(basin_char_time)
  
  
  
  
  #### MET DATA
  #GridMET data
  startTime <- Sys.time()
  #download grid met data for basin
  gm_extent <- getGridMET(test$basin, param = c('prcp','tmin','tmax','srad'), startDate = single_site$Date[1], endDate = single_site$Date[nrow(single_site)])
  #convert raster layers to brick
  gm_brick <- brick(gm_extent)
  #getGridMET returns the raster extent so clip out the basin from the extent
  gm_basin_mask <- mask(gm_brick, test$basin, maskValue = NA)
  #take the daily mean of all parameters
  n <- cellStats(gm_basin_mask[[names(gm_basin_mask)]], mean)
  #clean up data
  mean_vals <- tibble(date = names(n), value = unname(n)) %>%
    separate(date, c('year','month','day','variable')) %>%
    mutate(year = gsub("X", "", year)) %>%
    mutate(date = make_date(year, month, day)) %>%
    dplyr::select(date, value, variable) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(prcp = `1`, tmin = `2`, tmax = `3`, srad = `4`) %>%
    dplyr::select(date, prcp, tmin, tmax, srad)
  #write to file
  write_csv(mean_vals, paste0('01_fetch/out/met_data/',site_data$site_no[i],'_met_data.csv'))
  
  #calculate time
  endTime <- Sys.time()  
  met_time = endTime-startTime
  #print time
  print(paste0('Site ',i,' of ',nrow(site_data), '| Met data saved: '))
  print(met_time)
  
  #calculate total time
  iter_end <- Sys.time()
  iter_time = iter_end-iter_start
  #print total time
  print(paste0('Site ',i,' of ',nrow(site_data), '| Total time: '))
  print(iter_time)
  
}
