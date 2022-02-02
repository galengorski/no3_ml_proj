#===================================================================================#
# NOTES: script for joining hydro data to climate data and adding in static 
# attributes
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
# install.packages('magrittr')
library(magrittr)
 install.packages('netcdf4')
library(netcdf4)
#####
#===================================================================================#

hydro_data <- read_csv('02_munge/out/all_sites_data_bfs.csv')
sites <- unique(hydro_data$site_no)
hydro_data_temp <- hydro_data %>%
  filter(site_no == sites[i])
met_data <- read_csv(paste0('01_fetch/out/met_data/',sites[i],'_met_data.csv'))

nrow(hydro_data_temp) == nrow(met_data)
hydro_met <- merge(hydro_data_temp, met_data, by.x = 'Date', by.y = 'date', all = TRUE) %>%
  select(-...1)

basin_char <- read_csv(paste0('01_fetch/out/basin_char/',sites[i],'_basin_char.csv'))
land_cover <- read.table(paste0('01_fetch/out/nlcd_data/land_cover_',sites[i],'.csv'), header = TRUE) %>%
  mutate(characteristic_id = paste0('NLCD_', cat), characteristic_value = value, percent_nodata = NA) %>%
  select(characteristic_id, characteristic_value, percent_nodata)

basin_char_lc <- rbind(basin_char, land_cover)
