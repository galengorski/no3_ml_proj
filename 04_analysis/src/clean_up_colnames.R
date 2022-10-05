#===================================================================================#
# NOTES: cleaning up static attributes column names
# 
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-09-23                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
#####
#===================================================================================#

#read in the full suite of basin charactersitics, even ones that were never considered in the model including 
#the calculated metrics like cq slope
basin_char_messy <- read_csv('02_munge/out/basin_char_full.csv')


basin_char_clean <- basin_char_messy
colnames(basin_char_clean) <- gsub('TOT_','',colnames(basin_char_clean))
colnames(basin_char_clean) <- gsub('NLCD_','',colnames(basin_char_clean))

basin_char_clean <- basin_char_clean %>%
  rename_at(vars(BASIN_AREA:BASIN_SLOPE,'STREAM_SLOPE','STREAM_LENGTH'), function(x) paste0('PHYS_',x))%>%
  rename('PHYS_STREAM_DENS' = 'STRM_DENS') %>%
  rename_at(vars(HGA:HGD, SRL55AG), function(x) paste0('SOIL_',x)) %>%
  rename('PHYS_LAT' = 'lat', 'PHYS_LONG' = 'long') %>%
  rename_at(vars(CANALDITCH,ARTIFICIAL,TILES92,NPDES_MAJ,NPDES_MAJ_DENS,RESERVOIR,NORM_STORAGE2013,MAJOR2013,NDAMS2013,NID_STORAGE2013,TILE_DRAIN), function(x) paste0('ANTHRO_',x))%>%
  rename_at(vars(N97), function(x) paste0('CHEM_',x)) %>%
  rename('CHEM_FERT_N' = 'fert_uN_mt_sqkm') %>%
  rename_at(vars(DEV:WTLND, LAKEPOND), function(x) paste0('LULC_',x)) %>%
  rename_at(vars(DTW:NO3_PUB), function(x) paste0("GW_",x)) %>%
  rename_at(vars(BFI:WB5100_ANN), function(x) paste0("HYDRO_",x)) %>%
  rename_at(vars(CWD:PPT7100_ANN), function(x) paste0("CLIMATE_",x)) %>%
  rename('CHEM_MEAN_NO3' = 'mean_no3','CHEM_SD_NO3' = 'sd_no3','CHEM_CQ_SLOPE' = 'cq_slope') %>% 
  rename('HYDRO_MEAN_Q' = 'mean_q', 'HYDRO_SD_Q' = 'sd_q')

write_csv(basin_char_clean, '04_analysis/out/basin_char_calc_clean.csv')


