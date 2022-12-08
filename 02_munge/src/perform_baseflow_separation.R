#===================================================================================#
# NOTES: use baseflow sepearation to get baseflow and stormflow for each site
# using the ecohydrology package
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2021-10-18                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
# install.packages('EcoHydRology')
library(EcoHydRology)
library(magrittr)
#####
#===================================================================================#

#read input data
input_data <- read_csv('01_fetch/out/hydro_filled_220128.csv')
sites <- unique(input_data$site_no)

data_bfs <- data.frame()

for(i in 1:length(sites)){
  #subset the site
  temp <- input_data %>%
    filter(site_no == sites[i])
  #perform the hydrograph sep
  temp_bfs <- BaseflowSeparation(temp$discharge, filter_parameter = 0.925, passes = 3)
  
  #plotting functionaliy
  #h <- hydrograph(streamflow = temp$discharge, timeSeries = as.Date(temp$Date), streamflow2 = temp_bfs[,1], precip = temp$prcp)
  #print(h)
  #bind new columns to old and rename
  temp <- cbind(temp, temp_bfs) %>%
    mutate(baseflow = bt, quickflow = qft)
  #select columns
  temp <- temp %>%
    select(site_no, Date, discharge, baseflow, quickflow, nitrate, discharge_interp)
  data_bfs <- rbind(data_bfs, temp)
}

write.csv(data_bfs, '02_munge/out/all_sites_data_bfs.csv')
