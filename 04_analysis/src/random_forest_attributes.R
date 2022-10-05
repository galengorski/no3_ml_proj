#===================================================================================#
# NOTES: this script uses the model metrics from the several nitrate runs and the watershed attributes to 
#investigate which attributes are important for determining model performance, we'll start with a random
#forest approach
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-05-05                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
install.packages('randomForest')
library(randomForest)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(RColorBrewer)
library(Hmisc)

#####
#===================================================================================#

drop_sites <- c('03340900','06899900')
basin_char <- read_csv('02_munge/out/basin_char_full.csv')
colnames(basin_char)[1] <- 'site_no'

attr <- basin_char %>%
  filter(site_no %nin% drop_sites) %>%
  mutate(NLCD_DEV = NLCD_21+NLCD_22+NLCD_23+NLCD_24, NLCD_FOR = NLCD_41+NLCD_42+NLCD_43, NLCD_AG = NLCD_81+NLCD_82) %>%
  mutate(fert_uN_mt_kmAg = fert_uN_mt/(NLCD_82*CAT_BASIN_AREA)) %>%
  dplyr::select(site_no, NLCD_DEV, NLCD_FOR, NLCD_AG, fert_uN_mt_kmAg, CAT_BASIN_AREA, CAT_SILTAVE, CAT_CLAYAVE, CAT_ROCKTYPE_200, CAT_ROCKTYPE_300, CAT_ROCKTYPE_400, CAT_ROCKTYPE_500, CAT_ROCKTYPE_600,CAT_LAKEPOND, CAT_STRM_DENS, CAT_SRL55AG, lat, long)
  

#site_list <- read_csv('01_fetch/out/site_list_220128.csv')
hydro_filled <- read_csv('01_fetch/out/hydro_filled_220128.csv')

n_summary <- hydro_filled %>%
  group_by(site_no) %>%
  summarise(mean_no3 = mean(nitrate, na.rm = TRUE), sd_no3 = sd(nitrate, na.rm = TRUE), mean_q = mean(discharge), sd_q = sd(discharge), nobs = sum(is.na(nitrate)))


site_list_n <- merge(attr, n_summary, by = 'site_no') %>%
  filter(site_no %nin% drop_sites)

#read in model results
ss <- read_csv('03_model/out/single_site/Run_02/Rep_00/AllSitesModelResults.csv')
ms <- read_csv('03_model/out/multi_site/Run_14/Rep_00/AllSitesModelResults.csv')
msr <- read_csv('03_model/out/multi_site/Run_24/Rep_00/AllSitesModelResults.csv')

ss['Site_name'] <- ms['Site_name']
ss['run_short'] <- '46 Single-Site Models'

ms['run_short'] <- '1 Multi-Site Model'
msr['run_short'] <- '1 Multi-Site Model 7-day Window'
runs <- rbind(ss, ms, msr)

sites_perf <- merge(site_list_n, msr, by.x = 'site_no', by.y = 'Site_number', all.x = TRUE, all.y = TRUE) %>% 
  as_tibble() %>%
  dplyr::select(NLCD_DEV:nobs,NRMSE_validation)


rf_ss_rmse <- randomForest(NRMSE_validation~., data = sites_perf, importance = T, ntree = 500, na.action=na.omit)
imp <- importance(rf_ss_rmse, type = 1, scale = FALSE)
imp_norm <- imp/max(imp)

varImpPlot(rf_ss_rmse)



plot(sites_perf$NLCD_FOR, sites_perf$NRMSE_validation)
lu <- basin_char %>% dplyr::select(NLCD_11:NLCD_82) %>% rowSums()
cbind(basin_char$site_no, lu)
