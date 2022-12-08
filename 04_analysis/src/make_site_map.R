#===================================================================================#
# NOTES: make a map of site locations 
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
library(leaflet)
library(leaflet.providers)
library(mapview)
library(Hmisc)
library(tigris)
#####
#===================================================================================#



drop_sites <- c('03340900','06899900')
#basin_char <- read_csv('02_munge/out/basin_char_full.csv')
site_list <- read_csv('01_fetch/out/site_list_220507.csv')
hydro_filled <- read_csv('01_fetch/out/hydro_filled_220128.csv')

n_summary <- hydro_filled %>%
  group_by(site_no) %>%
  summarise(mean_no3 = mean(nitrate, na.rm = TRUE), sd_no3 = sd(nitrate, na.rm = TRUE), 
            mean_q = mean(discharge), sd_q = sd(discharge), nobs = sum(is.na(nitrate)))


site_list_n <- merge(site_list[,c('site_no','station_nm', 'dec_lat_va','dec_long_va')], n_summary, by = 'site_no') %>%
  filter(site_no %nin% drop_sites)


pal <- colorNumeric("Reds", domain = NULL)
m <- leaflet(site_list_n) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(-84.699, 37.5, zoom=5) %>%
  addCircleMarkers(lng = ~dec_long_va, lat = ~dec_lat_va,
                   fillColor = ~pal(nobs), stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 0.70,
                   radius = 6, color = 'black', label = paste(site_list_n$station_nm, round(site_list_n$nobs, 2), sep = ' | ')) %>%
  addLegend('bottomright', pal = pal, values = ~site_list_n$nobs, title = 'Number of nitrate </br>observations')
m
mapshot(m, file = "04_analysis/figs/site_map_nobs.jpeg", remove_controls = TRUE)


###
#read in model results
ssr <- read_csv('03_model/out/single_site/Run_02/Rep_00/AllSitesModelResults.csv')
ms <- read_csv('03_model/out/multi_site/Run_14/Rep_00/AllSitesModelResults.csv')
msr <- read_csv('03_model/out/multi_site/Run_24/Rep_00/AllSitesModelResults.csv')

ss['Site_name'] <- ms['Site_name']
ss['run_short'] <- '46 Single-Site Models'
ssr['run_short'] <- '46 Single-Site Models 7-day Window'

ms['run_short'] <- '1 Multi-Site Model'
msr['run_short'] <- '1 Multi-Site Model 7-day Window'
runs <- rbind(ssr, msr)

sites_perf <- merge(site_list_n, runs, by.x = 'site_no', by.y = 'Site_number', all.x = TRUE, all.y = TRUE)


pal <- colorNumeric("Reds", domain = NULL)
to_plot <- sites_perf %>%
  filter(run_short == '1 Multi-Site Model 7-day Window') %>%
  filter(NSE_validation > -10)
  
m <-leaflet(to_plot) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(-84.699, 37.5, zoom=5) %>%
  addCircleMarkers(lng = ~dec_long_va, lat = ~dec_lat_va,
                   fillColor = ~pal(NSE_validation), stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 0.70,
                   radius = 6, color = 'black', label = paste(to_plot$station_nm, round(to_plot$NSE_validation, 2), sep = ' | ')) %>%
  addLegend('bottomright', pal = pal, values = ~to_plot$NSE_validation, title = 'NSE Validation')
m


sites_perf %>%
filter(NSE_validation > -10) %>%
ggplot(aes(x = mean_q, y = NRMSE_validation)) +
  geom_point()+
  facet_wrap(.~run_short)+
  coord_trans(x = 'log10')


#plot the best model in a map

best_models <- sites_perf %>%
  #filter(run_short != '1 Multi-Site Model 7-day Window') %>%
  group_by(site_no) %>%
  arrange(NRMSE_validation, .by_group = TRUE) %>%
  dplyr::summarise(min_RMSE_valid = min(RMSE_validation), model = first(run_short), 
            station_nm = first(station_nm), lat = first(dec_lat_va), long = first(dec_long_va))

ss_pal <- colorNumeric(colorRamp(c("#9e0e36", "#f2c4cd"), interpolate = "spline"), domain = c(0,6))
ms_pal <- colorNumeric(colorRamp(c("#080acf", "#acdce6"), interpolate = "spline"), domain = c(0.7,3))
ss_pal <- colorBin("Blues", ss_plot$min_RMSE_valid, 5, pretty = TRUE, reverse = TRUE)
ms_pal <- colorBin("Reds", ms_plot$min_RMSE_valid, 5, pretty = TRUE, reverse = TRUE)

ss_plot <- best_models %>%
  filter(model == '46 Single-Site Models 7-day Window')
ms_plot <- best_models %>%
  filter(model == '1 Multi-Site Model 7-day Window')

m <-leaflet(ss_plot) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(-84.699, 37.5, zoom=5) %>%
  addCircleMarkers(lng = ~long, lat = ~lat,
                   fillColor = ~ss_pal(min_RMSE_valid), stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 1,
                   radius = 6, color = 'black', label = paste(ss_plot$station_nm, round(ss_plot$min_RMSE_valid, 2), sep = ' | ')) %>%
  addCircleMarkers(lng = ms_plot$long, lat = ms_plot$lat,
                   fillColor = ms_pal(ms_plot$min_RMSE_valid), stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 1,
                   radius = 6, color = 'black', label = paste(ms_plot$station_nm, round(ms_plot$min_RMSE_valid, 2), sep = ' | ')) %>%
  addLegend('bottomright', pal = ss_pal, values = ~ss_plot$min_RMSE_valid, title = 'Single Site </br> RMSE </br> [NO3-N] </br> (mg/L)') %>%
  addLegend('bottomright', pal = ms_pal, values = ~ms_plot$min_RMSE_valid, title = 'Multi Site </br> RMSE </br> [NO3-N] </br> (mg/L)')
m
mapshot(m, file = "04_analysis/figs/RMSE_Performance_Map.jpeg", remove_controls = TRUE)




#sites_perf$RMSE_validation/sites_perf$NRMSE_validation
#library(ModelMetrics)
#test <- read_csv('03_model/out/multi_site/Run_22/Rep_00/01408500/ModelResults.csv')
#rmse(test[test$`Training/Validation` == 'Validation',]$Predicted, test[test$`Training/Validation` == 'Validation',]$Labeled)/
#mean(test[test$`Training/Validation` == 'Validation',]$Labeled)
#sites_perf[sites_perf$site_no == '01408500',]

hydro_filled$site_no %>% unique()

hydro_filled_test <- hydro_filled[hydro_filled$site_no == '01493112',]
ms_test <- read_csv('03_model/out/multi_site/Run_24/Rep_00/01493112/ModelResults.csv')
ss_test <- read_csv('03_model/out/single_site/Run_00/Rep_00/01493112/ModelResults.csv')

mean(ms_test[ms_test$`Training/Validation` == 'Validation',]$Labeled)
mean(ss_test[ss_test$`Training/Validation` == 'Validation',]$Labeled)


plot(ms_test$DateTime, ms_test$Labeled, typ = 'l', col = 'blue')
lines(ss_test$DateTime, ss_test$Labeled, type = 'l', col = 'red')
lines(hydro_filled_test$Date, hydro_filled_test$nitrate, type = 'l', col = 'black')


######
#Random forest on attributes


######
#map of best performing model
site_list_runs <- merge(site_list_n, runs, by.x = 'site_no', by.y = 'Site_number') 

best_models <- site_list_runs %>%
  group_by(site_no) %>%
  arrange(NSE_validation) %>%
  dplyr::summarize(site_no = first(site_no), station_nm = first(station_nm), lat = first(dec_lat_va),long = first(dec_long_va), nse_validation = last(NSE_validation), model = as.numeric(first(run_short)))

bins <- c(0,1,)
pal <- colorBin("Spectral", best_models$model, bins = c(0,5,7,45), pretty = TRUE, reverse = TRUE)

leaflet(best_models) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(-84.699, 37.5, zoom=5) %>%
  addCircleMarkers(lng = ~long, lat = ~lat,
                   fillColor = ~pal(model), stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 1,
                   radius = 6, color = 'black', label = paste(best_models$station_nm, round(best_models$nse_validation,2), best_models$model, sep = '|'))


runs %>%
  filter(cluster == )

