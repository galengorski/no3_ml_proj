#===================================================================================#
# NOTES:                                                                            #
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-05-10                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
library(mapview)
library(leaflet)
library(leaflet.providers)
library(ggplot2)
library(ggridges)
#####
#===================================================================================#

site_info <- read_csv('04_analysis/out/basin_char_w_clusters_6.csv')

pal <- colorBin("Spectral", site_info$cluster, 8, pretty = TRUE, reverse = TRUE)

leaflet(site_info) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(-84.699, 37.5, zoom=5) %>%
  addCircleMarkers(lng = ~long, lat = ~lat,
                   fillColor = ~pal(cluster), stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 1,
                   radius = 6, color = 'black', label = site_info$station_nm)

site_info %>%
  dplyr::select(mean_no3, cq_slope, NLCD_AG,DTW,UNSAT_TT,NO3_DOM, NO3_PUB,TOT_BFI, cluster) %>%
  pivot_longer(cols = mean_no3:TOT_BFI) %>%
  mutate(cluster = as.factor(cluster)) %>%
  mutate(value = as.numeric(value)) %>%
  ggplot(aes(y = cluster)) +
  geom_density_ridges(
    aes(x = value, fill = cluster),
    alpha = 0.6, color = 'black'
  )+
  scale_fill_manual(values = c('#5E4FA2','#66C2A5','#E6F598','#FDAE61','#D53E4F','#9E0142'))+
  labs(
    x = "Attribute value",
    y = "Cluster Number",
    title = "Cluster attributes",
    subtitle = "47 sites",
    caption = ""
  ) +
  coord_cartesian(clip = "off")+
  theme_ridges(grid = FALSE)+
  facet_wrap(~name, scales = 'free_x')


site_info$cluster %>% table()


site_info %>%
  dplyr::select(mean_no3, NLCD_AG,DTW,UNSAT_TT,NO3_DOM, NO3_PUB,TOT_BFI, cluster) %>%
  pivot_longer(cols = mean_no3:TOT_BFI) %>%
  mutate(cluster = as.factor(cluster)) %>%
  mutate(value = as.numeric(value)) %>%
  ggplot(aes(y = cluster)) +
  geom_density_ridges(
    aes(x = value, fill = cluster),
    alpha = 0.6, color = 'black'
  )+
  scale_fill_manual(values = c('#5E4FA2','#66C2A5','#E6F598','#FDAE61','#D53E4F','#9E0142'))+
  labs(
    x = "Attribute value",
    y = "Cluster Number",
    title = "Cluster attributes",
    subtitle = "47 sites",
    caption = ""
  ) +
  coord_cartesian(clip = "off")+
  theme_ridges(grid = FALSE)+
  facet_wrap(~name, scales = 'free_x')


site_info_runs <- merge(site_info, run02, by.x = 'site_no', by.y = 'Site_number')

site_info_runs %>%
  mutate(cluster = as.factor(cluster)) %>%
  ggplot(aes(x = cq_slope, y = RMSE_validation, color = cluster)) +
  geom_point()+
  ylim(0,10)
  
  
