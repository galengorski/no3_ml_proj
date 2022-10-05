#===================================================================================#
# NOTES:                                                                            #
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-07-05                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('Hmisc')
library(Hmisc)
# install.packages('maps')
library(maps)
# install.packages('RColorBrewer')
library(RColorBrewer)
# install.packages(sf)
library(sf)
# install.packages('tidyverse')
library(tidyverse)
# install.packages('tmap')
library(tmap)
# install.packages('tmaptools')
library(tmaptools)
# install.packages('tigris')
library(tigris)
# install.packages('ggspatial')
library(ggspatial)
# install.packages("viridis")
library(viridis)
# install.packages("gridExtra")
library(gridExtra)
# install.packages("ggpubr")
library(ggpubr)
# install.packages('ModelMetrics')
library(ModelMetrics)
# install.packages('reshape2')
library(reshape2)
# install.packages('forcats')
library(forcats)
# install.packages('cowplot')
library(cowplot)

#####
#===================================================================================#

##############---------map of sites---------##############
#read in site data
site_info <- read_csv('04_analysis/out/basin_char_clean.csv') %>%
#  mutate(cluster_f = as.factor(cluster))
  #dplyr::select(site_no, lat, long, mean_no3)
#download state shapefiles
states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_runs_sf <- st_as_sf(site_info, coords = c('PHYS_LONG','PHYS_LAT'), crs = 4326)


ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_runs_sf, aes(fill = CHEM_MEAN_NO3), shape = 21, size = 3, alpha = 0.8) +
  scale_fill_gradient(low = '#ffffe5', high = '#cc4c02')+
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  guides(fill=guide_colourbar(title="Mean [NO3-N]\n(mg/L)"))+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white")
  )+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(-0.1, "in"), pad_y = unit(1.8, "in"),
    style = ggspatial::north_arrow_minimal(
      text_size = 8, line_width = .8
    )
  )

##############---------single site model results---------##############
#===================================================================================#
ss_site_runs_sf <- read_csv('04_analysis/out/single_site_ensemble_run_summary.csv')
site_runs_sf <- st_as_sf(ss_site_runs_sf, coords = c('long','lat'), crs = 4326)


ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_runs_sf, aes(fill = Testing_RMSE), shape = 21, size = 3, alpha = 0.75) +
  #scale_fill_steps2(high = '#084081', mid = '#7bccc4', low = '#f7fcf0', nice.breaks = TRUE)+
  scale_fill_viridis()+
  #scale_fill_steps()+
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  guides(fill=guide_colourbar(title="RMSE\n(mg/L)"))+
  ggtitle('Single-site model results')+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white")
  )+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(-0.1, "in"), pad_y = unit(1.8, "in"),
    style = ggspatial::north_arrow_minimal(
      text_size = 8, line_width = .8
    )
  )


##############---------Good and bad time series---------##############
#===================================================================================#
single_site <- read_csv('04_analysis/out/single_site_ensemble_run_summary.csv')
multi_site <- read_csv('04_analysis/out/multi_site_ensemble_full_run_summary.csv')

ss_ms <- merge(single_site, multi_site, by = 'site_no', suffixes = c('_ss','_ms'))

ss_ms$ss_minus_ms <- ss_ms$Testing_NSE_ss - ss_ms$Testing_NSE_ms


site <- '05482500'

rep <- c('Rep_00','Rep_01','Rep_02','Rep_03','Rep_04')
ss_ens <- read_csv(file.path('03_model/out/single_site/Run_00_Full/Rep_00', site,'ModelResults.csv'))
for(i in 2:length(rep)){
  temp_rep <- read_csv(file.path('03_model/out/single_site/Run_00_Full',rep[i], site,'ModelResults.csv'))
  ss_ens[paste0('Predicted_',i)] <- temp_rep$Predicted
}

good_ss <- ss_ens %>%
  relocate(Predicted, .after = `Train/Val/Test`) %>%
  rowwise() %>%
  mutate(Predicted_mean_ss = mean(c(Predicted, Predicted_2, Predicted_3, Predicted_4, Predicted_5))) %>%
  arrange(DateTime) %>%
  dplyr::select(!Predicted:Predicted_5)

ms_ens <- read_csv(file.path('03_model/out/multi_site/Run_03/Rep_00', site,'ModelResults.csv'))
for(i in 2:length(rep)){
  temp_rep <- read_csv(file.path('03_model/out/multi_site/Run_03',rep[i], site,'ModelResults.csv'))
  ms_ens[paste0('Predicted_',i)] <- temp_rep$Predicted
}

good_ss_ms <- ms_ens %>%
  relocate(Predicted, .after = `Train/Val/Test`) %>%
  rowwise() %>%
  mutate(Predicted_mean_ms = mean(c(Predicted, Predicted_2, Predicted_3, Predicted_4, Predicted_5))) %>%
  arrange(DateTime) %>%
  dplyr::select(!Predicted:Predicted_5)


# #single site model
# run06s <- read_csv('03_model/out/single_site/hyper_param_tune/HP_06/Rep_00/AllSitesModelResults.csv')
# run06s['cluster'] <- NA
# run06s <- run40s[-46,]
# 
# good <- read_csv('03_model/out/single_site/hyper_param_tune/HP_06/Rep_00/05524500/ModelResults.csv') %>% arrange(DateTime)
# medium <- read_csv('03_model/out/single_site/hyper_param_tune/HP_06/Rep_00/03336890/ModelResults.csv') %>% arrange(DateTime)
# bad <- read_csv('03_model/out/single_site/hyper_param_tune/HP_06/Rep_00/05482500/ModelResults.csv') %>% arrange(DateTime)


g_ss <- rbind(good_ss %>%
        dplyr::select(DateTime,Predicted_mean_ss,`Train/Val/Test`) %>%
        rename('Value' = 'Predicted_mean_ss', 'Set' = 'Train/Val/Test'),
      good_ss %>%
        dplyr::select(DateTime,Labeled) %>%
        mutate(Set = 'Observed') %>%
        rename('Value' = 'Labeled')
        ) %>%
  ggplot(aes(x = DateTime, y = Value, color = Set))+
  geom_line()+
  scale_color_manual(values = c('black','#ae2012','#0a9396'))+
  theme_bw()+
  ylab('[NO3-N] (mg/L)')+
  xlab('')+
  ggtitle(paste0('Single-site model | ',basin_char[basin_char$site_no == site,]$station_nm,' | ', site ,'\n NSE = ',
                 round(single_site[single_site$site_no == site,]$Testing_NSE, digits = 2)))+
  theme(legend.position="none")

g_ms <- rbind(good_ss_ms %>%
                dplyr::select(DateTime,Predicted_mean_ms,`Train/Val/Test`) %>%
                rename('Value' = 'Predicted_mean_ms', 'Set' = 'Train/Val/Test'),
              good_ss %>%
                dplyr::select(DateTime,Labeled) %>%
                mutate(Set = 'Observed') %>%
                rename('Value' = 'Labeled')
  ) %>%
    ggplot(aes(x = DateTime, y = Value, color = Set))+
    geom_line()+
    scale_color_manual(values = c('black','#ae2012','#0a9396'))+
    theme_bw()+
    ylab('[NO3-N] (mg/L)')+
    xlab('')+
    ggtitle(paste0('Multi-site model | ',basin_char[basin_char$site_no == site,]$station_nm,' | ', site ,'\n NSE = ',
                   round(multi_site[multi_site$site_no == site,]$Testing_NSE, digits = 2)))+
    theme(legend.position="none")

ggarrange(g_ss,g_ms,
          nrow = 2, align = 'hv')


ss_ms %>%
  #filter(run_short > 2) %>%
  pivot_longer(cols = c(Testing_NSE_ss, Testing_NSE_ms)) %>%
  dplyr::select(name, value) %>%
  mutate(runfct = as.factor(name)) %>%
  mutate(NSE = as.numeric(value)) %>%
  ggplot(aes(y = runfct)) +
  geom_density_ridges(
    aes(x = NSE, fill = paste(runfct, name)),
    alpha = 0.6, color = 'black'
  )+
  labs(
    x = "NSe (mg/L)",
    y = "Model Type",
    title = "Model performance",
    subtitle = "46 sites",
    #caption = "seq_len = 365; lr = 0.005; owd = 0.001; bs = 512, "
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  #scale_fill_cyclical(
  #  breaks = c("1 Multi-Site Model RMSE_training", "1 Multi-Site Model RMSE_validation"),
  #  labels = c(`1 Multi-Site Model RMSE_training` = "Training", `1 Multi-Site Model RMSE_validation` = "Validation"),
  #  values = c("#ae2012", "#0a9396"),
  #  name = " ", guide = "legend"
  #)+
  coord_cartesian(clip = "off")+
  theme_ridges(grid = FALSE)+
  xlim(0,1)

g_xplot <- good %>%
  ggplot(aes(x = Labeled, y = Predicted, fill = `Training/Validation`))+
  geom_abline(slope = 1, intercept = 0, col = 'black', linetype = 2, size = 1.2)+
  geom_point(shape = 21, size = 1.4, alpha = 0.6)+
  ylab('Predicted')+
  xlab('Observed')+
  theme_bw()+
  ylim(0,12)+
  xlim(0,12)+
  theme(legend.position="none", plot.title = element_text(colour = "#0a9396"))+
  ggtitle(paste0('RMSE: ',
                 round(rmse(good[good$`Training/Validation` == 'Validation',]$Predicted, 
                            good[good$`Training/Validation` == 'Validation',]$Labeled),digits = 2)))


m <-rbind(medium %>%
            dplyr::select(DateTime,Predicted,`Training/Validation`) %>%
            rename('Value' = 'Predicted', 'Set' = 'Training/Validation'),
          medium %>%
            dplyr::select(DateTime,Labeled) %>%
            mutate(Set = 'Observed') %>%
            rename('Value' = 'Labeled')
            ) %>%
  ggplot(aes(x = DateTime, y = Value, color = Set))+
  geom_line()+
  scale_color_manual(values = c('black','#ae2012','#0a9396'))+
  theme_bw()+
  ylab('[NO3-N] (mg/L)')+
  xlab('')+
  ggtitle('Spoon River near St. Joseph, IL | 03336890')+
  theme(legend.position="none")

m_xplot <- medium %>%
  ggplot(aes(x = Labeled, y = Predicted, fill = `Training/Validation`))+
  geom_abline(slope = 1, intercept = 0, col = 'black', linetype = 2, size = 1.2)+
  geom_point(shape = 21, size = 1.4, alpha = 0.6)+
  ylab('Predicted')+
  xlab('Observed')+
  ylim(0,14)+
  xlim(0,14)+
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(colour = "#0a9396"))+
  ggtitle(paste0('RMSE: ',
                 round(rmse(medium[medium$`Training/Validation` == 'Validation',]$Predicted, 
                            medium[medium$`Training/Validation` == 'Validation',]$Labeled),digits = 2)))


b <-rbind(bad %>%
            dplyr::select(DateTime,Predicted,`Training/Validation`) %>%
            rename('Value' = 'Predicted', 'Set' = 'Training/Validation'),
          bad %>%
            dplyr::select(DateTime,Labeled) %>%
            mutate(Set = 'Observed') %>%
            rename('Value' = 'Labeled')
            ) %>%
  ggplot(aes(x = DateTime, y = Value, color = Set))+
  geom_line()+
  scale_color_manual(values = c('black','#ae2012','#0a9396'))+
  theme_bw()+
  ylab('[NO3-N] (mg/L)')+
  xlab('')+
  ggtitle('North Raccoon River near Jefferson, IA | 05482500')+
  theme(legend.position="none")

b_xplot <- bad %>%
  ggplot(aes(x = Labeled, y = Predicted, fill = `Training/Validation`))+
  geom_abline(slope = 1, intercept = 0, col = 'black', linetype = 2, size = 1.2)+
  geom_point(shape = 21, size = 1.4, alpha = 0.6)+
  ylab('Predicted')+
  xlab('Observed')+
  ylim(0,30)+
  xlim(0,30)+
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(colour = "#0a9396"))+
  ggtitle(paste0('RMSE: ',
                 round(rmse(bad[bad$`Training/Validation` == 'Validation',]$Predicted, bad[bad$`Training/Validation` == 'Validation',]$Labeled),digits = 2)))
  
#975x538 looks pretty good
ggarrange(g,g_xplot,
          m,m_xplot, 
          b,b_xplot,
             nrow = 3, ncol = 2, widths= c(3,1), align = 'hv')
#===================================================================================#

##############---------Heatmap of attributes---------##############
#===================================================================================#


site_runs_ss <- merge(site_info, run04s, by.x = 'site_no', by.y = 'Site_number', all.x = FALSE, all.y = FALSE) %>%
  filter(run_short == 'Single-site')


basin_char_runs_clean <- site_runs_ss
colnames(basin_char_runs_clean) <- gsub('TOT_','',colnames(basin_char_runs_clean))
colnames(basin_char_runs_clean) <- gsub('NLCD_','',colnames(basin_char_runs_clean))

basin_char_runs_clean <- basin_char_runs_clean %>%
  dplyr::select(run_short, BASIN_AREA:cq_slope, RMSE_validation, NRMSE_validation, NSE_validation, PBIAS_validation) %>%
  dplyr::select(!HGAC) %>%
  rename_at(vars(BASIN_AREA:BASIN_SLOPE), function(x) paste0('PHYS_',x))%>%
  rename_at(vars(HGA:HGD, SRL55AG), function(x) paste0('SOIL_',x)) %>%
  rename('PHYS_LAT' = 'lat', 'PHYS_LONG' = 'long') %>%
  rename_at(vars(CANALDITCH,ARTIFICIAL,DITCHES92,TILES92,NPDES_MAJ,NPDES_MAJ_DENS,RESERVOIR,NORM_STORAGE2013), function(x) paste0('ANTHRO_',x))%>%
  rename_at(vars(N97), function(x) paste0('CHEM_',x)) %>%
  rename('CHEM_FERT_N' = 'fert_uN_mt_sqkm') %>%
  rename_at(vars(DEV:WTLND, LAKEPOND), function(x) paste0('LULC_',x)) %>%
  rename_at(vars(DTW:NO3_PUB), function(x) paste0("GW_",x)) %>%
  rename_at(vars(BFI:WB5100_ANN), function(x) paste0("HYDRO_",x)) %>%
  rename_at(vars(CWD:PPT7100_ANN), function(x) paste0("CLIMATE_",x)) %>%
  rename('CHEM_MEAN_NO3' = 'mean_no3','CHEM_SD_NO3' = 'sd_no3','CHEM_CQ_SLOPE' = 'cq_slope') %>% 
  rename('HYDRO_MEAN_Q' = 'mean_q', 'HYDRO_SD_Q' = 'sd_q') %>%
  dplyr::select(-ANTHRO_DITCHES92, -ANTHRO_TILES92, -ANTHRO_NORM_STORAGE2013, 
                -ANTHRO_CANALDITCH, -ANTHRO_NPDES_MAJ_DENS, -CHEM_N97, -STREAM_SLOPE,
                -SOIL_SRL55AG)

basin_char_runs_clean %>%
  filter(run_short == 'Single-site') %>%
  mutate(APBIAS_validation = abs(PBIAS_validation)) %>%
  dplyr::select(-PBIAS_validation, -run_short) %>%
  cor() %>%
  melt() %>%
  as_tibble() %>%
  filter(Var1 == "RMSE_validation")%>%#,"RMSE_validation","NRMSE_training","NRMSE_validation",
                     #"NSE_training","NSE_validation","APBIAS_training" ,"APBIAS_validation")) %>%
  filter(Var2 %nin% c("RMSE_training","RMSE_validation","NRMSE_training","NRMSE_validation",
                      "NSE_training","NSE_validation","APBIAS_training" ,"APBIAS_validation")) %>%
  mutate(Var2 = as.character(Var2)) %>%
  mutate(Var2 = factor(Var2)) %>%
  mutate(Var2 = reorder(Var2, value)) %>%
  filter(abs(value) >0.3) %>%
  ggplot(aes(x = Var2, y = value, fill = value))+
  geom_bar(stat = 'identity',position = 'dodge')+
  #geom_tile(color = 'white')+
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000", limits = c(-1,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle('Correlation of single-site RMSE and\nbasin characteristics')+
  guides(fill=guide_colourbar(title="Pearson r"))+
  coord_flip()+
  theme_bw()+
  ylab('Pearson r')+
  xlab('')+
  ylim(-1,1)

##############---------Map of clusters---------##############
#===================================================================================#
basin_char <- read_csv('04_analysis/out/basin_char_calc_clean.csv')
basin_names <- read_csv('04_analysis/out/basin_char_w_clusters_10.csv') %>% dplyr::select(lat,long,station_nm)
basin_clusters <- read_csv('04_analysis/out/basin_char_w_clusters_220923.csv') %>% dplyr::select(site_no, cluster_02, hydro_terrane)

basin_char <- left_join(basin_char, basin_names, by = c('PHYS_LAT' = 'lat', 'PHYS_LONG' = 'long')) %>%
  left_join(basin_clusters, by = 'site_no') %>%
  mutate(cluster = factor(cluster_02))

states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_runs_sf <- st_as_sf(basin_char, coords = c('PHYS_LONG','PHYS_LAT'), crs = 4326)


map <- ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_runs_sf, aes(fill = cluster), shape = 21, size = 3.5, alpha = 0.9) +
  #scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  guides(fill=guide_legend(title="Cluster"))+
  theme(legend.position = 'left',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white")
  )+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(-0.1, "in"), pad_y = unit(1.8, "in"),
    style = ggspatial::north_arrow_minimal(
      text_size = 8, line_width = .8
    )
  )
map


##############---------Map of best model---------##############
#===================================================================================#
ss <- read_csv('04_analysis/out/single_site_ensemble_run_summary.csv') %>%
  mutate(cluster = NA, run = 'Single-site')
ms <- read_csv('04_analysis/out/multi_site_ensemble_full_run_summary.csv') %>%
  mutate(cluster = NA, run = 'Multi-site')
# cl1 <- read_csv('04_analysis/out/cluster_01_ensemble_full_run_summary.csv') %>%
#   mutate(run = 'Clustered_01')
cl2 <- read_csv('04_analysis/out/cluster_02_ensemble_full_run_summary.csv') %>%
  mutate(run = 'Clustered')
ht <- read_csv('04_analysis/out/hydro_terrane_ensemble_full_run_summary.csv') %>%
  mutate(run = 'Hydro terrane')

all_models <- rbind(ss, ms, cl2, ht) %>%
  filter(run != 'Clustered_01') %>%
  mutate(run = factor(run, levels = c("Single-site","Clustered","Hydro terrane","Multi-site")))


best_models <- all_models %>%
  group_by(site_no) %>%
  arrange(Testing_RMSE, .by_group = TRUE) %>%
  summarise(site_no = first(site_no), RMSE = first(Testing_RMSE), run = first(run), cluster = first(cluster),
            lat = first(lat), long = first(long))
  
  

states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_runs_sf <- st_as_sf(best_models, coords = c('long','lat'), crs = 4326)


map <- ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_runs_sf, aes(fill = run), shape = 21, size = 3, alpha = 0.9) +
  scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  guides(fill=guide_legend(title="Best performing model\nat each site"))+
  theme(legend.position = 'left',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white")
  )+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(-0.1, "in"), pad_y = unit(1.8, "in"),
    style = ggspatial::north_arrow_minimal(
      text_size = 8, line_width = .8
    )
  )

  
dist <- all_models %>%
  mutate(models = factor(run, levels = rev(c("Single-site","Clustered","Hydro terrane","Multi-site")))) %>%
  ggplot(aes(x = models, y = Testing_RMSE, fill = models))+
  geom_boxplot(alpha = 0.9)+
  scale_fill_manual(values = rev(c('#00bbf9','#ff595e','#ffca3a','#00f5d4')))+
  scale_color_manual(values = rev(c('#00bbf9','#ff595e','#ffca3a','#00f5d4')))+
  guides(title = "Model")+
  theme_bw()+
  ggtitle('Model performance at all sites')+
  #geom_jitter(aes(fill=run), size=1.5, alpha=0.7, shape = 21, color = 'black')+
  coord_flip()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = 'none')+
  xlab('')+
  ylab('RMSE (mg/L)')
#dist

plot_grid(map,dist, rel_heights = c(3,2), nrow = 2, align = 'v', axis = 'l', labels = 'AUTO')
ggsave('04_analysis/figs/best_models_map_dist.jpg',height = 6, width = 8)

#summary table of four models
all_models %>%
  group_by(run) %>%
  summarise(med_RMSE = median(Testing_RMSE), med_NSE = median(Testing_NSE), med_PBIAS = median(Testing_PBIAS),
            med_r = median(Testing_r), med_KGE = median(Testing_KGE))

#a table for which models perform best for which sites
#
a <- cbind(
  #RMSE
  all_models %>%
    group_by(site_no) %>%
    arrange(Testing_RMSE, .by_group = TRUE) %>%
    summarise(run = first(run)) %>%
    pull(run) %>%
    table() %>%
    as_tibble() %>%
    pull(n),
  #NSE
  all_models %>%
    group_by(site_no) %>%
    arrange(desc(Testing_NSE), .by_group = TRUE) %>%
    summarise(run = first(run)) %>%
    pull(run) %>%
    table() %>%
    as_tibble() %>%
    pull(n),
  #PBIAS
  all_models %>%
    group_by(site_no) %>%
    arrange(abs(Testing_PBIAS), .by_group = TRUE) %>%
    summarise(run = first(run)) %>%
    pull(run) %>%
    table() %>%
    as_tibble() %>%
    pull(n),
  #r
  all_models %>%
    group_by(site_no) %>%
    arrange(desc(Testing_r), .by_group = TRUE) %>%
    summarise(run = first(run)) %>%
    pull(run) %>%
    table() %>%
    as_tibble() %>%
    pull(n),
  #KGE
  all_models %>%
    group_by(site_no) %>%
    arrange(desc(Testing_KGE), .by_group = TRUE) %>%
    summarise(run = first(run)) %>%
    pull(run) %>%
    table() %>%
    as_tibble() %>%
    pull(n)
) %>%

colnames(a) <- c('RMSE','NSE','PBIAS','r','KGE')
rownames(a) <- c('Single-site','Clustered','Hydro terrane','Multi-site')  
  

#performance metric by cluster or hydroterrane
all_models %>%
  group_by(hydro_terrane, run) %>%
  summarise(med_NSE = median(Testing_NSE)) %>%
  pivot_wider(names_from = 'run', values_from = 'med_NSE')

