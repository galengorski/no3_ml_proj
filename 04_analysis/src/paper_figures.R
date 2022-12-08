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
site_info <- read_csv('04_analysis/out/basin_char_calc_clean.csv')# %>%
#  mutate(cluster_f = as.factor(cluster))
  #dplyr::select(site_no, lat, long, mean_no3)
#download state shapefiles
states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_runs_sf <- st_as_sf(site_info, coords = c('PHYS_LONG','PHYS_LAT'), crs = 4326)


mean_no3_map <- ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_runs_sf, aes(fill = CHEM_MEAN_NO3), shape = 21, size = 5, alpha = 0.8) +
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
  )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

all_data <- read_csv('02_munge/out/all_sites_data_bfs.csv')

n_obs_df <- all_data %>%
  filter(nitrate >= 0) %>%
  group_by(site_no) %>%
  summarise(n_obs = n())

n_obs_plot <- n_obs_df %>%
  arrange(n_obs) %>%
  mutate(index = seq(1,48)) %>%
  ggplot(aes(x = n_obs, y = index)) +
  geom_line(size = 2.5, color = '#ca6702')+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  ylab('Number of sites')+
  xlab('Number of observations')+
  geom_vline(aes(xintercept = median(n_obs_df$n_obs)), color = 'black', linetype="dashed", size=1)+
  geom_text(x = 2500, y = 15, label = "50% of sites have\n>1500 observations", size = 6)

no3_median <- all_data %>%
  filter(nitrate >= 0) %>%
  ggplot(aes(x = nitrate)) +
  geom_histogram(color = 'black', fill = '#ca6702', alpha = 0.85)+  
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  ylab('Number of observation')+
  xlab('[NO3-N] (mg/L)')

lulc_sites <- site_info %>%
  select(starts_with('LULC'))%>% 
  select(!LULC_LAKEPOND) %>%
  pivot_longer(cols = LULC_DEV:LULC_WTLND) %>%
  mutate(name = factor(name, levels = rev(c('LULC_AG','LULC_DEV','LULC_FOR','LULC_WTLND')), 
                       labels = rev(c('Agriculture','Developed','Forested','Wetlands')))) %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_jitter(position=position_jitter(0.25), shape=21, size = 4, alpha = 0.85)+
  scale_fill_manual(values = rev(c('#f4a261','firebrick','#adc178','lightblue')))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = 'none')+
  ylab('Fraction of contributing area')+
  xlab('Landuse')+
  coord_flip()
  
  

data_plots <- plot_grid(n_obs_plot, no3_median, lulc_sites, ncol = 1, #labels = c('B','C','D'), 
                      label_size = 20, hjust = -2.3, align = 'v', axis = 'l')
#jpeg('04_analysis/figs/site_map_with_data_coverage.jpeg', height = 600, width = 1200)
#plot_grid(mean_no3_map, data_plots, ncol = 2, rel_widths = c(4,3))#, labels = 'AUTO', align = 'l', label_size = 20)
#dev.off()

plot_grid(mean_no3_map, data_plots, ncol = 2, rel_widths = c(4,3), align = 'v')
ggsave('04_analysis/figs/site_map_with_data_coverage.jpeg',height = 7, width = 14, dpi = 500)


##############---------single site model results---------##############
#===================================================================================#
ss_site_runs_sf <- read_csv('04_analysis/out/single_site_ensemble_run_summary.csv')
site_runs_sf <- st_as_sf(ss_site_runs_sf, coords = c('long','lat'), crs = 4326)


ss_perf_plot <- ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_runs_sf, aes(fill = Testing_NRMSE), shape = 21, size = 5, alpha = 0.75) +
  #scale_fill_steps2(high = '#084081', mid = '#7bccc4', low = '#f7fcf0', nice.breaks = TRUE)+
  #RMSE
  #scale_fill_viridis(option = 'C', direction = -1)+
  #NRMSE
  scale_fill_viridis(option = 'C', direction = -1, limits = c(23,100), na.value = '#5a189a')+
  #NSE
  #scale_fill_viridis(limits = c(-0.2, 1), direction = 1, option = 'C', na.value = '#5a189a')+
  #scale_fill_steps()+
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  #guides(fill=guide_colourbar(title="RMSE\n(mg/L)"))+
  #guides(fill=guide_colourbar(title="NSE"))+
  guides(fill=guide_colourbar(title="Normalized\nRMSE"))+
  #ggtitle('Single-site model results')+
  ggspatial::annotation_scale(
    text_cex = 1.2,
    location = "tr",
    bar_cols = c("grey60", "white")
  )+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(-0.1, "in"), pad_y = unit(1.8, "in"),
    style = ggspatial::north_arrow_minimal(
      text_size = 10, line_width = .8
    )
  )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

#jpeg('04_analysis/figs/ss_nrmse_map.jpeg', height = 400, width = 600)
ss_perf_plot
ggsave('04_analysis/figs/ss_nrmse_map.jpeg', height = 6, width = 8, dpi = 500)

#dev.off()
colors <- viridis(15, option = 'C')
ss_clean <- ss_site_runs_sf %>%
  filter(Testing_NSE > -0.5)

ss_clean$color <- colourvalues::colour_values(ss_clean$Testing_NSE, palette = 'magma')

hist(ss_clean$Testing_NSE)

hist(ss_clean$Testing_NSE, breaks = seq(-13,1,0.05), xlim = c(-0.5,1))#, col = ss_clean$color)

ss_clean %>%
  ggplot(aes(x=Testing_NSE, color = Testing_NRMSE)) +
  geom_histogram(color = 'white')+
  theme_bw()+
  scale_fill_viridis()


nrmse <- ss_clean %>%
  arrange(Testing_NRMSE) %>%
  mutate(index = seq(0,1,length.out = 45)) %>%
  ggplot(aes(y = index, x = Testing_NRMSE)) +
  geom_vline(xintercept = median(ss_site_runs_sf$Testing_NRMSE), color = 'red', linetype = 'dashed')+
  geom_line(size = 1.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16))+
  ylab('Fraction of sites')+
  xlab('Normalized RMSE')+
  ggtitle('Normalized RMSE')
  
rmse <- ss_clean %>%
  arrange(Testing_RMSE) %>%
  mutate(index = seq(0,1,length.out = 45)) %>%
  ggplot(aes(y = index, x = Testing_RMSE)) +
  geom_vline(xintercept = median(ss_site_runs_sf$Testing_RMSE), color = 'red', linetype = 'dashed')+
  geom_line(size = 1.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16))+
  ylab('Fraction of sites')+
  xlab('RMSE (mg/L)')+
  ggtitle('RMSE')

nse <- ss_clean %>%
  arrange(Testing_NSE) %>%
  mutate(index = seq(0,1,length.out = 45)) %>%
  ggplot(aes(y = index, x = Testing_NSE)) +
  geom_vline(xintercept = median(ss_site_runs_sf$Testing_NSE), color = 'red', linetype = 'dashed')+
  geom_line(size = 1.2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16))+
  ylab('Fraction of sites')+
  xlab('NSE')+
  ggtitle('NSE')
  
plot_grid(nrmse, rmse, nse, ncol = 1)
ggsave('04_analysis/figs/ss_metrics_cdf.jpg', height = 9, width = 4, dpi = 300)


##############---------Good and bad time series---------##############
#===================================================================================#
single_site <- read_csv('04_analysis/out/single_site_ensemble_run_summary.csv')
multi_site <- read_csv('04_analysis/out/multi_site_ensemble_full_run_summary.csv')


cl <- read_csv('04_analysis/out/cluster_ensemble_full_run_summary.csv') %>%
  mutate(hydro_terrane = NA, run = 'Clustered') %>%
  rename('lat' = 'PHYS_LAT','long'= 'PHYS_LONG')
ss <- read_csv('04_analysis/out/single_site_ensemble_run_summary.csv') %>%
  mutate(hydro_terrane = NA, cluster = cl$cluster, run = 'Single-site')
ms <- read_csv('04_analysis/out/multi_site_ensemble_full_run_summary.csv') %>%
  mutate(hydro_terrane = NA, cluster = cl$cluster, run = 'Multi-site')
ht <- read_csv('04_analysis/out/hydro_terrane_ensemble_full_run_summary.csv') %>%
  mutate(hydro_terrane = cluster, cluster = cl$cluster, run = 'Hydro terrane') %>%
  rename('lat' = 'PHYS_LAT','long'= 'PHYS_LONG')
cl$hydro_terrane <- ht$hydro_terrane
ss$hydro_terrane <- ht$hydro_terrane
ms$hydro_terrane <- ht$hydro_terrane


ss_ms <- merge(ss, ms, by = 'site_no', suffixes = c('_ss','_ms'))

ss_ms <- ss_ms %>%
  as_tibble() %>%
  mutate(ss_minus_ms = Testing_RMSE_ss - Testing_RMSE_ms)

ss_ms %>%
  ggplot(aes(x = Testing_RMSE_ss, y = ss_minus_ms, fill = cluster_ss))+
  geom_point(shape = 21, color = 'black', size = 3)+
  theme_bw()+
  #ylim(-1,1)+
  #xlim(-1,1)+
  ggtitle('Improvement from single-site to multi-site\n <0 indicates worse performance in multi-site model')+
  xlab('Single site RMSE (mg/L)')+
  ylab('Difference in RMSE (mg/L)')

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
  theme(legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

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
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

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
  theme(legend.position="none", plot.title = element_text(colour = "#0a9396"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
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
  theme(legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

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
  theme(legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

b_xplot <- bad %>%
  ggplot(aes(x = Labeled, y = Predicted, fill = `Training/Validation`))+
  geom_abline(slope = 1, intercept = 0, col = 'black', linetype = 2, size = 1.2)+
  geom_point(shape = 21, size = 1.4, alpha = 0.6)+
  ylab('Predicted')+
  xlab('Observed')+
  ylim(0,30)+
  xlim(0,30)+
  theme_bw()+
  theme(legend.position="none", plot.title = element_text(colour = "#0a9396"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
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
library(viridis)
library(rstatix)

#cleaned up names
names_lookup <- read_csv('04_analysis/out/basin_char_names_lookup.csv')

#read in the basin characteristics which include all static characteristics including those
#that were not included in modeling and those that were calculated like cq slope
basin_char_clean <- read_csv('04_analysis/out/basin_char_calc_clean.csv') %>%
  dplyr::select(`...1`,names_lookup[!is.na(names_lookup$Used_Modeling),]$Names_Clean)


ss <- read_csv('04_analysis/out/single_site_ensemble_run_summary.csv')

basin_char_num <- merge(basin_char_clean, ss[,c('site_no','Testing_RMSE','Testing_NRMSE','Testing_NSE','Testing_KGE','Testing_r')]) %>%
                            dplyr::select(-c(site_no, ...1)) %>%
  as_tibble()

cor_test <- cor(basin_char_num)[,c('Testing_RMSE','Testing_NRMSE','Testing_NSE','Testing_KGE','Testing_r')]
cor_test <- cor_test[order(row.names(cor_test)), ]
#remove correlations between performance metrics
cor_test <- cor_test[1:55,]
#cor_test[abs(cor_test) < 0.25] <- NA

cor_test_tib <- cor_test %>%
     as_tibble() %>%
     mutate(var_names = rownames(cor_test)) %>%
  mutate(color = if_else(Testing_NRMSE>0,"neg","pos"))


cols <- c(rep('#9a031e',3),rep('#127475',2),rep('#7161ef',2),
          rep('#52b69a',2),rep('#fb8b24',4),rep('#184e77',1),
          rep('#5f0f40',5))

cor_test_plot <- cor_test_tib %>%
  arrange(desc(abs(Testing_NRMSE))) %>% 
  slice(1:20) %>% 
  filter(var_names != "ANTHRO_TILES92") %>%
  #filter(Testing_NRMSE_cor > 0.25) %>%
  dplyr::select(var_names, Testing_NRMSE, color) %>%
  #add_row(Testing_NRMSE = c(0,0), var_names = c('FILL SPACE','FILL SPACE 2')) %>%
  mutate(var_names = replace(var_names, var_names == 'ANTHRO_CANALDITCH', 'CANAL/DITCH')) %>%
  mutate(var_names = replace(var_names, var_names == 'ANTHRO_MAJOR2013', 'MAJOR DAMS')) %>%
  mutate(var_names = replace(var_names, var_names == 'ANTHRO_NID_STORAGE2013', 'MAXIMUM RESERVOIR STORAGE')) %>%
  mutate(var_names = replace(var_names, var_names == 'ANTHRO_NORM_STORAGE2013', 'NORMAL RESERVOIR STORAGE')) %>%
  mutate(var_names = replace(var_names, var_names == 'ANTHRO_TILE_DRAIN', 'TILE DRAINS')) %>%
  mutate(var_names = replace(var_names, var_names == 'CHEM_FERT_N', 'FERTILIZER APPLIED')) %>%
  mutate(var_names = replace(var_names, var_names == 'GW_DTW', 'DEPTH TO GW')) %>%
  mutate(var_names = replace(var_names, var_names == 'GW_NO3_DOM', 'GW NO3 DOM')) %>%
  mutate(var_names = replace(var_names, var_names == 'GW_NO3_PUB', 'GW NO3 PUB')) %>%
  mutate(var_names = replace(var_names, var_names == 'GW_TRANSM', 'TRANSMISSVITY')) %>%
  mutate(var_names = replace(var_names, var_names == 'HYDRO_BFI', 'BASEFLOW INDEX')) %>%
  mutate(var_names = replace(var_names, var_names == 'HYDRO_RECHG', 'RECHARGE')) %>%
  mutate(var_names = replace(var_names, var_names == 'LULC_FOR', 'FOREST')) %>%
  mutate(var_names = replace(var_names, var_names == 'LULC_WTLND', 'WETLANDS')) %>%
  mutate(var_names = replace(var_names, var_names == 'PHYS_BASIN_SLOPE', 'BASIN SLOPE')) %>%
  mutate(var_names = replace(var_names, var_names == 'PHYS_STREAM_SLOPE', 'STREAM SLOPE')) %>%
  mutate(var_names = replace(var_names, var_names == 'SOIL_HGA', 'HIGH INFILTRATION')) %>%
  mutate(var_names = replace(var_names, var_names == 'SOIL_HGAD', 'HIGH INFILTRATION*')) %>%
  mutate(var_names = replace(var_names, var_names == 'SOIL_HGBD', 'MEDIUM INFILTRATION*')) %>%
  mutate(var_names = factor(var_names, levels = rev(c('MEDIUM INFILTRATION*','HIGH INFILTRATION*','HIGH INFILTRATION',
                                                  'STREAM SLOPE','BASIN SLOPE','WETLANDS','FOREST',
                                                  'RECHARGE','BASEFLOW INDEX',
                                                  'TRANSMISSVITY','GW NO3 PUB','GW NO3 DOM',
                                                  'DEPTH TO GW','FERTILIZER APPLIED','TILE DRAINS',
                                                  'NORMAL RESERVOIR STORAGE','MAXIMUM RESERVOIR STORAGE','MAJOR DAMS','CANAL/DITCH')))) %>%
  ggplot(aes(x = var_names, y = Testing_NRMSE, fill = color))+
  geom_segment( aes(x=var_names, xend=var_names, y=0, yend=Testing_NRMSE)) +
  geom_point(size=5, shape=21, stroke=1)+ 
  scale_fill_manual(values = c('red','blue'))+
  #ggtitle('Correlation of single-site model Normalized RMSE and basin attributes')+
  scale_x_discrete(limits=rev)+
  coord_flip()+
  theme_bw()+
  ylab('Pearson r')+
  xlab('')+
  ylim(-0.35,0.35)+
  theme(axis.text = element_text(size = 12),
        legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14))+
  geom_hline(yintercept = 0, color = 'black')+
  theme(axis.text.y = element_text(colour = cols))

cor_test_plot
ggsave('04_analysis/figs/single_site_basin_char_lollipop.jpeg', height = 6, width = 8, dpi = 750)
#dev.off()  
  

##############---------Map of clusters---------##############
#===================================================================================#
clusters_ht <- read_csv('04_analysis/out/basin_char_w_clusters_hydroterranes_221005.csv')
cluster_assig <- clusters_ht %>%
  dplyr::select(site_no, PHYS_LONG, PHYS_LAT, cluster_01, hydro_terrane) %>%
  mutate(Cluster = factor(cluster_01))

states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_cluster <- st_as_sf(cluster_assig, coords = c('PHYS_LONG','PHYS_LAT'), crs = 4326)


cl_map <- ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_cluster, aes(fill = Cluster), shape = 21, size = 5, alpha = 0.8) +
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill = guide_legend(nrow = 1, override.aes = list(size=9)))+
  scale_fill_manual(values = cols) +
  #guides(fill = guide_legend(nrow = 1))+
  #guides(fill=guide_colourbar(title="Cluster assignment"))+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_cex = 1.6
  )+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(-0.1, "in"), pad_y = unit(1.8, "in"),
    style = ggspatial::north_arrow_minimal(
      text_size = 16, line_width = .8
    )
  )+
  theme(axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))
cl_map
#table(cluster_assig$cluster)


####################################
#PCA plot with biplot for cluster analysis
#from: https://medium.com/@RaharditoDP/principal-component-analysis-with-biplot-analysis-in-r-ee39d17096a1
library(factoextra)
library(FactoMineR)
library(scales)
show_col(hue_pal()(7))

basin_char_for_pca <- as.matrix(basin_char_clean %>%
                                  dplyr::select(all_of(names_lookup)))
rownames(basin_char_for_pca) <- basin_char_clean$site_no

res.pca <- PCA(basin_char_for_pca, graph = FALSE)

#cols <- c('#F8766D','#C49A00','#53B400','#00C094','#00B6EB','#A58AFF','#FB61D7')
#cols <- c('#ebac23','#b80058','#008cf9','#006e00','#d163e6','#b24502','#ff9287','#00c6f8')
cols <- brewer.pal(7, 'Set1')
library(paletteer) 
cols <- paletteer_d("awtools::bpalette")[c(2,3,5,7,10,12,13,14)]

cl_biplot <- fviz_pca_biplot(res.pca, 
                             fill.ind = factor(clusters_ht$cluster_01), pointshape = 21, pointsize = 5, mean.point = FALSE, 
                             label = 'var', legend.title = 'Cluster', repel = TRUE, 
                             select.var = list(contrib = 12), ggtheme = theme_bw(), col.var = 'darkgray', xlab = 'PC1', ylab = 'PC2',
                             title = 'Principal component analysis of clustered sites', alpha = 0.85)+
  scale_fill_manual(values = c(cols))+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 18))

#lulc
lulc_l <- clusters_ht %>%
  group_by(cluster_01) %>%
  dplyr::select(starts_with('LULC')) %>%
  summarise(across(LULC_DEV:LULC_WTLND,median)) %>%
  pivot_longer(cols = LULC_DEV:LULC_WTLND) %>%
  mutate(Landuse = factor(name, levels = rev(c('LULC_AG','LULC_DEV','LULC_FOR','LULC_WTLND')))) %>%
  mutate(Cluster = factor(cluster_01)) %>%
  ggplot(aes(x = Cluster, y = value*100, fill = Landuse))+
  geom_bar(stat = 'identity')+
  theme_bw()+
  theme(legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 18))+
  scale_fill_manual(values = rev(c('#f4a261','firebrick','#adc178','lightblue')), 
                    labels = rev(c('Agriculture','Developed','Forested','Wetlands')))+
  guides(fill = guide_legend(nrow = 2, override.aes = list(size=3)))+
  ggtitle('Land use')+
  ylab('Percentage of contributing area')

lulc <- lulc_l + theme(legend.position = 'none')
lulc_leg <- get_legend(lulc_l)

#soils
soils_l <- clusters_ht %>%
  group_by(cluster_01) %>%
  dplyr::select(starts_with('SOIL')) %>%
  mutate(High = SOIL_HGA+SOIL_HGAD, Moderate = SOIL_HGB+SOIL_HGBC+SOIL_HGBD,
         Slow = SOIL_HGC+SOIL_HGCD, `Very slow` = SOIL_HGD) %>%
  dplyr::select(!starts_with('SOIL')) %>%
  summarise(across(High:`Very slow`,median)) %>%
  pivot_longer(cols = High:`Very slow`) %>%
  mutate(Infiltration = factor(name, levels = rev(c('Very slow','Slow','Moderate','High')))) %>%
  mutate(Cluster = factor(cluster_01)) %>%
  ggplot(aes(x = Cluster, y = value, fill = Infiltration))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = rev(c('#432818','#99582a','#bb9457','#ffe6a7')), 
                    labels = rev(c('Very slow','Slow','Moderate','High')))+
  guides(fill = guide_legend(nrow = 2, override.aes = list(size=3)))+
  ggtitle('Soil infiltration capacity')+
  theme_bw()+
  theme(legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        title = element_text(size = 18))+
  ylab('Percentage of contributing area')

soils <- soils_l + theme(legend.position = 'none')
soils_leg <- get_legend(soils_l)


hydro_mean_q <- clusters_ht %>%
  group_by(cluster_01) %>%
  summarise(across(PHYS_BASIN_AREA:CHEM_CQ_SLOPE,median)) %>%
  mutate(Cluster = factor(cluster_01)) %>%
  ggplot(aes(x = Cluster, y = log10(HYDRO_MEAN_Q), fill = Cluster))+
  geom_bar(stat = 'identity', color = 'black')+
  ggtitle('Mean discharge')+
  scale_fill_manual(values = c(cols))+
  theme_bw()+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 18))+
  ylab('log(Mean discharge) (cfs)')

anthro_maj <- clusters_ht %>%
  group_by(cluster_01) %>%
  summarise(across(PHYS_BASIN_AREA:CHEM_CQ_SLOPE,median)) %>%
  mutate(Cluster = factor(cluster_01)) %>%
  ggplot(aes(x = Cluster, y = ANTHRO_MAJOR2013, fill = Cluster))+
  geom_bar(stat = 'identity', color = 'black')+
  ggtitle('Major dams')+
  scale_fill_manual(values = c(cols))+
  theme_bw()+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 18))+
  ylab('Number of major dams')

anthro_npdes <- clusters_ht %>%
  group_by(cluster_01) %>%
  summarise(across(PHYS_BASIN_AREA:CHEM_CQ_SLOPE,median)) %>%
  mutate(Cluster = factor(cluster_01)) %>%
  ggplot(aes(x = Cluster, y = ANTHRO_NPDES_MAJ, fill = Cluster))+
  geom_bar(stat = 'identity', color = 'black')+
  ggtitle('Major NPDES sites')+
  scale_fill_manual(values = c(cols))+
  theme_bw()+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 18))+
  ylab('Number of NPDES sites')

hydro_recharge <- clusters_ht %>%
  group_by(cluster_01) %>%
  summarise(across(PHYS_BASIN_AREA:CHEM_CQ_SLOPE,median)) %>%
  mutate(Cluster = factor(cluster_01)) %>%
  ggplot(aes(x = Cluster, y = HYDRO_RECHG, fill = Cluster))+
  geom_bar(stat = 'identity', color = 'black')+
  ggtitle('Amount of recharge')+
  scale_fill_manual(values = c(cols))+
  theme_bw()+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 18))+
  ylab('Recharge')

barplots <- plot_grid(hydro_mean_q, anthro_maj, anthro_npdes, hydro_recharge, lulc, soils, lulc_leg, soils_leg,
          rel_heights = c(3,3,3,0.5), ncol = 2, labels = c('C','D','E','F','G','H','',''), 
          label_size = 20, hjust = -2.3, align = 'v', axis = 'l')
map_pca <- plot_grid(cl_map, cl_biplot, ncol = 1, labels = 'AUTO', align = 'l', label_size = 20)
#jpeg('04_analysis/figs/clusters_map_new_col.jpeg', height = 800, width = 1100)
plot_grid(map_pca, barplots, ncol = 2)
ggsave('04_analysis/figs/clusters_map_new_col_02.jpeg', height = 9, width = 16, dpi = 300)
#dev.off()

#layout <- as.matrix(rbind(c(1,1,1,1,3,3,4,4),c(1,1,1,1,3,3,4,4),
#                          c(1,1,1,1,5,5,6,6),c(2,2,2,2,5,5,6,6),
#                          c(2,2,2,2,7,7,8,8),c(2,2,2,2,7,7,8,8)))
#pdf('04_analysis/figs/clusters_map.pdf', height = 8, width = 11)
#jpeg('04_analysis/figs/clusters_map.jpeg', height = 800, width = 1100)
#gridExtra::grid.arrange(cl_map, cl_biplot, hydro_mean_q, anthro_maj, anthro_npdes, hydro_recharge, lulc, soils, layout_matrix = layout)
#dev.off()
cl_map
ggsave('04_analysis/figs/cluster_map_only.jpeg', height = 6, width = 8, dpi = 300)


#View and save cluster attributes
cluster_grouped <- clusters_ht %>%
  group_by(cluster_01) %>%
  summarise(across(PHYS_BASIN_AREA:CHEM_CQ_SLOPE,median))

# for(i in 2:56){
# plot_df <- data.frame(cluster_01 = cluster_grouped$cluster_01, var = cluster_grouped[[i]])
# var_name <- colnames(cluster_grouped)[i]
# var_plot <- plot_df %>%  
#   mutate(Cluster = factor(cluster_01)) %>%
#   ggplot(aes(x = Cluster, y = var, fill = Cluster))+
#   geom_bar(stat = 'identity', color = 'black')+
#   ggtitle(var_name)+
#   scale_fill_manual(values = c(cols))+
#   theme_bw()+
#   theme(legend.text = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         axis.text = element_text(size = 16),
#         axis.title = element_text(size = 16),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         legend.position = 'none',
#         title = element_text(size = 18))+
#   ylab(var_name)
# 
# var_plot
# ggsave(paste0('04_analysis/figs/cluster_barplots/',var_name,'.jpeg'),height = 6, width = 8, dpi = 150)
# }

##############---------Map of best model---------##############
#===================================================================================#
cl <- read_csv('04_analysis/out/cluster_ensemble_full_run_08_C_summary.csv') %>%
  mutate(hydro_terrane = NA, run = 'Clustered') %>%
  rename('lat' = 'PHYS_LAT','long'= 'PHYS_LONG')
ss <- read_csv('04_analysis/out/single_site_ensemble_run_summary.csv') %>%
  mutate(hydro_terrane = NA, cluster = cl$cluster, run = 'Single-site')
ms <- read_csv('04_analysis/out/multi_site_ensemble_full_run_MS_07_summary.csv') %>%
  mutate(hydro_terrane = NA, cluster = cl$cluster, run = 'Multi-site')
ht <- read_csv('04_analysis/out/hydro_terrane_ensemble_full_run_08_HT_summary.csv') %>%
  mutate(hydro_terrane = cluster, cluster = cl$cluster, run = 'Hydro terrane') %>%
  rename('lat' = 'PHYS_LAT','long'= 'PHYS_LONG')
cl$hydro_terrane <- ht$hydro_terrane
ss$hydro_terrane <- ht$hydro_terrane
ms$hydro_terrane <- ht$hydro_terrane


all_models <- rbind(ss, ms, cl, ht) %>%
  mutate(run = factor(run, levels = c("Single-site","Clustered","Hydro terrane","Multi-site")))


best_models <- all_models %>%
  group_by(site_no) %>%
  arrange(Testing_RMSE, .by_group = TRUE) %>%
  summarise(site_no = first(site_no), RMSE = first(Testing_RMSE), run = first(run), cluster = first(cluster),
            lat = first(lat), long = first(long))
  

states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_runs_sf <- st_as_sf(best_models, coords = c('long','lat'), crs = 4326) #%>%
  #filter(run %in% c('Single-site','Clustered','Hydro terrane'))



map <- ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_runs_sf, aes(fill = run), shape = 21, size = 5, alpha = 0.8) +
  scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  guides(fill=guide_legend(title="Best performing model\nat each site"))+
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_cex = 1.6
  )+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(-0.1, "in"), pad_y = unit(1.8, "in"),
    style = ggspatial::north_arrow_minimal(
      text_size = 16, line_width = .8
    )
  )+
  guides(fill = guide_legend(nrow = 1, override.aes = list(size=9)))

map
ggsave('04_analysis/figs/best_model_map_4.jpg', height = 6, width = 8, dpi = 300)

dist <- all_models %>%
  mutate(models = factor(run, 
                         levels = rev(c("Single-site","Clustered","Hydro terrane","Multi-site")),
                         labels = rev(c("Single-site","Clustered","Hydro terrane","Global")))) %>%
  ggplot(aes(x = models, y = Testing_RMSE, fill = models))+
  geom_boxplot(alpha = 0.9)+
  scale_fill_manual(values = rev(c('#00bbf9','#ff595e','#ffca3a','#00f5d4')))+
  #scale_color_manual(values = rev(c('#00bbf9','#ff595e','#ffca3a','#00f5d4')))+
  guides(title = "Model")+
  theme_bw()+
  ggtitle('Model performance at all sites')+
  #geom_jitter(aes(fill=run), size=1.5, alpha=0.7, shape = 21, color = 'black')+
  coord_flip()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 12))+
  xlab('')+
  ylab('RMSE (mg/L)')
dist
ggsave('04_analysis/figs/compare_models_dist.jpeg', height = 2, width = 4, dpi = 1000)
#improvement over single site models
best_non_ss_models <- all_models %>%
  filter(run != 'Single-site') %>%
  group_by(site_no) %>%
  arrange(Testing_RMSE, .by_group = TRUE) %>%
  summarise(site_no = first(site_no), NSE = first(Testing_NSE), 
            NRMSE = first(Testing_NRMSE),
            RMSE = first(Testing_RMSE),
            run = first(run), cluster = first(cluster), 
            hydro_terrane = first(hydro_terrane),
            lat = first(lat), long = first(long))

best_models_char <- best_non_ss_models %>%
  mutate(color = if_else(run == 'Single-site', '#00bbf9',
                         if_else(run == 'Clustered', '#ff595e',
                                 if_else(run == 'Hydro terrane', '#ffca3a','#00f5d4')))) %>%
  left_join(basin_char, by = 'site_no')

improve <- all_models %>%
  select(site_no, Testing_RMSE, run) %>%
  pivot_wider(names_from = run, values_from = Testing_RMSE) %>%
  left_join(best_models_char[,c('site_no','run','RMSE','color')], by = ('site_no')) %>%
  mutate(improvement = `Single-site`-`RMSE`) %>%
  mutate(run = as.character(run)) %>%
  mutate(run = if_else(improvement<0, 'Single-site', run)) %>%
  mutate(run = factor(run, levels = c('Single-site','Clustered','Hydro terrane','Multi-site'))) %>%
  ggplot(aes(x = `Single-site`, y = improvement, fill = run)) +
  geom_hline(yintercept = 0, color = 'darkgray', linetype="dashed", size=1)+
  geom_point(shape = 21, size = 4, alpha = 0.8)+
  scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  #xlim(0,150)+
  #ylim(-50,50)+
  theme_bw()+
  ggtitle('Improvement over single-site models')+
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 12))+
  ylab('Improvement in RMSE (mg/L)')+
  xlab('Single-site RMSE (mg/L)')
improve
ggsave('04_analysis/figs/single_site_improvement.jpeg', height = 4, width = 4, dpi = 1000)


non_maps <- plot_grid(improve, dist, nrow = 1, align = 'h', axis = 'b')
plot_grid(map,non_maps, rel_heights = c(3,2), nrow = 2)
ggsave('04_analysis/figs/best_models_map_dist_imp.jpg',height = 7, width = 12, dpi = 300)

##############---------Map of best model---------##############
#===================================================================================#
rmse_clust_l <- all_models %>%
  dplyr::select(cluster, run, Testing_NRMSE) %>%
  group_by(run) %>%
  mutate(cluster_plot = factor(cluster, levels = c('01','02','03','04','05','06','07'),
                               labels = c('1','2','3','4',
                                          '5','6','7')))%>%
  ggplot(aes(x = cluster_plot, y = Testing_NRMSE, fill = run)) +
  geom_boxplot()+
  theme_bw()+
  scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  theme(legend.key.height = unit(1, 'cm'), #change legend key size
        legend.key.width = unit(0.75, 'cm'),
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=10),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #ylab('RMSE [N-NO3] (mg/L)')+
  #xlab('')+
  labs(fill = '', y = 'NRMSE [N-NO3] (mg/L)', x = '')+
  ylim(0,150)

rmse_clust <- rmse_clust_l + theme(legend.position = 'none')

nse_clust <- all_models %>%
  dplyr::select(cluster, run, Testing_NSE) %>%
  group_by(run) %>%
  mutate(cluster_plot = factor(cluster, levels = c('01','02','03','04','05','06','07'),
                               labels = c('1','2','3','4',
                                          '5','6','7')))%>%
  ggplot(aes(x = cluster_plot, y = Testing_NSE, fill = run)) +
  geom_boxplot()+
  theme_bw()+
  scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab('NSE')+
  xlab('Cluster')+
  ylim(-0.5,1)

legend <- get_legend(rmse_clust_l)

jpeg('04_analysis/figs/RMSE_NSE_by_cluster.jpeg', height = 400, width = 600)
plot_grid(rmse_clust, legend, nse_clust, NULL, nrow = 2, rel_widths = c(1,.3,1,.3),align = 'v', axis = 'l', labels = c('A','','B',''))
dev.off()

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
)

colnames(a) <- c('RMSE','NSE','PBIAS','r','KGE')
rownames(a) <- c('Single-site','Clustered','Hydro terrane','Multi-site')  
a

#performance metric by cluster or hydroterrane
all_models %>%
  group_by(cluster, run) %>%
  summarise(med_NSE = median(Testing_NSE)) %>%
  pivot_wider(names_from = 'run', values_from = 'med_NSE')


#best models
best_models <- all_models %>%
  group_by(site_no) %>%
  arrange(desc(Testing_NSE), .by_group = TRUE) %>%
  summarise(site_no = first(site_no), RMSE = first(Testing_RMSE), run = first(run), cluster = first(cluster),
            lat = first(lat), long = first(long))

cl_sites <- best_models %>%
  filter(run == "Clustered") %>%
  pull(site_no)

print('Best model is clustered: ')
site_info[site_info$site_no %in% cl_sites,]$station_nm

ms_sites <- best_models %>%
  filter(run == 'Multi-site') %>%
  pull(site_no)

print('Best model is multi-site: ')
site_info[site_info$site_no %in% ms_sites,]$station_nm


########################################################
##Maps for presentation

fc <- sf::st_read("01_fetch/in/Haj_Terranes/HydrogeologicTerranes.gdb", layer = "HydrogeologicTerranes_poly")
fc_rpj <- st_transform(fc, crs = 4326)
# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
#plot(fc)
library(paletteer) 
#for clusters
cols <- paletteer_d("awtools::bpalette")[c(2,3,5,7,10,12,13,14)]
#for hydro terranes
cols_ht <- rev(paletteer_d("Polychrome::green_armytage"))

clusters_ht <- read_csv('04_analysis/out/basin_char_w_clusters_hydroterranes_221005.csv')
cluster_assig <- clusters_ht %>%
  dplyr::select(site_no, PHYS_LONG, PHYS_LAT, cluster_01, hydro_terrane) %>%
  mutate(Cluster = factor(cluster_01))

states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_cluster <- st_as_sf(cluster_assig, coords = c('PHYS_LONG','PHYS_LAT'), crs = 4326)


cl_map <- ggplot(data = states) +
  geom_sf()+
  #geom_sf(data = fc_rpj, aes(fill = Terrane), color = 'black', alpha = 0.15, show.legend = FALSE)+
  geom_sf(data = site_cluster, aes(fill = hydro_terrane), shape = 21, size = 5, alpha = 0.8) +
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  guides(fill = guide_legend(nrow = 1, override.aes = list(size=9)))+
  scale_fill_manual(values = cols) +
  #guides(fill = guide_legend(nrow = 1))+
  #guides(fill=guide_colourbar(title="Cluster assignment"))+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_cex = 1.6
  )+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(-0.1, "in"), pad_y = unit(1.8, "in"),
    style = ggspatial::north_arrow_minimal(
      text_size = 16, line_width = .8
    )
  )+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))
cl_map
#ggsave('04_analysis/figs/map_all_sites.jpeg', height = 6, width = 8, dpi = 300)



