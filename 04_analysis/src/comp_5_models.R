#===================================================================================#
# NOTES:  Comparison of 5 modeling approaches
#
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-09-30                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
#####
#===================================================================================#

# cl1 <- read_csv('04_analysis/out/cluster_01_ensemble_full_run_summary.csv') %>%
#   mutate(hydro_terrane = NA, run = 'Clustered_01')
cl2 <- read_csv('04_analysis/out/cluster_02_ensemble_full_run_summary.csv') %>%
  mutate(hydro_terrane = NA, run = 'Clustered')
ss <- read_csv('04_analysis/out/single_site_ensemble_run_summary.csv') %>%
  mutate(hydro_terrane = NA, cluster = cl2$cluster, run = 'Single-site')
ms <- read_csv('04_analysis/out/multi_site_ensemble_full_run_summary.csv') %>%
  mutate(hydro_terrane = NA, cluster = cl2$cluster, run = 'Multi-site')
ht <- read_csv('04_analysis/out/hydro_terrane_ensemble_full_run_summary.csv') %>%
  mutate(hydro_terrane = cluster, cluster = cl2$cluster, run = 'Hydro terrane')
#cl1$hydro_terrane <- ht$hydro_terrane
cl2$hydro_terrane <- ht$hydro_terrane
ss$hydro_terrane <- ht$hydro_terrane
ms$hydro_terrane <- ht$hydro_terrane


all_models <- rbind(ss, ms, cl2, ht) %>%
  #filter(run != 'Clustered_01') %>%
  mutate(run = factor(run, levels = c("Single-site","Clustered","Hydro terrane","Multi-site")))


best_models <- all_models %>%
  group_by(site_no) %>%
  arrange(desc(Testing_KGE), .by_group = TRUE) %>%
  summarise(site_no = first(site_no), NSE = first(Testing_NSE), run = first(run), cluster = first(cluster), 
            hydro_terrane = first(hydro_terrane),
            lat = first(lat), long = first(long))

table(best_models$run)


all_models %>%
  mutate(run = factor(run)) %>%
  ggplot(aes(x = run, y = Testing_RMSE, fill = run)) +
  geom_bar(stat = 'identity')+
  #ylim(0,1)+
  facet_wrap(.~site_no)

all_models %>% group_by(run) %>% summarize(med_r = median(Testing_r))


#make a map of the best models for each site
basin_char <- read_csv('04_analysis/out/basin_char_calc_clean.csv')
basin_names <- read_csv('04_analysis/out/basin_char_w_clusters_10.csv') %>% dplyr::select(lat,long,station_nm)

basin_char <- left_join(basin_char, basin_names, by = c('PHYS_LAT' = 'lat', 'PHYS_LONG' = 'long'))


best_models_char <- best_models %>%
  mutate(color = if_else(run == 'Single-site', '#00bbf9',
         if_else(run == 'Clustered', '#ff595e',
         if_else(run == 'Hydro terrane', '#ffca3a','#00f5d4')))) %>%
  left_join(basin_char, by = 'site_no')

par(mfrow = c(2,2))
for (i in 9:60){
jpeg(paste0('04_analysis/figs/best_model_drivers/',colnames(best_models_char)[i],'.jpeg'))
plot(pull(best_models_char[,i]), pull(best_models_char[,'NSE']), pch = 21, bg = best_models_char$color, main = colnames(best_models_char)[i],
     xlab = colnames(best_models_char)[i], ylab = 'NSE', ylim = c(0,1))  
dev.off()
}


#make a time series of the plots where clustered models did better
cl_sites <- best_models_char %>%
  filter(run == 'Clustered') %>%
  dplyr::select(site_no,station_nm,cluster,hydro_terrane)

j = 1
for (j in 1:6){
rep <- c('Rep_00','Rep_01','Rep_02','Rep_03','Rep_04')
site <- cl_sites$site_no[j]
cluster <- cl_sites$cluster[j]
hydro_terrane <- cl_sites$hydro_terrane[j]
station_nm <- cl_sites$station_nm[j]


#cluster
cl_ens <- read_csv(file.path('03_model/out/multi_site/Run_04C/Rep_00',paste0('Cluster_',cluster), site,'ModelResults.csv'))
for(i in 2:length(rep)){
  temp_rep <- read_csv(file.path('03_model/out/multi_site/Run_04C',rep[i], paste0('Cluster_',cluster), site,'ModelResults.csv'))
  cl_ens[paste0('Predicted_',i)] <- temp_rep$Predicted
}
cl <- cl_ens %>%
  relocate(Predicted, .after = `Train/Val/Test`) %>%
  rowwise() %>%
  mutate(Preds = mean(c(Predicted, Predicted_2, Predicted_3, Predicted_4, Predicted_5))) %>%
  arrange(DateTime) %>%
  dplyr::select(!Predicted:Predicted_5) %>%
  mutate(Model = 'Clustered')

#single site
ss_ens <- read_csv(file.path('03_model/out/single_site/Run_00_Full/Rep_00', site,'ModelResults.csv'))
for(i in 2:length(rep)){
  temp_rep <- read_csv(file.path('03_model/out/single_site/Run_00_Full',rep[i], site,'ModelResults.csv'))
  ss_ens[paste0('Predicted_',i)] <- temp_rep$Predicted
}
ss <- ss_ens %>%
  relocate(Predicted, .after = `Train/Val/Test`) %>%
  rowwise() %>%
  mutate(Preds = mean(c(Predicted, Predicted_2, Predicted_3, Predicted_4, Predicted_5))) %>%
  arrange(DateTime) %>%
  dplyr::select(!Predicted:Predicted_5) %>%
  mutate(Model = 'Single-site')

#multi site
ms_ens <- read_csv(file.path('03_model/out/multi_site/Run_02/Rep_00', site,'ModelResults.csv'))
for(i in 2:length(rep)){
  temp_rep <- read_csv(file.path('03_model/out/multi_site/Run_02',rep[i], site,'ModelResults.csv'))
  ms_ens[paste0('Predicted_',i)] <- temp_rep$Predicted
}
ms <- ms_ens %>%
  relocate(Predicted, .after = `Train/Val/Test`) %>%
  rowwise() %>%
  mutate(Preds = mean(c(Predicted, Predicted_2, Predicted_3, Predicted_4, Predicted_5))) %>%
  arrange(DateTime) %>%
  dplyr::select(!Predicted:Predicted_5) %>%
  mutate(Model = 'Multi-site')

#hydro_terrane
ht_ens <- read_csv(file.path('03_model/out/multi_site/Run_05HT/Rep_00',paste0('Terrane_',hydro_terrane), site,'ModelResults.csv'))
for(i in 2:length(rep)){
  temp_rep <- read_csv(file.path('03_model/out/multi_site/Run_05HT',rep[i],paste0('Terrane_',hydro_terrane), site,'ModelResults.csv'))
  ht_ens[paste0('Predicted_',i)] <- temp_rep$Predicted
}
ht <- ht_ens %>%
  relocate(Predicted, .after = `Train/Val/Test`) %>%
  rowwise() %>%
  mutate(Preds = mean(c(Predicted, Predicted_2, Predicted_3, Predicted_4, Predicted_5))) %>%
  arrange(DateTime) %>%
  dplyr::select(!Predicted:Predicted_5) %>%
  mutate(Model = 'Hydro terrane')

g_ss <- rbind(ss %>%
                filter(`Train/Val/Test` == 'Testing') %>%
                dplyr::select(DateTime,Labeled) %>%
                mutate(Set = 'Observed') %>%
                rename('Preds' = 'Labeled','Model' = 'Set'),
              ss %>%
                filter(`Train/Val/Test` == 'Testing') %>%
                dplyr::select(DateTime,Preds,Model),
              ms %>%
                filter(`Train/Val/Test` == 'Testing') %>%
                dplyr::select(DateTime,Preds,Model),
              cl %>%
                filter(`Train/Val/Test` == 'Testing') %>%
                dplyr::select(DateTime,Preds,Model),
              ht %>%
                filter(`Train/Val/Test` == 'Testing') %>%
                dplyr::select(DateTime,Preds,Model)
) %>%
  as_tibble() %>%
  mutate(Model = factor(Model, levels = c('Observed',"Single-site","Clustered","Hydro terrane","Multi-site"))) %>%
  ggplot(aes(x = DateTime, y = Preds, color = Model))+
  geom_line()+
  scale_color_manual(values = c('black','#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  theme_bw()+
  ylab('[NO3-N] (mg/L)')+
  xlab('')+
  #geom_vline(xintercept = ss[ss$`Train/Val/Test` == 'Testing',]$DateTime[1])+
  ggtitle(paste0(station_nm,' | Cluster ', cluster,' | Hydro Terrane ',hydro_terrane))+
  theme(legend.position="none")


rmse_bpl <- all_models %>%
  filter(site_no == site) %>%
  mutate(Model = factor(run, levels = c("Single-site","Clustered","Hydro terrane","Multi-site"))) %>%
  ggplot(aes(x = Model, y = Testing_RMSE, fill = Model))+
  geom_bar(stat = 'identity', color = 'black')+
  scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  theme_bw()+
  ylab('RMSE (mg/L)')

layout <- matrix(c(1,1,2))
gridExtra::grid.arrange(g_ss, rmse_bpl, layout_matrix = layout)
}

