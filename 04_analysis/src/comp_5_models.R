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

gh <- '~/Documents/GitHub/no3_ml_proj/'
gd <- '~/galengorski@berkeley.edu - Google Drive/My Drive/ESDL Postdoc/02_Projects/no3_ml_proj/'

ss_run_id <- 'Run_01_Baseline_230422_Discharge_l10'
ms_run_id <- 'Run_01_multisite_230529'
cl_run_id <- 'Run_02_cluster_230530'
ht_run_id <- 'Run_06_230424_HydroTerrane_Discharge_l10'

date <- '2023-05-31'

basin_char <- read_csv(file.path(gh, '04_analysis/out/basin_char_w_clusters_hydroterranes_230529.csv'))

ms <- read_csv(file.path(gd,paste0('04_analysis/out/',ms_run_id,'_ensemble_results_',date,'.csv'))) %>%
  relocate(site_no:long, .before = Testing_RMSE) %>%
  rename('PHYS_LAT' = 'lat','PHYS_LONG' = 'long') %>%
  mutate(run = 'global', hydro_terrane = basin_char$hydro_terrane, cluster = basin_char$cluster) %>%
  relocate(run:cluster, .before = PHYS_LAT)

ss <- read_csv(file.path(gd,paste0('04_analysis/out/',ss_run_id,'_ensemble_results_',date,'.csv'))) %>%
  relocate(site_no:long, .before = Testing_RMSE) %>%
  rename('PHYS_LAT' = 'lat','PHYS_LONG' = 'long') %>%
  mutate(run = 'Single-site', hydro_terrane = basin_char$hydro_terrane, cluster = basin_char$cluster) %>%
  relocate(run:cluster, .before = PHYS_LAT)

cl <- read_csv(file.path(gd,paste0('04_analysis/out/',cl_run_id,'_ensemble_results_',date,'.csv'))) %>%
  relocate(PHYS_LAT:PHYS_LONG, .before = Testing_RMSE) %>%
  dplyr::select(!cluster) %>%
  mutate(run = 'Clustered', hydro_terrane = basin_char$hydro_terrane, cluster = basin_char$cluster) %>%
  relocate(run:cluster, .before = PHYS_LAT)

ht <- read_csv(file.path(gd,paste0('04_analysis/out/',ht_run_id,'_ensemble_results_',date,'.csv'))) %>%
  relocate(PHYS_LAT:PHYS_LONG, .before = Testing_RMSE) %>%
  dplyr::select(!cluster) %>%
  mutate(run = 'Hydro terrane', hydro_terrane = basin_char$hydro_terrane, cluster = basin_char$cluster) %>%
  relocate(run:cluster, .before = PHYS_LAT)


all_models <- rbind(#ss, 
  ms, cl#, 
  #ht
  ) %>%
  mutate(run = factor(run, levels = c("Single-site","Clustered","Hydro terrane","global")))


write_csv(all_models, file.path(gd, paste0('04_analysis/out/all_models_summary_',Sys.Date(),'.csv')))


###########################################
#Site summaries for updated models
gh <- '~/Documents/GitHub/no3_ml_proj/'
gd <- '~/galengorski@berkeley.edu - Google Drive/My Drive/ESDL Postdoc/02_Projects/no3_ml_proj/'

all_models <- read_csv(file.path(gd, paste('04_analysis/out/all_models_summary_2023-04-26.csv')))

basin_char <- read_csv(file.path(gh, '04_analysis/out/basin_char_w_clusters_hydroterranes_230423.csv'))
sites <- basin_char$site_no
reps <- 10

ss_run_id <- 'Run_01_Baseline_230422_Discharge_l10'
g_run_id <- 'Run_03_230423_Discharge_l10'
cl_run_id <- 'Run_05_230424_Cluster_Discharge_l10'
ht_run_id <- 'Run_06_230424_HydroTerrane_Discharge_l10'

for(j in 1:length(unique(basin_char$site_no))){

  site <- unique(basin_char$site_no)[j]
  
  #site name
  site_nm <- basin_char %>%
    filter(site_no == site) %>%
    pull(station_nm)
  
  #cluster
  cluster <- basin_char %>%
    filter(site_no == site) %>%
    pull(cluster)
  
  #ht
  hydro_terrane <- basin_char %>%
    filter(site_no == site) %>%
    pull(hydro_terrane)
  
  
  print(site_nm)
  
  
  #single-site
  single_site_run_summary <- data.frame()
  site_temp <- read_csv(file.path(paste0(gh,'03_model/out/single_site/',ss_run_id,'/Rep_00'),site,'ModelResults.csv'),show_col_types = FALSE)[,c("DateTime","Labeled","Train/Val/Test")]
  for (j in 1:reps){
    rep <- str_pad(j-1, 2, pad = '0')
    site_rep_temp <- read_csv(file.path(paste0(gh,'03_model/out/single_site/',ss_run_id),paste0('Rep_',rep),site,'ModelResults.csv'),show_col_types = FALSE)[,"Predicted"]
    site_temp <- cbind(site_temp,site_rep_temp) 
  }
  site_temp$Predicted_mean <- rowMeans(site_temp[grepl( "Predicted" , names( site_temp ) )]) 
  site_temp_test <- site_temp[site_temp$`Train/Val/Test` == 'Testing',]
  colnames(site_temp_test)[4:13] <- paste0('Rep_',seq(1:10))
  site_temp_summary <- site_temp_test %>%
    tibble() %>%
    rename('Set' = `Train/Val/Test`) 
  
  #global
  global_run_summary <- data.frame()
  site_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',g_run_id,'/Rep_00'),site,'ModelResults.csv'),show_col_types = FALSE)[,c("DateTime","Labeled","Train/Val/Test")]
  for (j in 1:reps){
    rep <- str_pad(j-1, 2, pad = '0')
    site_rep_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',g_run_id),paste0('Rep_',rep),site,'ModelResults.csv'),show_col_types = FALSE)[,"Predicted"]
    site_temp <- cbind(site_temp,site_rep_temp) 
  }
  site_temp$Predicted_mean <- rowMeans(site_temp[grepl( "Predicted" , names( site_temp ) )]) 
  site_temp_test <- site_temp[site_temp$`Train/Val/Test` == 'Testing',]
  colnames(site_temp_test)[4:13] <- paste0('Rep_',seq(1:10))
  global_temp_summary <- site_temp_test %>%
    tibble() %>%
    rename('Set' = `Train/Val/Test`) 
  
  #cluster
  cluster_run_summary <- data.frame()
  clust <- paste0('Cluster_0',basin_char %>%
      filter(site_no == site) %>%
      pull(cluster))
  site_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',cl_run_id,'/Rep_00'),clust,site,'ModelResults.csv'),show_col_types = FALSE)[,c("DateTime","Labeled","Train/Val/Test")]
  for (j in 1:reps){
      rep <- str_pad(j-1, 2, pad = '0')
      site_rep_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',cl_run_id),paste0('Rep_',rep),clust,site,'ModelResults.csv'),show_col_types = FALSE)[,"Predicted"]
      site_temp <- cbind(site_temp,site_rep_temp) 
  }
  
  site_temp$Predicted_mean <- rowMeans(site_temp[grepl( "Predicted" , names( site_temp ) )]) 
  site_temp_test <- site_temp[site_temp$`Train/Val/Test` == 'Testing',]
  colnames(site_temp_test)[4:13] <- paste0('Rep_',seq(1:10))
  clust_temp_summary <- site_temp_test %>%
    tibble() %>%
    rename('Set' = `Train/Val/Test`) 
  
  #hydroterrane
  ht_run_summary <- data.frame()
  ht <- paste0('Terrane_',basin_char %>%
                    filter(site_no == site) %>%
                    pull(hydro_terrane))
  site_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',ht_run_id,'/Rep_00'),ht,site,'ModelResults.csv'),show_col_types = FALSE)[,c("DateTime","Labeled","Train/Val/Test")]
  for (j in 1:reps){
    rep <- str_pad(j-1, 2, pad = '0')
    site_rep_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',ht_run_id),paste0('Rep_',rep),ht,site,'ModelResults.csv'),show_col_types = FALSE)[,"Predicted"]
    site_temp <- cbind(site_temp,site_rep_temp) 
  }
  
  site_temp$Predicted_mean <- rowMeans(site_temp[grepl( "Predicted" , names( site_temp ) )]) 
  site_temp_test <- site_temp[site_temp$`Train/Val/Test` == 'Testing',]
  colnames(site_temp_test)[4:13] <- paste0('Rep_',seq(1:10))
  ht_temp_summary <- site_temp_test %>%
    tibble() %>%
    rename('Set' = `Train/Val/Test`) 
  
  all_ts <- rbind(
    site_temp_summary %>%
      mutate(run = 'Single-site'),
    global_temp_summary %>%
      mutate(run = 'Global'),
    clust_temp_summary %>%
      mutate(run = "Clustered"),
    ht_temp_summary %>%
      mutate(run = 'Hydro terrane'),
    ht_temp_summary %>%
      mutate(run = 'Observed') %>%
      mutate(Predicted_mean = Labeled))
  
  g_ss <- all_ts %>%
    mutate(models = factor(run, levels = c('Observed',"Single-site","Clustered","Hydro terrane","Global"))) %>%
    ggplot(aes(x = DateTime, y = Predicted_mean, color = models))+
    geom_line()+
    scale_color_manual(values = c('black','#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
    theme_bw()+
    ylab('[NO3-N] (mg/L)')+
    xlab('')+
    ggtitle(paste0(site_nm,' | Cluster ', cluster,' | Hydro Terrane ',hydro_terrane))+
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12))
  
  cdf <- all_ts %>%
    filter(!is.na(Predicted_mean)) %>%
    mutate(models = factor(run, levels = c('Observed',"Single-site","Clustered","Hydro terrane","Global"))) %>%
    group_by(models) %>%
    arrange(Predicted_mean) %>%
    mutate(idx = row_number(models)/sum(!is.na(Predicted_mean))) %>%
    ggplot(aes(y = idx, x = Predicted_mean, col = models))+
    geom_line()+
    theme_bw()+
    scale_color_manual(values = c('black','#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
    ylab('CDF')+
    xlab('[NO3-N] (mg/L)')+
    theme(legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10))
  
  nrmse_bpl <- all_models %>%
    filter(site_no == site) %>%
    mutate(models = factor(run, 
                           levels = c("Single-site","Clustered","Hydro terrane","global"),
                           labels = c("Single-site","Clustered","Hydro terrane","Global"))) %>%
    ggplot(aes(x = models, y = Testing_NRMSE, fill = models))+
    geom_bar(stat = 'identity', color = 'black')+
    scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
    theme_bw()+
    ylab('NRMSE')+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          legend.position = 'none')+
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  nse_bpl <- all_models %>%
    filter(site_no == site) %>%
    mutate(models = factor(run, 
                           levels = c("Single-site","Clustered","Hydro terrane","global"),
                           labels = c("Single-site","Clustered","Hydro terrane","Global"))) %>%
    ggplot(aes(x = models, y = Testing_NSE, fill = models))+
    geom_bar(stat = 'identity', color = 'black')+
    scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
    theme_bw()+
    ylab('NSE')+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_blank())+
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  nse_bpl_nl <- nse_bpl + theme(legend.position = 'none')
  legend <- get_legend(nse_bpl)
  
  lower_panel <- plot_grid(cdf, nrmse_bpl, nse_bpl_nl, legend, nrow = 1)
  plot_grid(g_ss, lower_panel, nrow = 2, align = 'hv')
  ggsave(paste0(file.path(gh,'04_analysis/figs/site_summaries',site),'.png'), height = 6, width = 10)
}

###########################################
best_models <- all_models %>%
  group_by(site_no) %>%
  arrange(Testing_RMSE, .by_group = TRUE) %>%
  summarise(site_no = first(site_no), NSE = first(Testing_NSE), NRMSE = first(Testing_NRMSE),
            run = first(run), cluster = first(cluster), 
            hydro_terrane = first(hydro_terrane),
            lat = first(PHYS_LAT), long = first(PHYS_LONG))

table(best_models$run)


all_models %>%
  mutate(run = factor(run)) %>%
  ggplot(aes(x = run, y = Testing_RMSE, fill = run)) +
  geom_bar(stat = 'identity')+
  #ylim(0,1)+
  facet_wrap(.~site_no, scales = 'free_y')

all_models %>% 
  group_by(run) %>% 
  dplyr::summarize(med_r = median(Testing_NSE))


#make a map of the best models for each site
#basin_char <- read_csv('04_analysis/out/basin_char_calc_clean.csv')
#basin_names <- read_csv('04_analysis/out/basin_char_w_clusters_10.csv') %>% dplyr::select(lat,long,station_nm)

#basin_char <- left_join(basin_char, basin_names, by = c('PHYS_LAT' = 'lat', 'PHYS_LONG' = 'long'))


best_models_char <- best_models %>%
  mutate(color = if_else(run == 'Single-site', '#00bbf9',
         if_else(run == 'Clustered', '#ff595e',
         if_else(run == 'Hydro terrane', '#ffca3a','#00f5d4')))) %>%
  left_join(basin_char, by = 'site_no')

par(mfrow = c(2,2))
for (i in 11:60){
jpeg(paste0('04_analysis/figs/best_model_drivers/NRMSE/',colnames(best_models_char)[i],'.jpeg'))
plot(pull(best_models_char[,i]), pull(best_models_char[,'NRMSE']), pch = 21, bg = best_models_char$color, main = colnames(best_models_char)[i],
     xlab = colnames(best_models_char)[i], ylab = 'NRMSE', cex = 2, ylim = c(0,150))  
dev.off()
}


#make a time series of the plots where clustered models did better
cl_sites <- best_models_char %>%
  #filter(run == 'Clustered') %>%
  dplyr::select(site_no,station_nm,cluster,hydro_terrane)

j = 1
for (j in 1:nrow(cl_sites)){
rep <- c('Rep_00','Rep_01','Rep_02','Rep_03','Rep_04')
site <- cl_sites$site_no[j]
cluster <- cl_sites$cluster[j]
hydro_terrane <- cl_sites$hydro_terrane[j]
station_nm <- cl_sites$station_nm[j]


#cluster
cl_ens <- read_csv(file.path('03_model/out/multi_site/Run_08_C/Rep_00',paste0('Cluster_',cluster), site,'ModelResults.csv'))
for(i in 2:length(rep)){
  temp_rep <- read_csv(file.path('03_model/out/multi_site/Run_08_C',rep[i], paste0('Cluster_',cluster), site,'ModelResults.csv'))
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
ms_ens <- read_csv(file.path('03_model/out/multi_site/Run_07_MS/Rep_00', site,'ModelResults.csv'))
for(i in 2:length(rep)){
  temp_rep <- read_csv(file.path('03_model/out/multi_site/Run_07_MS',rep[i], site,'ModelResults.csv'))
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
ht_ens <- read_csv(file.path('03_model/out/multi_site/Run_08_HT/Rep_00',paste0('Terrane_',hydro_terrane), site,'ModelResults.csv'))
for(i in 2:length(rep)){
  temp_rep <- read_csv(file.path('03_model/out/multi_site/Run_08_HT',rep[i],paste0('Terrane_',hydro_terrane), site,'ModelResults.csv'))
  ht_ens[paste0('Predicted_',i)] <- temp_rep$Predicted
}
ht <- ht_ens %>%
  relocate(Predicted, .after = `Train/Val/Test`) %>%
  rowwise() %>%
  mutate(Preds = mean(c(Predicted, Predicted_2, Predicted_3, Predicted_4, Predicted_5))) %>%
  arrange(DateTime) %>%
  dplyr::select(!Predicted:Predicted_5) %>%
  mutate(Model = 'Hydro terrane')

all_ts <- rbind(ss %>%
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
  as_tibble()
g_ss <- all_ts %>%
  mutate(Model = factor(Model, levels = c('Observed',"Single-site","Clustered","Hydro terrane","Multi-site"))) %>%
  ggplot(aes(x = DateTime, y = Preds, color = Model))+
  geom_line()+
  scale_color_manual(values = c('black','#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  theme_bw()+
  ylab('[NO3-N] (mg/L)')+
  xlab('')+
  #geom_vline(xintercept = ss[ss$`Train/Val/Test` == 'Testing',]$DateTime[1])+
  ggtitle(paste0(station_nm,' | Cluster ', cluster,' | Hydro Terrane ',hydro_terrane))+
  theme(legend.position="none",
  panel.grid.major = element_blank(), panel.grid.minor = element_blank())

cdf <- all_ts %>%
  filter(!is.na(Preds)) %>%
  mutate(Model = factor(Model, levels = c('Observed',"Single-site","Clustered","Hydro terrane","Multi-site"))) %>%
  group_by(Model) %>%
  arrange(Preds) %>%
  mutate(idx = row_number(Model)/sum(!is.na(Preds))) %>%
  ggplot(aes(y = idx, x = Preds, col = Model))+
  geom_line()+
  theme_bw()+
  scale_color_manual(values = c('black','#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  ylab('CDF')+
  xlab('[NO3-N] (mg/L)')+
  theme(legend.position="none",
  panel.grid.major = element_blank(), panel.grid.minor = element_blank())

nrmse_bpl <- all_models %>%
  filter(site_no == site) %>%
  mutate(Model = factor(run, levels = c("Single-site","Clustered","Hydro terrane","Multi-site"))) %>%
  ggplot(aes(x = Model, y = Testing_RMSE, fill = Model))+
  geom_bar(stat = 'identity', color = 'black')+
  scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  theme_bw()+
  ylab('RMSE (mg/L)')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf(paste0('04_analysis/figs/site_summary/',station_nm,'.pdf'), height = 7, width = 10)
layout <- as.matrix(cbind(c(1,1,2), c(1,1,3)))
gridExtra::grid.arrange(g_ss, cdf, rmse_bpl, layout_matrix = layout)
dev.off()
}


##########
#Compare where multi-site models are doing better
best_non_ss_models <- all_models %>%
  filter(run != 'Single-site') %>%
  group_by(site_no) %>%
  arrange(Testing_RMSE, .by_group = TRUE) %>%
  summarise(site_no = first(site_no), NSE = first(Testing_NSE), NRMSE = first(Testing_NRMSE),
            run = first(run), cluster = first(cluster), 
            hydro_terrane = first(hydro_terrane),
            lat = first(lat), long = first(long))

best_models_char <- best_non_ss_models %>%
  mutate(color = if_else(run == 'Single-site', '#00bbf9',
                         if_else(run == 'Clustered', '#ff595e',
                                 if_else(run == 'Hydro terrane', '#ffca3a','#00f5d4')))) %>%
  left_join(basin_char, by = 'site_no')

improvement_ms_models <- all_models %>%
  select(site_no, Testing_NRMSE, run) %>%
  pivot_wider(names_from = run, values_from = Testing_NRMSE) %>%
  left_join(best_models_char, by = ('site_no')) %>%
  mutate(improvement = `Single-site`-`NRMSE`) %>%
  mutate(run = as.character(run)) %>%
  mutate(run = if_else(improvement<0, 'Single-site', run)) %>%
  mutate(run = factor(run, levels = c('Single-site','Clustered','Hydro terrane','Multi-site')))


for(i in (15:69)){
print(i)
print(colnames(improvement_ms_models)[i])
tmp <- data.frame(var = improvement_ms_models[[i]], 
                    improvement = improvement_ms_models[["improvement"]],
                    run = improvement_ms_models[['run']])
  
var_plot <- ggplot(tmp, aes(x = var, y = improvement, fill = run)) +
  geom_hline(yintercept = 0, color = 'darkgray', linetype="dashed", size=1)+
  geom_point(shape = 21, size = 4, alpha = 0.8)+
  scale_fill_manual(values = c('#00bbf9','#ff595e','#ffca3a','#00f5d4'))+
  #xlim(0,150)+
  ylim(-50,50)+
  theme_bw()+
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #legend.position = 'right',
        title = element_text(size = 18))+
  ylab('Improvement in NRMSE')+
  xlab(colnames(improvement_ms_models)[i])+
  ggtitle(colnames(improvement_ms_models)[i])

jpeg(paste0('04_analysis/figs/improvement_plots/',colnames(improvement_ms_models)[i],'.jpeg'))
print(var_plot)
dev.off()
}

  
#########################################################################
#Compare two models

two_runs <- rbind(
read_csv('04_analysis/out/Run_01_230420_All_Features_ensemble_results_2023-04-21.csv') %>%
  mutate(run = 'a-Gloabl No log', cluster = NA),
read_csv('04_analysis/out/Run_03_230423_Discharge_l10_ensemble_results_2023-04-26.csv') %>%
  mutate(run = 'b-Global Log discharge', cluster = NA),
read_csv('04_analysis/out/Run_00_Full_230130_ensemble_results_2023-02-24.csv') %>%
  mutate(run = 'c-Single-site No log', cluster = NA),
read_csv('04_analysis/out/Run_01_Baseline_230422_Discharge_l10_ensemble_results_2023-04-26.csv') %>%
  mutate(run = 'd-Single-site Log discharge', cluster = NA),
read_csv('04_analysis/out/Run_06_C_230208_ensemble_results_2023-02-24.csv') %>%
  mutate(run = 'e-Cluster No log') %>%
  rename('lat' = 'PHYS_LAT', 'long' = 'PHYS_LONG'),
read_csv('04_analysis/out/Run_05_230424_Cluster_Discharge_l10_ensemble_results_2023-04-26.csv') %>%
  mutate(run = 'f-Cluster Log discharge') %>%
  rename('lat' = 'PHYS_LAT', 'long' = 'PHYS_LONG'),
read_csv('04_analysis/out/Run_03_HT_230202_ensemble_results_2023-02-24.csv') %>%
  mutate(run = 'g-Hydro Terrane No log', cluster = NA) %>%
  rename('lat' = 'PHYS_LAT', 'long' = 'PHYS_LONG'),
read_csv('04_analysis/out/Run_06_230424_HydroTerrane_Discharge_l10_ensemble_results_2023-04-26.csv') %>%
  mutate(run = 'h-Hydro Terrane Log discharge', cluster = NA) %>%
  rename('lat' = 'PHYS_LAT', 'long' = 'PHYS_LONG')


)

two_runs %>%
  ggplot(aes(x = run, y = Testing_PBIAS, fill = run))+
  geom_boxplot()+
  ylim(-0,100)+
  scale_fill_manual(values = c('#00f5d4','#00f5d4','#00bbf9','#00bbf9','#ff595e','#ff595e','#ffca3a','#ffca3a'))+
  theme(legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(0.75, 'cm'),
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


two_runs %>%
  group_by(run) %>%
  summarise(med_RMSE = median(Testing_RMSE), med_NSE = median(Testing_NSE), med_PBIAS = median(Testing_PBIAS),
              med_r = median(Testing_r), med_KGE = median(Testing_KGE))


plot(two_runs[two_runs$run == 'Cluster No log',]$Testing_KGE, two_runs[two_runs$run == 'Cluster Log discharge',]$Testing_KGE, 
     xlim = c(-1,1), ylim = c(-1,1), xlab = 'KGE No log', ylab = 'KGE Log', main = 'Effect of taking the log of Discharge', las = 1)
abline(0,1, col = 'red')



