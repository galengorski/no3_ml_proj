#===================================================================================#
# NOTES: this script is for analyzing the single site model runs with 
# full data
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-08-07                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('hydroGOF')
library(hydroGOF)
# install.packages('tidyverse')
library(tidyverse)
#####
#===================================================================================#

basin_char <- read_csv('01_fetch/out/site_list_220507.csv')
sites <- basin_char$site_no
reps <- c('00','01','02','03','04')

single_site_run_summary <- data.frame()


for (i in 1:length(sites)){
  site_temp <- read_csv(file.path('03_model/out/single_site/Run_00_Full/Rep_00',sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
  for (j in 1:length(reps)){
    site_rep_temp <- read_csv(file.path('03_model/out/single_site/Run_00_Full',paste0('Rep_',reps[j]),sites[i],'ModelResults.csv'))[,"Predicted"]
    site_temp <- cbind(site_temp,site_rep_temp) 
  }
  site_temp_summary <- tibble(Set = site_temp$`Train/Val/Test`, Labeled = site_temp$Labeled, Predicted_mean = rowMeans(site_temp[,3:7])) %>%
    group_by(Set) %>%
    summarise(RMSE = hydroGOF::rmse(Predicted_mean, Labeled), NRMSE = nrmse(Predicted_mean, Labeled), 
              NSE = NSE(Predicted_mean, Labeled), r = rPearson(Predicted_mean, Labeled),
              PBIAS = pbias(Predicted_mean, Labeled), KGE = KGE(Predicted_mean, Labeled)) %>%
    pivot_wider(values_from = RMSE:KGE, names_from = Set, names_glue = "{Set}_{.value}") %>%
    mutate(site_no = sites[i])
  single_site_run_summary <- rbind(single_site_run_summary, site_temp_summary)
}

single_site_run_summary$lat <- basin_char$dec_lat_va
single_site_run_summary$long <- basin_char$dec_long_va
  
write_csv(single_site_run_summary, '04_analysis/out/single_site_ensemble_run_summary.csv')

#multi-site ensembles
multi_site_run_summary <- data.frame()


for (i in 1:length(sites)){
  site_temp <- read_csv(file.path('03_model/out/multi_site/Run_03/Rep_00',sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
  for (j in 1:length(reps)){
    site_rep_temp <- read_csv(file.path('03_model/out/multi_site/Run_03',paste0('Rep_',reps[j]),sites[i],'ModelResults.csv'))[,"Predicted"]
    site_temp <- cbind(site_temp,site_rep_temp) 
  }
  site_temp_summary <- tibble(Set = site_temp$`Train/Val/Test`, Labeled = site_temp$Labeled, Predicted_mean = rowMeans(site_temp[,3:7])) %>%
    group_by(Set) %>%
    summarise(RMSE = hydroGOF::rmse(Predicted_mean, Labeled), NRMSE = nrmse(Predicted_mean, Labeled), 
              NSE = NSE(Predicted_mean, Labeled), r = rPearson(Predicted_mean, Labeled),
              PBIAS = pbias(Predicted_mean, Labeled), KGE = KGE(Predicted_mean, Labeled)) %>%
    pivot_wider(values_from = RMSE:KGE, names_from = Set, names_glue = "{Set}_{.value}") %>%
    mutate(site_no = sites[i])
  multi_site_run_summary <- rbind(multi_site_run_summary, site_temp_summary)
}

multi_site_run_summary$lat <- basin_char$dec_lat_va
multi_site_run_summary$long <- basin_char$dec_long_va

write_csv(multi_site_run_summary, '04_analysis/out/multi_site_ensemble_full_run_summary.csv')


#clustered-site 01 (4 clusters) ensembles
basin_char_clusters <- read_csv('04_analysis/out/basin_char_w_clusters_220923.csv')
#sites <- basin_char_clusters$site_no
cluster_01 <- basin_char_clusters$cluster_01
#cluster_02 <- basin_char_clusters$cluster_02
cluster_01_run_summary <- data.frame()

for(k in 1:length(unique(cluster_01))){
  cluster <- paste0(0,unique(cluster_01)[k])
  cluster_sites <- basin_char_clusters[basin_char_clusters$cluster_01 == unique(cluster_01)[k],]$site_no

  for (i in 1:length(cluster_sites)){
    site_temp <- read_csv(file.path('03_model/out/multi_site/Run_03C/Rep_00',paste0('Cluster_',cluster),cluster_sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
    for (j in 1:length(reps)){
      site_rep_temp <- read_csv(file.path('03_model/out/multi_site/Run_03C',paste0('Rep_',reps[j]),paste0('Cluster_',cluster),cluster_sites[i],'ModelResults.csv'))[,"Predicted"]
      site_temp <- cbind(site_temp,site_rep_temp) 
    }
    site_temp_summary <- tibble(Set = site_temp$`Train/Val/Test`, Labeled = site_temp$Labeled, Predicted_mean = rowMeans(site_temp[,3:7])) %>%
      group_by(Set) %>%
      summarise(RMSE = hydroGOF::rmse(Predicted_mean, Labeled), NRMSE = nrmse(Predicted_mean, Labeled), 
                NSE = NSE(Predicted_mean, Labeled), r = rPearson(Predicted_mean, Labeled),
                PBIAS = pbias(Predicted_mean, Labeled), KGE = KGE(Predicted_mean, Labeled)) %>%
      pivot_wider(values_from = RMSE:KGE, names_from = Set, names_glue = "{Set}_{.value}") %>%
      mutate(site_no = cluster_sites[i], cluster = cluster)
    cluster_01_run_summary <- rbind(cluster_01_run_summary, site_temp_summary)
}
}
cluster_01_run_summary <- merge(cluster_01_run_summary, basin_char_clusters[,c('site_no','lat','long')], by = 'site_no')

write_csv(cluster_01_run_summary, '04_analysis/out/cluster_01_ensemble_full_run_summary.csv')

#clustered-site 02 (6 clusters) ensembles
basin_char_clusters <- read_csv('04_analysis/out/basin_char_w_clusters_220923.csv')
#sites <- basin_char_clusters$site_no
cluster_02 <- basin_char_clusters$cluster_02
cluster_02_run_summary <- data.frame()

for(k in 1:length(unique(cluster_02))){
  cluster <- paste0(0,unique(cluster_02)[k])
  cluster_sites <- basin_char_clusters[basin_char_clusters$cluster_02 == unique(cluster_02)[k],]$site_no
  
  for (i in 1:length(cluster_sites)){
    site_temp <- read_csv(file.path('03_model/out/multi_site/Run_04C/Rep_00',paste0('Cluster_',cluster),cluster_sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
    for (j in 1:length(reps)){
      site_rep_temp <- read_csv(file.path('03_model/out/multi_site/Run_04C',paste0('Rep_',reps[j]),paste0('Cluster_',cluster),cluster_sites[i],'ModelResults.csv'))[,"Predicted"]
      site_temp <- cbind(site_temp,site_rep_temp) 
    }
    site_temp_summary <- tibble(Set = site_temp$`Train/Val/Test`, Labeled = site_temp$Labeled, Predicted_mean = rowMeans(site_temp[,3:7])) %>%
      group_by(Set) %>%
      summarise(RMSE = hydroGOF::rmse(Predicted_mean, Labeled), NRMSE = nrmse(Predicted_mean, Labeled), 
                NSE = NSE(Predicted_mean, Labeled), r = rPearson(Predicted_mean, Labeled),
                PBIAS = pbias(Predicted_mean, Labeled), KGE = KGE(Predicted_mean, Labeled)) %>%
      pivot_wider(values_from = RMSE:KGE, names_from = Set, names_glue = "{Set}_{.value}") %>%
      mutate(site_no = cluster_sites[i], cluster = cluster)
    cluster_02_run_summary <- rbind(cluster_02_run_summary, site_temp_summary)
  }
}

cluster_02_run_summary <- merge(cluster_02_run_summary, basin_char_clusters[,c('site_no','lat','long')], by = 'site_no')

write_csv(cluster_02_run_summary, '04_analysis/out/cluster_02_ensemble_full_run_summary.csv')

#hydro terrane ensembles
basin_char_clusters <- read_csv('04_analysis/out/basin_char_w_clusters_220923.csv')
#sites <- basin_char_clusters$site_no
hydro_terranes <- basin_char_clusters$hydro_terrane
ht_run_summary <- data.frame()

for(k in 1:length(unique(hydro_terranes))){
  ht <- unique(hydro_terranes)[k]
  cluster_sites <- basin_char_clusters[basin_char_clusters$hydro_terrane == ht,]$site_no
  
  for (i in 1:length(cluster_sites)){
    site_temp <- read_csv(file.path('03_model/out/multi_site/Run_05HT/Rep_00',paste0('Terrane_',ht),cluster_sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
    for (j in 1:length(reps)){
      site_rep_temp <- read_csv(file.path('03_model/out/multi_site/Run_05HT',paste0('Rep_',reps[j]),paste0('Terrane_',ht),cluster_sites[i],'ModelResults.csv'))[,"Predicted"]
      site_temp <- cbind(site_temp,site_rep_temp) 
    }
    site_temp_summary <- tibble(Set = site_temp$`Train/Val/Test`, Labeled = site_temp$Labeled, Predicted_mean = rowMeans(site_temp[,3:7])) %>%
      group_by(Set) %>%
      summarise(RMSE = hydroGOF::rmse(Predicted_mean, Labeled), NRMSE = nrmse(Predicted_mean, Labeled), 
                NSE = NSE(Predicted_mean, Labeled), r = rPearson(Predicted_mean, Labeled),
                PBIAS = pbias(Predicted_mean, Labeled), KGE = KGE(Predicted_mean, Labeled)) %>%
      pivot_wider(values_from = RMSE:KGE, names_from = Set, names_glue = "{Set}_{.value}") %>%
      mutate(site_no = cluster_sites[i], cluster = ht)
    ht_run_summary <- rbind(ht_run_summary, site_temp_summary)
  }
}

ht_run_summary <- merge(ht_run_summary, basin_char_clusters[,c('site_no','lat','long')], by = 'site_no')

write_csv(ht_run_summary, '04_analysis/out/hydro_terrane_ensemble_full_run_summary.csv')

