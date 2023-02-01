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
# install.packages('stringr')
library(stringr)
#####
#===================================================================================#

basin_char <- read_csv('04_analysis/out/basin_char_w_clusters_hydroterranes_221005.csv')
sites <- basin_char$site_no
reps <- 10

single_site_run_summary <- data.frame()


for (i in 1:length(sites)){
  site_temp <- read_csv(file.path('03_model/out/single_site/Run_00_Full_230130/Rep_00',sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
  for (j in 1:reps){
    rep <- str_pad(j-1, 2, pad = '0')
    site_rep_temp <- read_csv(file.path('03_model/out/single_site/Run_00_Full_230130',paste0('Rep_',rep),sites[i],'ModelResults.csv'))[,"Predicted"]
    site_temp <- cbind(site_temp,site_rep_temp) 
  }
  site_temp_summary <- tibble(Set = site_temp$`Train/Val/Test`, Labeled = site_temp$Labeled, Predicted_mean = rowMeans(site_temp[grepl( "Predicted" , names( site_temp ) )])) %>%
    group_by(Set) %>%
    summarise(RMSE = hydroGOF::rmse(Predicted_mean, Labeled), NRMSE = hydroGOF::nrmse(Predicted_mean, Labeled), 
              NSE = NSE(Predicted_mean, Labeled), r = rPearson(Predicted_mean, Labeled),
              PBIAS = pbias(Predicted_mean, Labeled), KGE = KGE(Predicted_mean, Labeled)) %>%
    pivot_wider(values_from = RMSE:KGE, names_from = Set, names_glue = "{Set}_{.value}") %>%
    mutate(site_no = sites[i])
  single_site_run_summary <- rbind(single_site_run_summary, site_temp_summary)
}

single_site_run_summary$lat <- basin_char$PHYS_LAT
single_site_run_summary$long <- basin_char$PHYS_LONG
  
write_csv(single_site_run_summary, '04_analysis/out/single_site_ensemble_run_summary_230130.csv')

#multi-site ensembles
multi_site_run_summary <- data.frame()


for (i in 1:length(sites)){
  site_temp <- read_csv(file.path('~/Documents/GitHub/no3_ml_proj/03_model/out/multi_site/Run_00_Full_230131/Rep_00',sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
  for (j in 1:reps){
    rep <- str_pad(j-1, 2, pad = '0')
    site_rep_temp <- read_csv(file.path('~/Documents/GitHub/no3_ml_proj/03_model/out/multi_site/Run_00_Full_230131',paste0('Rep_',rep),sites[i],'ModelResults.csv'))[,"Predicted"]
    site_temp <- cbind(site_temp,site_rep_temp) 
  }
  site_temp_summary <- tibble(Set = site_temp$`Train/Val/Test`, Labeled = site_temp$Labeled, Predicted_mean = rowMeans(site_temp[grepl( "Predicted" , names( site_temp ) )])) %>%
    group_by(Set) %>%
    summarise(RMSE = hydroGOF::rmse(Predicted_mean, Labeled), NRMSE = nrmse(Predicted_mean, Labeled), 
              NSE = NSE(Predicted_mean, Labeled), r = rPearson(Predicted_mean, Labeled),
              PBIAS = pbias(Predicted_mean, Labeled), KGE = KGE(Predicted_mean, Labeled)) %>%
    pivot_wider(values_from = RMSE:KGE, names_from = Set, names_glue = "{Set}_{.value}") %>%
    mutate(site_no = sites[i])
  multi_site_run_summary <- rbind(multi_site_run_summary, site_temp_summary)
}

multi_site_run_summary$lat <- basin_char$PHYS_LAT
multi_site_run_summary$long <- basin_char$PHYS_LONG

write_csv(multi_site_run_summary, '04_analysis/out/multi_site_ensemble_full_run_MS_summary_230131.csv')


#clustered-site 01 (7 clusters) ensembles
clusters <- basin_char$cluster_01
cluster_run_summary <- data.frame()


for(k in 1:length(unique(clusters))){
  cluster <- paste0(0,unique(clusters)[k])
  cluster_sites <- basin_char[basin_char$cluster_01 == unique(clusters)[k],]$site_no

  for (i in 1:length(cluster_sites)){
    site_temp <- read_csv(file.path('03_model/out/multi_site/Run_08_C/Rep_00',paste0('Cluster_',cluster),cluster_sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
    for (j in 1:length(reps)){
      site_rep_temp <- read_csv(file.path('03_model/out/multi_site/Run_08_C',paste0('Rep_',reps[j]),paste0('Cluster_',cluster),cluster_sites[i],'ModelResults.csv'))[,"Predicted"]
      site_temp <- cbind(site_temp,site_rep_temp) 
    }
    site_temp_summary <- tibble(Set = site_temp$`Train/Val/Test`, Labeled = site_temp$Labeled, Predicted_mean = rowMeans(site_temp[,3:7])) %>%
      group_by(Set) %>%
      summarise(RMSE = hydroGOF::rmse(Predicted_mean, Labeled), NRMSE = nrmse(Predicted_mean, Labeled), 
                NSE = NSE(Predicted_mean, Labeled), r = rPearson(Predicted_mean, Labeled),
                PBIAS = pbias(Predicted_mean, Labeled), KGE = KGE(Predicted_mean, Labeled)) %>%
      pivot_wider(values_from = RMSE:KGE, names_from = Set, names_glue = "{Set}_{.value}") %>%
      mutate(site_no = cluster_sites[i], cluster = cluster)
    cluster_run_summary <- rbind(cluster_run_summary, site_temp_summary)
}
}
cluster_run_summary <- merge(cluster_run_summary, basin_char[,c('site_no','PHYS_LAT','PHYS_LONG')], by = 'site_no')

write_csv(cluster_run_summary, '04_analysis/out/cluster_ensemble_full_run_08_C_summary.csv')



#hydro terrane ensembles
hydro_terranes <- basin_char$hydro_terrane
ht_run_summary <- data.frame()

for(k in 1:length(unique(hydro_terranes))){
  ht <- unique(hydro_terranes)[k]
  cluster_sites <- basin_char[basin_char$hydro_terrane == ht,]$site_no
  
  for (i in 1:length(cluster_sites)){
    site_temp <- read_csv(file.path('03_model/out/multi_site/Run_08_HT/Rep_00',paste0('Terrane_',ht),cluster_sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
    for (j in 1:length(reps)){
      site_rep_temp <- read_csv(file.path('03_model/out/multi_site/Run_08_HT',paste0('Rep_',reps[j]),paste0('Terrane_',ht),cluster_sites[i],'ModelResults.csv'))[,"Predicted"]
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

ht_run_summary <- merge(ht_run_summary, basin_char[,c('site_no','PHYS_LAT','PHYS_LONG')], by = 'site_no')

write_csv(ht_run_summary, '04_analysis/out/hydro_terrane_ensemble_full_run_08_HT_summary.csv')

