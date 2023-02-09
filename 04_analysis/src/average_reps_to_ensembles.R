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


gh <- '~/Documents/GitHub/no3_ml_proj/'
gd <- '~/galengorski@berkeley.edu - Google Drive/My Drive/ESDL Postdoc/02_Projects/no3_ml_proj/'

basin_char <- read_csv(file.path(gh, '04_analysis/out/basin_char_w_clusters_hydroterranes_230208.csv'))
sites <- basin_char$site_no
reps <- 10

ss_run_id <- 'Run_00_Full_230130'
ms_run_id <- 'Run_01_230201_Baseline'
cl_run_id <- 'Run_06_C_230208'
ht_run_id <- 'Run_03_HT_230202'

single_site_run_summary <- data.frame()


for (i in 1:length(sites)){
  site_temp <- read_csv(file.path(paste0(gd,'03_model/out/single_site/',ss_run_id,'/Rep_00'),sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
  for (j in 1:reps){
    rep <- str_pad(j-1, 2, pad = '0')
    site_rep_temp <- read_csv(file.path(paste0(gd,'03_model/out/single_site/',ss_run_id),paste0('Rep_',rep),sites[i],'ModelResults.csv'))[,"Predicted"]
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
  
write_csv(single_site_run_summary, file.path(gd,paste0('04_analysis/out/',ss_run_id,'_ensemble_results_',Sys.Date(),'.csv')))

#multi-site ensembles
multi_site_run_summary <- data.frame()


for (i in 1:length(sites)){
  site_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',ms_run_id,'/Rep_00'),sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
  for (j in 1:reps){
    rep <- str_pad(j-1, 2, pad = '0')
    site_rep_temp <- read_csv(file.path(gh,'03_model/out/multi_site/',ms_run_id,paste0('Rep_',rep),sites[i],'ModelResults.csv'))[,"Predicted"]
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

write_csv(multi_site_run_summary, file.path(gd,paste0('04_analysis/out/',ms_run_id,'_ensemble_results_',Sys.Date(),'.csv')))


#clustered-site models
clusters <- basin_char$cluster
cluster_run_summary <- data.frame()


for(k in 1:length(unique(clusters))){
  cluster <- paste0(0,unique(clusters)[k])
  cluster_sites <- basin_char[basin_char$cluster == unique(clusters)[k],]$site_no

  for (i in 1:length(cluster_sites)){
    site_temp <- read_csv(file.path(paste0(gh, '03_model/out/multi_site/',cl_run_id,'/Rep_00'),paste0('Cluster_',cluster),cluster_sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
    for (j in 1:reps){
      rep <- str_pad(j-1, 2, pad = '0')
      site_rep_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',cl_run_id),paste0('Rep_',rep),paste0('Cluster_',cluster),cluster_sites[i],'ModelResults.csv'))[,"Predicted"]
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

write_csv(cluster_run_summary, file.path(gd,paste0('04_analysis/out/',cl_run_id,'_ensemble_results_',Sys.Date(),'.csv')))



#hydro terrane ensembles
hydro_terranes <- basin_char$hydro_terrane
ht_run_summary <- data.frame()

for(k in 1:length(unique(hydro_terranes))){
  ht <- unique(hydro_terranes)[k]
  cluster_sites <- basin_char[basin_char$hydro_terrane == ht,]$site_no
  
  for (i in 1:length(cluster_sites)){
    site_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',ht_run_id,'/Rep_00'),paste0('Terrane_',ht),cluster_sites[i],'ModelResults.csv'))[,c("Labeled","Train/Val/Test")]
    for (j in 1:reps){
      rep <- str_pad(j-1, 2, pad = '0')
      site_rep_temp <- read_csv(file.path(paste0(gh,'03_model/out/multi_site/',ht_run_id),paste0('Rep_',rep),paste0('Terrane_',ht),cluster_sites[i],'ModelResults.csv'))[,"Predicted"]
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

write_csv(ht_run_summary, file.path(gd,paste0('04_analysis/out/',ht_run_id,'_ensemble_results_',Sys.Date(),'.csv')))

