#===================================================================================#
# NOTES:         cluster analysis                                                   #
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-08-05                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
# install.packages('NbClust')
library(NbClust)
# install.packages('factoextra')
library(factoextra)
# install.packages('clValid')
library(clValid)
# install.packages('maps')
library(maps)
# install.packages('RColorBrewer')
library(RColorBrewer)
# install.packages(sf)
library(sf)

#####
#===================================================================================#

#read in the basin characteristics which include all static characteristics including those
#that were not included in modeling and those that were calculated like cq slope
basin_char_clean <- read_csv('04_analysis/out/basin_char_calc_clean.csv')

#read in a file with the site names
site_names <- read_csv('01_fetch/out/site_list_220507.csv') %>% dplyr::select(site_no, station_nm)

basin_char_clean <- merge(basin_char_clean, site_names, by = 'site_no')

#read in feature importance
char_feat_imp <- read_csv('04_analysis/out/multi_site_ensemble_feature_importance.csv') %>%
  filter(!feat %in% c('Discharge','Precip','TempMax','TempMin','SolarRad')) %>%
  arrange(desc(feat_imp_mean)) %>%
  filter(feat_imp_mean > 0.25) %>%
  pull(feat)

#find the cleaned up names of the features with higher feature importances scores
names_lookup <- read_csv('04_analysis/out/basin_char_names_lookup.csv') %>%
  filter(Names %in% char_feat_imp | !is.na(Calculated)) %>%
  #i am going to remove both of these variables for clustering because a single site is an outlier and 
  #it makes clustering produce unhelpful results
  #filter(!Names_Clean %in% c('ANTHRO_NID_STORAGE2013','ANTHRO_MAJOR2013')) %>%
  pull(Names_Clean) 

#several of the reservoir basin variables are dominating the clustering because
#there is one site that is an outlier
#for ANTHRO_NID_SOTRAGE2013 it is Delaware at Trenton
basin_char_clean %>% 
  select(site_no, station_nm, ANTHRO_NID_STORAGE2013) %>% 
  arrange(desc(ANTHRO_NID_STORAGE2013)) %>%
  head()
#for ANTHRO_MAJOR2013 it is the Potomac River
basin_char_clean %>% 
  select(site_no, station_nm, ANTHRO_MAJOR2013) %>% 
  arrange(desc(ANTHRO_MAJOR2013)) %>%
  head()



used_for_modeling <- read_csv('04_analysis/out/basin_char_names_lookup.csv') %>%
  filter(Names %in% char_feat_imp) %>%
  pull(Names)

#scale and center the data
basin_char_scaled <- as.matrix(basin_char_clean %>%
                                 dplyr::select(all_of(names_lookup)) %>%
                                 scale())
rownames(basin_char_scaled) <- basin_char_clean$site_no


basin_char_cl <- clValid(basin_char_scaled, nClust = 2:8, clMethods = c('kmeans',"hierarchical"), validation = c('internal','stability'))

#eclust <- eclust(basin_char_scaled, k = 6, FUNcluster = 'hclust', hc_metric = 'euclidean')

summary(basin_char_cl)
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(basin_char_cl, legend=FALSE)
plot(nClusters(basin_char_cl),measures(basin_char_cl,"Dunn")[,,1],type="n",axes=F, xlab="",ylab="")
legend("center", clusterMethods(basin_char_cl), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

plot(basin_char_cl)


cluster_assignment_all_2 <- basin_char_cl@clusterObjs$kmeans$`2`$cluster
cluster_assignment_all_3 <- basin_char_cl@clusterObjs$kmeans$`3`$cluster
cluster_assignment_all_4 <- basin_char_cl@clusterObjs$kmeans$`4`$cluster
cluster_assignment_all_5 <- basin_char_cl@clusterObjs$kmeans$`5`$cluster
cluster_assignment_all_6 <- basin_char_cl@clusterObjs$kmeans$`6`$cluster
cluster_assignment_all_7 <- basin_char_cl@clusterObjs$kmeans$`7`$cluster
cluster_assignment_all_8 <- basin_char_cl@clusterObjs$kmeans$`8`$cluster

#make a data frame with cluster assignment
cluster_assig <- basin_char_clean %>%
  dplyr::select(site_no, PHYS_LAT, PHYS_LONG) %>%
  mutate(cluster = factor(cluster_assignment_all_8))


states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_cluster <- st_as_sf(cluster_assig, coords = c('PHYS_LONG','PHYS_LAT'), crs = 4326)


ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_cluster, aes(fill = cluster), shape = 21, size = 3, alpha = 0.8) +
  scale_colour_brewer(palette = "Spectral")+
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  #guides(fill=guide_colourbar(title="Cluster assignment"))+
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

table(cluster_assignment_all_2)
table(cluster_assignment_all_3)
table(cluster_assignment_all_4)
table(cluster_assignment_all_5)
table(cluster_assignment_all_6)
table(cluster_assignment_all_7)
table(cluster_assignment_all_8)

#Do a second round of clustering on the really big cluster, cluster number 1

cluster_one <- cluster_assignment_all_3[cluster_assignment_all_3 == 1] %>% names()

basin_char_cl1 <- basin_char_scaled[cluster_one,]

cl1_cl <- clValid(basin_char_cl1, nClust = 2:8, clMethods = c('kmeans',"hierarchical"), validation = c('internal','stability'))

summary(cl1_cl)
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(cl1_cl, legend=FALSE)
plot(nClusters(cl1_cl),measures(cl1_cl,"Dunn")[,,1],type="n",axes=F, xlab="",ylab="")
legend("center", clusterMethods(cl1_cl), col=1:9, lty=1:9, pch=paste(1:9))

cluster_one_assignment <- cl1_cl@clusterObjs$kmeans$`4`$cluster

#make a data frame with cluster assignment
cluster_assig <- basin_char_clean %>%
  dplyr::select(site_no, PHYS_LAT, PHYS_LONG) %>%
  filter(site_no %in% cluster_one) %>%
  mutate(cluster = factor(cluster_one_assignment))


states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_cluster <- st_as_sf(cluster_assig, coords = c('PHYS_LONG','PHYS_LAT'), crs = 4326)


ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_cluster, aes(fill = cluster), shape = 21, size = 3, alpha = 0.8) +
  scale_colour_brewer(palette = "Spectral")+
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  #guides(fill=guide_colourbar(title="Cluster assignment"))+
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

table(cluster_one_assignment)


#merge the cluster assignments together and save as a csv

ca_all <- cluster_assignment_all_3 %>% 
  as_tibble() %>%
  mutate(site_no = names(cluster_assignment_all_3)) %>%
  rename('cl_1' = 'value') %>%
  left_join(as_tibble(cluster_one_assignment) %>% mutate(site_no = names(cluster_one_assignment))) %>%
  rename('cl_2' = 'value') %>%
  mutate(cl21 = replace(cl_2, is.na(cl_2), cl_1[is.na(cl_2)]*100)) %>%
  dplyr::select(site_no, cl_1, cl21) %>%
  rename('cl_2' = 'cl21') %>%
  mutate(cl_2 = replace(cl_2, cl_2 == 200, 4)) %>%
  mutate(cl_2 = replace(cl_2, cl_2 == 300, 5)) %>%
  mutate(cl_2 = replace(cl_2, cl_2 == 400, 6))


write_csv(ca_all, '04_analysis/out/site_cluster_membership.csv')

basin_char_clusters <- basin_char_clean %>%
  dplyr::select(!`...1`) %>%
  relocate(station_nm, .after = site_no) %>%
  left_join(ca_all[,c('site_no','cl_1','cl_2')]) %>%
  rename('cluster_01' = 'cl_1') %>%
  rename('cluster_02' = 'cl_2')
  

hydro_terranes <- read_csv('04_analysis/out/basin_char_w_clusters_6_hydro_terranes.csv') %>%
  dplyr::select(site_no, hydro_terrane)

clusters_ht <- merge(basin_char_clusters, hydro_terranes, by = 'site_no')

write_csv(clusters_ht, '04_analysis/out/basin_char_w_clusters_hydroterranes_221005.csv')


#View final clusters
cluster_assig <- clusters_ht %>%
  dplyr::select(site_no, long, lat, cluster_02, hydro_terrane) %>%
  #filter(site_no %in% cluster_one) %>%
  mutate(cluster = factor(cluster_02))

states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_cluster <- st_as_sf(cluster_assig, coords = c('long','lat'), crs = 4326)


ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_cluster, aes(fill = hydro_terrane), shape = 21, size = 3, alpha = 0.8) +
  scale_colour_brewer(palette = "Spectral")+
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  #guides(fill=guide_colourbar(title="Cluster assignment"))+
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

table(cluster_one_assignment)


####################################
#PCA plot with biplot for cluster analysis
#read in the basin characteristics which include all static characteristics including those
#that were not included in modeling and those that were calculated like cq slope
basin_char_clean <- read_csv('04_analysis/out/basin_char_calc_clean.csv')

#read in a file with the site names
site_names <- read_csv('01_fetch/out/site_list_220507.csv') %>% dplyr::select(site_no, station_nm)

basin_char_clean <- merge(basin_char_clean, site_names, by = 'site_no')

#read in feature importance
char_feat_imp <- read_csv('04_analysis/out/multi_site_ensemble_feature_importance.csv') %>%
  filter(!feat %in% c('Discharge','Precip','TempMax','TempMin','SolarRad')) %>%
  arrange(desc(feat_imp_mean)) %>%
  filter(feat_imp_mean > 0.25) %>%
  pull(feat)

#find the cleaned up names of the features with higher feature importances scores
names_lookup <- read_csv('04_analysis/out/basin_char_names_lookup.csv') %>%
  filter(Names %in% char_feat_imp | !is.na(Calculated)) %>%
  #i am going to remove both of these variables for clustering because a single site is an outlier and 
  #it makes clustering produce unhelpful results
  #filter(!Names_Clean %in% c('ANTHRO_NID_STORAGE2013','ANTHRO_MAJOR2013')) %>%
  pull(Names_Clean) 

#several of the reservoir basin variables are dominating the clustering because
#there is one site that is an outlier
#for ANTHRO_NID_SOTRAGE2013 it is Delaware at Trenton
basin_char_clean %>% 
  select(site_no, station_nm, ANTHRO_NID_STORAGE2013) %>% 
  arrange(desc(ANTHRO_NID_STORAGE2013)) %>%
  head()
#for ANTHRO_MAJOR2013 it is the Potomac River
basin_char_clean %>% 
  select(site_no, station_nm, ANTHRO_MAJOR2013) %>% 
  arrange(desc(ANTHRO_MAJOR2013)) %>%
  head()



used_for_modeling <- read_csv('04_analysis/out/basin_char_names_lookup.csv') %>%
  filter(Names %in% char_feat_imp) %>%
  pull(Names)


basin_char_scaled <- as.matrix(basin_char_clean %>%
                                 dplyr::select(all_of(names_lookup)))
rownames(basin_char_scaled) <- basin_char_clean$site_no


#cluster internal stability metrics
# stab <- clValid(basin_char_scaled, 2:10, clMethods=c("hierarchical","kmeans"),validation="stability")
# optimalScores(stab)
# par(mfrow=c(2,2),mar=c(4,4,3,1))
# plot(stab, measure=c("APN","AD","ADM"),legend=FALSE)
# plot(nClusters(stab),measures(stab,"APN")[,,1],type="n",axes=F,xlab="",ylab="")
# legend("center", clusterMethods(stab), col=1:9, lty=1:9, pch=paste(1:9))
# par(op)

# t <- read_csv('04_analysis/out/basin_char_w_clusters_6.csv')
# t$cluster[1:46] <- cluster_assignment
# 
# basin_char_cl <- clValid(basin_char_scaled, nClust = 2:10, clMethods = c('kmeans',"hierarchical"))
# 
# 
# hclust_avg <- basin_char_cl@clusterObjs$hierarchical
# cut_avg <- cutree(hclust_avg, k = 2)
# plot(hclust_avg)
# rect.hclust(hclust_avg , k = 6, border = 2:3)
# abline(h = 5, col = 'red')

# 
# suppressPackageStartupMessages(library(dendextend))
# avg_dend_obj <- as.dendrogram(hclust_avg)
# avg_col_dend <- color_branches(avg_dend_obj, h = 2)
# plot(avg_col_dend)
# 
# dist_matrix <- dist(basin_char_scaled, method = 'euclidean')
# hclust_avg <- hclust(dist_matrix, method = 'average')
# plot(hclust_avg)
# cut_avg <- cutree(hclust_avg, k = 2)
# 
