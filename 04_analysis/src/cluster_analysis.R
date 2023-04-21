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
# install.packages('dplyr')
library(dplyr)
# install.packages('tidyr')
library(tidyr)
# install.packages('readr')
library(readr)
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
# install.packages('sf')
library(sf)
# install.packages('cowplot')
library(cowplot)
# install.packages('scales')
library(scales)
# install.packages('ggspatial')
library(ggspatial)
# install.packages('factoextra')
library(factoextra)
# install.packages('FactoMineR')
library(FactoMineR)

#####
#===================================================================================#

#read in the basin characteristics which include all static characteristics including those
#that were not included in modeling and those that were calculated like cq slope
basin_char_clean <- read_csv('04_analysis/out/basin_char_calc_clean.csv')

#read in a file with the site names
site_names <- read_csv('01_fetch/out/site_list_220507.csv') %>% dplyr::select(site_no, station_nm)

basin_char_clean <- merge(basin_char_clean, site_names, by = 'site_no') %>%
  tibble()

############################################
#Attempt with updated feature importance and using all features available for clustering
#read in updated feature importance
feat_imp_u <- read_csv('~/Documents/GitHub/no3_ml_proj/04_analysis/out/multi_site_ensemble_feature_importanceRun_00_Full_230131.csv') %>%
  filter(!feat %in% c('Discharge','Precip','TempMax','TempMin','SolarRad')) %>%
  filter(feat_imp_mean > 0.10) %>%
  pull(feat)

#find the cleaned up names of the features with higher feature importances scores
names_lookup_u <- read_csv('04_analysis/out/basin_char_names_lookup_formatted.csv') %>%
  filter(Names %in% feat_imp_u | !is.na(Calculated)) %>%
  pull(Names_Clean) 

#scale and center the data
basin_char_scaled_u <- as.matrix(basin_char_clean %>%
                                 dplyr::select(all_of(names_lookup_u)) %>%
                                                                 scale())
#add in site numbers as row names
rownames(basin_char_scaled_u) <- basin_char_clean$site_no

#calculate the clusters from 2 to 8 clusters
basin_char_cl_u <- clValid(basin_char_scaled_u, nClust = 2:8, clMethods = c('kmeans',"hierarchical"), validation = c('internal','stability'))

#choosing 6 clusters here
cluster_assignment_all_u <- basin_char_cl_u@clusterObjs$kmeans$`6`$cluster


#plot PCA to view how similar clusters are
res.pca <- PCA(basin_char_scaled_u, graph = FALSE)

fviz_screeplot(res.pca, addlabels = TRUE)

#cols <- c('#F8766D','#C49A00','#53B400','#00C094','#00B6EB','#A58AFF','#FB61D7')

cl_biplot <- fviz_pca_biplot(res.pca, 
                             fill.ind = factor(cluster_assignment_all_u),# palette = 'Spectral',
                             pointshape = 21, pointsize = 3.5, mean.point = FALSE, 
                             label = 'var', legend.title = 'Cluster', repel = TRUE, 
                             select.var = list(contrib = 12), ggtheme = theme_bw(), col.var = 'lightgray', xlab = 'PC1', ylab = 'PC2',
                             title = 'Principal component analysis of clustered sites', alpha = 0.85)+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())#,
        #legend.position = 'none')
cl_biplot

basin_char_cl_u@clusterObjs$kmeans$`2`$cluster %>% table()
basin_char_cl_u@clusterObjs$kmeans$`3`$cluster %>% table()
basin_char_cl_u@clusterObjs$kmeans$`4`$cluster %>% table()
basin_char_cl_u@clusterObjs$kmeans$`5`$cluster %>% table()
basin_char_cl_u@clusterObjs$kmeans$`6`$cluster %>% table()
# basin_char_cl_u@clusterObjs$kmeans$`7`$cluster %>% table()
# basin_char_cl_u@clusterObjs$kmeans$`8`$cluster %>% table()

#make a data frame with cluster assignment
cluster_assig <- basin_char_clean %>%
  dplyr::select(site_no, PHYS_LAT, PHYS_LONG) %>%
  mutate(cluster = factor(cluster_assignment_all_u))


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


#merge the cluster assignments together with the basin characteristics and save as a csv
cluster_assig$cluster %>% table()

#assign site 1 to a cluster, based on the PCA we assign it to cluster 5 which is closest to it
#in PCA space
#and reassign the cluster numbers so that they begin with 1
cluster_reassig <- cluster_assig %>%
  mutate(cluster = recode(cluster, '1' = '5', '6' = '1'))

#check to make sure the reassignment worked (cluster 1 should have 15, 2 has 3, 3 has 17, 4 has 5, and 5 has 6)
cluster_reassig$cluster %>% table()

basin_char_clusters <- basin_char_clean %>%
  dplyr::select(!`...1`) %>%
  relocate(station_nm, .after = site_no) %>%
  left_join(cluster_reassig[,c('site_no','cluster')])
  
hydro_terranes <- read_csv('04_analysis/out/basin_char_w_clusters_6_hydro_terranes.csv') %>%
  dplyr::select(site_no, hydro_terrane)

clusters_ht <- merge(basin_char_clusters, hydro_terranes, by = 'site_no') %>%
  tibble()

write_csv(clusters_ht, '04_analysis/out/basin_char_w_clusters_hydroterranes_230421.csv')





#######################################################################################
#------Everything below this line is code chunks that may be useful------#
#######################################################################################
#View final clusters

clusters_ht <- read_csv('04_analysis/out/basin_char_w_clusters_hydroterranes_221005.csv')
cluster_assig <- clusters_ht %>%
  dplyr::select(site_no, PHYS_LONG, PHYS_LAT, cluster_01, hydro_terrane) %>%
  mutate(cluster = factor(cluster_01))

states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = 4326)
states_map <- maps::map("state", plot = FALSE, fill = FALSE)
site_cluster <- st_as_sf(cluster_assig, coords = c('PHYS_LONG','PHYS_LAT'), crs = 4326)


cl_map <- ggplot(data = states) +
  geom_sf()+
  geom_sf(data = site_cluster, aes(fill = cluster), shape = 21, size = 3.5, alpha = 0.8) +
  coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
  theme_bw()+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill = guide_legend(nrow = 1, override.aes = list(size=5)))+
  scale_fill_manual(values = cols) +
  #guides(fill = guide_legend(nrow = 1))+
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
#cl_map
table(cluster_assig$cluster)


####################################
#PCA plot with biplot for cluster analysis
#from: https://medium.com/@RaharditoDP/principal-component-analysis-with-biplot-analysis-in-r-ee39d17096a1
library(factoextra)
library(FactoMineR)
show_col(hue_pal()(7))

basin_char_for_pca <- as.matrix(basin_char_clean %>%
                                  dplyr::select(all_of(names_lookup)))
rownames(basin_char_for_pca) <- basin_char_clean$site_no

res.pca <- PCA(basin_char_for_pca, graph = FALSE)

fviz_screeplot(res.pca, addlabels = TRUE)

cols <- c('#F8766D','#C49A00','#53B400','#00C094','#00B6EB','#A58AFF','#FB61D7')

cl_biplot <- fviz_pca_biplot(res.pca, 
                             fill.ind = factor(clusters_ht$cluster_01), pointshape = 21, pointsize = 3.5, mean.point = FALSE, 
                             label = 'var', legend.title = 'Cluster', repel = TRUE, 
                             select.var = list(contrib = 12), ggtheme = theme_bw(), col.var = 'lightgray', xlab = 'PC1', ylab = 'PC2',
                             title = 'Principal component analysis of clustered sites', alpha = 0.85)+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none')


lulc <- clusters_ht %>%
  group_by(cluster_01) %>%
  dplyr::select(starts_with('LULC')) %>%
  summarise(across(LULC_DEV:LULC_WTLND,median)) %>%
  pivot_longer(cols = LULC_DEV:LULC_WTLND) %>%
  mutate(Landuse = factor(name, levels = rev(c('LULC_AG','LULC_DEV','LULC_FOR','LULC_WTLND')))) %>%
  mutate(cluster = factor(cluster_01)) %>%
  ggplot(aes(x = cluster, y = value*100, fill = Landuse))+
  geom_bar(stat = 'identity')+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        title = element_text(size = 9))+
  scale_fill_manual(values = rev(c('#bb9457','firebrick','#adc178','lightblue')), 
                    labels = rev(c('Agriculture','Developed','Forested','Wetlands')))+
  guides(fill = guide_legend(nrow = 2, override.aes = list(size=3)))+
  ggtitle('Land use')+
  ylab('Percentage of contributing area')

soils <- clusters_ht %>%
  group_by(cluster_01) %>%
  dplyr::select(starts_with('SOIL')) %>%
  mutate(High = SOIL_HGA+SOIL_HGAD, Moderate = SOIL_HGB+SOIL_HGBC+SOIL_HGBD,
         Slow = SOIL_HGC+SOIL_HGCD, `Very slow` = SOIL_HGD) %>%
  dplyr::select(!starts_with('SOIL')) %>%
  summarise(across(High:`Very slow`,median)) %>%
  pivot_longer(cols = High:`Very slow`) %>%
  mutate(Infiltration = factor(name, levels = rev(c('Very slow','Slow','Moderate','High')))) %>%
  mutate(cluster = factor(cluster_01)) %>%
  ggplot(aes(x = cluster, y = value, fill = Infiltration))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = rev(c('#432818','#99582a','#bb9457','#ffe6a7')), 
                    labels = rev(c('Very slow','Slow','Moderate','High')),
                    name = '')+
  guides(fill = guide_legend(nrow = 2, override.aes = list(size=3)))+
  ggtitle('Soil infiltration capacity')+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 9))+
  ylab('Percentage of contributing area')
soils 

hydro_mean_q <- clusters_ht %>%
  group_by(cluster_01) %>%
  summarise(across(PHYS_BASIN_AREA:CHEM_CQ_SLOPE,median)) %>%
  mutate(cluster = factor(cluster_01)) %>%
  ggplot(aes(x = cluster, y = log10(HYDRO_MEAN_Q), fill = cluster))+
  geom_bar(stat = 'identity')+
  ggtitle('Mean discharge')+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 9))+
  ylab('log(Mean discharge) (cfs)')

anthro_maj <- clusters_ht %>%
  group_by(cluster_01) %>%
  summarise(across(PHYS_BASIN_AREA:CHEM_CQ_SLOPE,median)) %>%
  mutate(cluster = factor(cluster_01)) %>%
  ggplot(aes(x = cluster, y = ANTHRO_MAJOR2013, fill = cluster))+
  geom_bar(stat = 'identity')+
  ggtitle('Major dams')+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 9))+
  ylab('Number of major dams')

anthro_npdes <- clusters_ht %>%
  group_by(cluster_01) %>%
  summarise(across(PHYS_BASIN_AREA:CHEM_CQ_SLOPE,median)) %>%
  mutate(cluster = factor(cluster_01)) %>%
  ggplot(aes(x = cluster, y = ANTHRO_NPDES_MAJ, fill = cluster))+
  geom_bar(stat = 'identity')+
  ggtitle('Major NPDES sites')+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 9))+
  ylab('Number of NPDES sites')

hydro_recharge <- clusters_ht %>%
  group_by(cluster_01) %>%
  summarise(across(PHYS_BASIN_AREA:CHEM_CQ_SLOPE,median)) %>%
  mutate(cluster = factor(cluster_01)) %>%
  ggplot(aes(x = cluster, y = HYDRO_RECHG, fill = cluster))+
  geom_bar(stat = 'identity')+
  ggtitle('Amount of recharge')+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none',
        title = element_text(size = 9))+
  ylab('Recharge')

layout <- as.matrix(rbind(c(1,1,1,1,3,3,4,4),c(1,1,1,1,3,3,4,4),
                          c(1,1,1,1,5,5,6,6),c(2,2,2,2,5,5,6,6),
                          c(2,2,2,2,7,7,8,8),c(2,2,2,2,7,7,8,8)))
pdf('04_analysis/figs/clusters_map.pdf', height = 8, width = 11)
gridExtra::grid.arrange(cl_map, cl_biplot, lulc, soils, hydro_mean_q, anthro_maj, anthro_npdes, hydro_recharge, layout_matrix = layout)
dev.off()

#------------------------------------------------------------------------#
######################################################################
#which ones were included in the updated model
used_u <- basin_char_scaled_u %>% colnames()
used_old <- basin_char_scaled %>% colnames()

new_inclusion <- used_u[which(!used_u %in% used_old)]
old_inclusion <- used_old[which(!used_old %in% used_u)]


basin_char_clean %>%
  select(new_inclusion) %>%
  mutate_all(~(scale(.) %>% as.vector)) %>%
  pivot_longer(cols = new_inclusion) %>%
  ggplot(aes(x = name, y = value))+
  geom_boxplot()+
  coord_flip()+
  ggtitle('New Inclusions')

basin_char_clean %>%
  dplyr::select(all_of(used_old)) %>%
  mutate_all(~(scale(.) %>% as.vector)) %>%
  pivot_longer(cols = used_old) %>%
  ggplot(aes(x = name, y = value))+
  geom_boxplot()+
  coord_flip()+
  ggtitle('Previously used')
######################################################################

#read in feature importance
char_feat_imp <- read_csv('04_analysis/out/multi_site_ensemble_feature_importance.csv') %>%
  filter(!feat %in% c('Discharge','Precip','TempMax','TempMin','SolarRad')) %>%
  arrange(desc(feat_imp_mean)) %>%
  filter(feat_imp_mean > 0.25) %>%
  pull(feat)

#find the cleaned up names of the features with higher feature importances scores
names_lookup <- read_csv('04_analysis/out/basin_char_names_lookup.csv') %>%
  filter(Names %in% char_feat_imp | !is.na(Calculated)) %>%
  pull(Names_Clean) 

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

plot(basin_char_cl_u)


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


