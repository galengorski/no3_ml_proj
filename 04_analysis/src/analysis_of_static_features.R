#===================================================================================#
# NOTES:                                                                            #
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2022-04-27                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
#install.packages('vegan')
library(vegan)
#install.packages('ape')
library(ape)
#install.packages('dplyr')
library(dplyr)
library(ggplot2)
library(nhdplusTools)
library(tmap)
library(tmaptools)
library(tigris)
library(grid)
library(ggridges)
library(sf)
library(Hmisc)
library(leaflet)
library(randomForest)
library(factoextra)
library(corrr)

#####
#===================================================================================#
#read in model run results
#multi_site model
run40m <- read_csv('03_model/out/multi_site/Run_40/Rep_00/AllSitesModelResults.csv')
run40m['cluster'] <- NA
#single site model
runHP06s <- read_csv('03_model/out/single_site/hyper_param_tune/HP_06/Rep_00/AllSitesModelResults.csv')
runHP06s['cluster'] <- NA
runHP06s <- runHP06s[-46,]
runHP06s$Site_name = run40m$Site_name
#clustered model
clusters <- list.files('03_model/out/multi_site/Run_42/')
run42 <- data.frame()
for(clust in clusters){
  temp_clust42 <- read_csv(paste0('03_model/out/multi_site/Run_42/',clust,'/AllSitesModelResults.csv'))
  temp_clust42['cluster'] <- clust
  run42 <- rbind(run42, temp_clust42)
}
#terranes model
terranes <- list.files('03_model/out/multi_site/Run_41/')
run41 <- data.frame()
for(terr in terranes){
  temp_terr41 <- read_csv(paste0('03_model/out/multi_site/Run_41/',terr,'/AllSitesModelResults.csv'))
  temp_terr41['cluster'] <- terr
  run41 <- rbind(run41, temp_terr41)
}

runHP06s['run_short'] <- 'Single-site'
run40m['run_short'] <- 'Multi-site'
run41['run_short'] <- 'Hydro terrane'
run42['run_short'] <- 'Clustered'

runs <- rbind(run40m, runHP06s, run41, run42)


#how do the run compare to eachother?
par(mfrow = c(2,3))

plot(run40s$RMSE_validation, run40m$RMSE_validation, pch = 21, cex = 1.2, bg = 'darkgray', las = 1, ylim = c(0,6), xlim = c(0,6),
     xlab = 'Single-site RMSE (mg/L)', ylab = 'Multi-site RMSE (mg/L)')
abline(0,1, lty = 2, col = 'red', lwd = 0.8)

plot(run40s$RMSE_validation, run41$RMSE_validation, pch = 21, cex = 1.2, bg = 'darkgray', las = 1, ylim = c(0,6), xlim = c(0,6),
     xlab = 'Single-site RMSE (mg/L)', ylab = 'Hydro terrane RMSE (mg/L)')
abline(0,1, lty = 2, col = 'red', lwd = 0.8)

plot(run40s$RMSE_validation, run42$RMSE_validation, pch = 21, cex = 1.2, bg = 'darkgray', las = 1, ylim = c(0,6), xlim = c(0,6),
     xlab = 'Single-site RMSE (mg/L)', ylab = 'Clustered RMSE (mg/L)')
abline(0,1, lty = 2, col = 'red', lwd = 0.8)

plot(run40m$RMSE_validation, run42$RMSE_validation, pch = 21, cex = 1.2, bg = 'darkgray', las = 1, ylim = c(0,6), xlim = c(0,6),
     xlab = 'Multi-site RMSE (mg/L)', ylab = 'Clustered RMSE (mg/L)')
abline(0,1, lty = 2, col = 'red', lwd = 0.8)

plot(run40m$RMSE_validation, run41$RMSE_validation, pch = 21, cex = 1.2, bg = 'darkgray', las = 1, ylim = c(0,6), xlim = c(0,6),
     xlab = 'Multi-site RMSE (mg/L)', ylab = 'Hydro terrane RMSE (mg/L)')
abline(0,1, lty = 2, col = 'red', lwd = 0.8)

plot(run42$RMSE_validation, run41$RMSE_validation, pch = 21, cex = 1.2, bg = 'darkgray', las = 1, ylim = c(0,6), xlim = c(0,6),
     xlab = 'Clustered RMSE (mg/L)', ylab = 'Hydro terrane RMSE (mg/L)')
abline(0,1, lty = 2, col = 'red', lwd = 0.8)


#https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html
runs %>%
  #filter(run_short > 2) %>%
  pivot_longer(cols = c(RMSE_training, RMSE_validation)) %>%
  mutate(runfct = as.factor(run_short)) %>%
  mutate(RMSE = as.numeric(value)) %>%
  ggplot(aes(y = runfct)) +
  geom_density_ridges(
    aes(x = RMSE, fill = paste(runfct, name)),
    alpha = 0.6, color = 'black'
  )+
  labs(
    x = "RMSE (mg/L)",
    y = "Model Type",
    title = "Model performance",
    subtitle = "46 sites",
    caption = "seq_len = 365; lr = 0.005; owd = 0.001; bs = 512, "
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    breaks = c("Multi-site RMSE_training", "Multi-site RMSE_validation"),
    labels = c(`Multi-site RMSE_training` = "Training", `Multi-site RMSE_validation` = "Validation"),
    values = c("#ae2012", "#0a9396"),
    name = " ", guide = "legend"
  )+
  coord_cartesian(clip = "off")+
  theme_ridges(grid = FALSE)


summary_stats <- runs %>% 
  group_by(run_short) %>% 
  summarise(NSE_training = median(NSE_training),
            NSE_validation = median(NSE_validation),
            RMSE_training = median(RMSE_training),
            RMSE_validation = median(RMSE_validation),
            NRMSE_training = median(NRMSE_training),
            NRMSE_validation = median(NRMSE_validation),
            PBIAS_training = median(PBIAS_training),
            PBIAS_validation = median(PBIAS_validation))

summary_stats %>%
  pivot_longer(cols = NSE_training:PBIAS_validation, names_sep = '_', names_to = c('stat','set')) %>%
  ggplot(aes(x = run_short, y = value, fill = set)) +
  geom_bar(stat = 'identity',position = 'dodge') +
  facet_wrap(.~stat, scales = 'free_y')


#performance map
site_info <- read_csv('04_analysis/out/basin_char_w_clusters_6_hydro_terranes.csv')

#download states outlines

states <- states()
conus <- states %>%
  filter(NAME %nin% c('Puerto Rico','Hawaii','Alaska','Guam','American Somoa','Commonwealth of the Northern Mariana Islands','United States Virigin Islands'))

site_runs <- merge(site_info, runs, by.x = 'site_no', by.y = 'Site_number', all.x = FALSE, all.y = TRUE)
site_runs_sf <- st_as_sf(site_info, coords = c('long','lat'), crs = 4326)

bbox <- st_bbox(site_runs_sf) %>%
  st_as_sfc()

m_site <- site_runs_sf %>%
  filter(run_short == 'Multi-site')
pal <- RColorBrewer::brewer.pal(7, "RdBu")
tm_shape(conus, bbox = bbox*c(1,1.1)) +
  tm_borders() +
  tm_shape(TOT_ARTIFICIAL)+
  tm_symbols(col = 'NRMSE_validation'), 
             #breaks = c(0,.2,.4,.6,.8,1,8), 
             #palette = pal, 
             #midpoint = 8)+
  tm_facets(by = 'run_short')


#look at the cdf by model run

runs %>%
  group_by(run_short) %>%
  arrange(NSE_validation)%>%
  mutate(plotting = seq(1,46)) %>%
  ggplot(aes(y = plotting, x = NSE_validation, color = run_short))+
  geom_line() +
  xlim(-1,1) +
  theme_bw()


###https://ourcodingclub.github.io/tutorials/ordination/
####https://rpkgs.datanovia.com/factoextra/reference/fviz_pca.html
basin_char <- site_info[ , colSums(is.na(site_info)) == 0][,3:55]
slim <- basin_char[,!colSums(basin_char)==0]
slim$cluster <- as.factor(site_info$cluster)
#slim$cluster <- best_models$model
pca_slim <- prcomp(slim[,-53], scale = TRUE, center = TRUE)

fviz_pca_ind(pca_slim, label="none", col.ind=slim$cluster, palette = "Dark2")

fviz_pca_var(pca_slim, col.var = "contrib",
             gradient.cols = c("blue", "red"),
             ggtheme = theme_minimal(),
             select.var = list(contrib = 10))
fviz_pca_biplot(pca_slim, label = "var", habillage=slim$cluster,
                select.var = list(contrib = 10),
                ggtheme = theme_minimal(), repel = TRUE)

pca_var <- pca_attr$sdev^2
pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)
barplot(pca_var_perc, main = "Variation Plot", xlab = "PCs", ylab = "Percentage Variance", ylim = c(0, 100))

plot(pca_attr$x[,1], pca_attr$x[,2], #bg = best_model$color, 
     xlab = paste('PC 1',pca_var_perc[1],'% Explained'), ylab = paste('PC 2',pca_var_perc[2],'% Explained'),
     pch = 21, col = 'black', cex = 1.4)
#text(pca_attr$x[,1], pca_attr$x[,2], labels = sites$STANAME, col = best_model$color)

rownames(pca_attr$x) <- site_info$station_nm

fviz_pca_ind(pca_attr,
             col.ind = 'cos2',
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_var(pca_attr,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca_attr,
             label = 'none',
             addEllipses = TRUE,
             #habillage = site_info$cluster[1:4],
             palette = 'Dark2',
             repel = TRUE,
             select.ind = list(cos2 = 4)# Avoid text overlapping
)

fviz_pca_biplot(pca_attr[,1:4], repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

facto_summarize(pca_attr, "var", axes = 1:2,
                select = list(contrib = 5))[,-1]

#######################
basin_char_runs <- merge(site_info, runs, by.x = 'site_no', by.y = 'Site_number', all.x = T, all.y = T)

best_models_nse <- basin_char_runs %>%
  group_by(site_no) %>%
  arrange(NSE_validation, .by_group = TRUE) %>%
  dplyr::summarise(NSE = max(NSE_validation), model = last(run_short), cluster = first(cluster.x), hydro = first(hydro_terrane),
                   station_nm = first(Site_name), lat = first(lat), long = first(long))

best_models_nse %>% 
  filter(NSE > 0.67)

best_models_nrmse <- basin_char_runs %>%
  group_by(site_no) %>%
  arrange(NRMSE_validation, .by_group = TRUE) %>%
  dplyr::summarise(NRMSE = min(NRMSE_validation), model = first(run_short), cluster = first(cluster.x), hydro = first(hydro_terrane),
                   station_nm = first(Site_name), lat = first(lat), long = first(long))

top_nrmse <- best_models_nrmse %>% 
  filter(NRMSE < 0.16)


########################################################################
#comparison of rmse single site vs improvement from other sites
########################################################################
best_models_nrmse <- basin_char_runs %>%
  filter(run_short != 'Single-site') %>%
  group_by(site_no) %>%
  arrange(NRMSE_validation, .by_group = TRUE) %>%
  dplyr::summarise(NRMSE = min(NRMSE_validation), model = first(run_short), cluster = first(cluster.x), hydro = first(hydro_terrane),
                   station_nm = first(Site_name), lat = first(lat), long = first(long))

best_models_nrmse %>%
  mutate(single_site = runs[runs$run_short == 'Single-site',]$NRMSE_validation) %>%
  ggplot(aes(x = single_site, y = single_site-NRMSE, fill = model))+
  geom_point(shape = 21, color = 'black', size = 3)+
  theme_bw()+
  ylim(-0.25,1.2)+
  xlim(0,1.5)+
  ggtitle('Improvement from single-site to multi-site\n <0 indicates worse performance in multi-site model')+
  xlab('Single site NRMSE (mg/L)')+
  ylab('Difference in NRMSE (mg/L)')
########################################################################

basin_char_runs %>% 
  filter(hydro_terrane == '2A')


site_info %>%
  dplyr::select(TOT_HGA,NLCD_WTLND,TOT_RECHG, cluster) %>%
  pivot_longer(cols = TOT_HGA:TOT_RECHG) %>%
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

site_info %>%
  dplyr::select(TOT_HGA,NLCD_WTLND,TOT_RECHG, hydro_terrane) %>%
  pivot_longer(cols = TOT_HGA:TOT_RECHG) %>%
  mutate(hydro_terrane = as.factor(hydro_terrane)) %>%
  mutate(value = as.numeric(value)) %>%
  ggplot(aes(y = hydro_terrane)) +
  geom_density_ridges(
    aes(x = value, fill = hydro_terrane),
    alpha = 0.6, color = 'black'
  )+
  #scale_fill_manual(values = c('#5E4FA2','#66C2A5','#E6F598','#FDAE61','#D53E4F','#9E0142', 'white','black'))+
  labs(
    x = "Attribute value",
    y = "Hydro terrane",
    title = "Hydro terrane attributes",
    subtitle = "47 sites",
    caption = ""
  ) +
  coord_cartesian(clip = "off")+
  theme_ridges(grid = FALSE)+
  facet_wrap(~name, scales = 'free_x')


 
t <- site_info %>%
  group_by(hydro_terrane) %>%
  summarise(across(TOT_BASIN_AREA:cq_slope, median))

bmr <- best_models_nrmse %>%
  group_by(hydro) %>%
  summarise(med_NRMSE = median(NRMSE), max_NRMSE = max(NRMSE), min_NRMSE = min(NRMSE))

tbmr <- merge(t, bmr, by.x = 'hydro_terrane', by.y = 'hydro')
tbmr$color <- c('#5E4FA2','#66C2A5','#E6F598','#FDAE61','#D53E4F','#9E0142', 'white','black')

#best multi-site models vs. predictors 
par(mfrow = c(2,2)) 
for(i in 2:54){
plot(tbmr[,i], tbmr$med_NRMSE, bg = tbmr$color, pch = 21, ylab = 'Median NRMSE', xlab = colnames(tbmr)[i], cex = 1.6, las = 1, alpha = 0.8)
}


#############################################################################################
#############################################################################################
#THIS PLOT LOOKS AT THE CORRELATION BETWEEN SINGLE SITE MODEL PERFORMANCE AND PREDICTORS####
#compare single site models with predictors
best_ss_models_nrmse <- basin_char_runs %>%
  filter(run_short == 'Single-site')

best_ss_models_nrmse$cluster_factor <- factor(best_ss_models_nrmse$cluster.x)
levels(best_ss_models_nrmse$cluster_factor)  <- c('#5E4FA2','#66C2A5','#E6F598','#FDAE61','#D53E4F','#9E0142', 'orange','black')

best_ss_slim <- best_ss_models_nrmse %>%
  select_if(is.numeric) %>%
  dplyr::select(TOT_BASIN_AREA:cq_slope,NRMSE_validation, -TOT_HGAC)

corr_p <- Hmisc::rcorr(as.matrix(best_ss_slim),type = 'pearson')

r_p_df <- tibble('attribute' = rownames(corr_p$r), 
  r = corr_p$r %>%
  as_tibble() %>%
  dplyr::pull(NRMSE_validation),
  p = corr_p$P %>%
    as_tibble() %>%
    dplyr::pull(NRMSE_validation)) %>% 
  arrange(p)

par(mfrow = c(2,2)) 
for(i in 1:10){
  attr <- r_p_df[i,]
  plot(best_ss_models_nrmse[,attr$attribute], best_ss_models_nrmse$NRMSE_validation, bg = best_ss_models_nrmse$cluster_factor, pch = 21, 
       ylab = 'Single site NRMSE', xlab = attr$attribute, cex = 1.6, las = 1, alpha = 0.8, ylim = c(0,2),
       main = paste0(attr$attribute,' |r = ',round(attr$r,2), ' |p = ',round(attr$p,3)))
}

#####now look at how attributes are associated with increase model performance using multi-site models####
#not as much interesting with these figures
diff_perf <- merge(best_ss_models_nrmse,best_models_nrmse, by = 'site_no')
diff_perf$ss_ms <- diff_perf$NRMSE_validation - diff_perf$NRMSE

diff_slim <- diff_perf %>%
  select_if(is.numeric) %>%
  dplyr::select(TOT_BASIN_AREA:cq_slope,ss_ms, -TOT_HGAC)

diff_corr_p <- Hmisc::rcorr(as.matrix(diff_slim),type = 'pearson')

diff_r_p_df <- tibble('attribute' = rownames(diff_corr_p$r), 
                 r = diff_corr_p$r %>%
                   as_tibble() %>%
                   dplyr::pull(ss_ms),
                 p = diff_corr_p$P %>%
                   as_tibble() %>%
                   dplyr::pull(ss_ms)) %>% 
  arrange(p)

par(mfrow = c(2,2)) 
for(i in 1:53){
  attr <- diff_r_p_df[i,]
  plot(diff_perf[,attr$attribute], diff_perf$ss_ms, bg = diff_perf$cluster_factor, pch = 21, 
       ylab = 'Change in NRMSE', xlab = attr$attribute, cex = 1.6, las = 1, alpha = 0.8, ylim = c(-.5,.5),
       main = paste0(attr$attribute,' |r = ',round(attr$r,2), ' |p = ',round(attr$p,3),'\n < 0 single site is better'))
}

##################################################
##################################################
#MODELS ARE DOING BETTER WITH MORE RECHARGE, WETLANDS, AND TOT_HGA 
basin_char_runs %>%
  ggplot(aes(x = TOT_RECHG, y = NRMSE_validation, fill = cluster.y)) +
  geom_point(size = 3, shape = 21)+
  ylim(0,1)+
  facet_wrap(~run_short, scales = 'free_y')
##################################################
##################################################


runs %>%
  filter(run_short == 'Clustered') %>%
  group_by(cluster) %>%
  summarise(rmse = mean(RMSE_validation), nrmse = mean(NRMSE_validation), nse = mean(NSE_validation))

pal <- colorFactor(c('#f94144','#f8961e','#772e25','#2b9348'), 
                   levels = c("Single-site", "Multi-site","Clustered","Hydro terrane"), 
                   ordered = FALSE)
m <- leaflet(best_models) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(-84.699, 37.5, zoom=5) %>%
  addCircleMarkers(lng = ~long, lat = ~lat,
                   fillColor = ~pal(model), stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 0.70,
                   radius = 6, color = 'black', label = paste(best_models$station_nm, round(best_models$NSE, 3), sep = ' | ')) %>%
  addLegend('bottomright', pal = pal, values = ~best_models$model, title = 'Best model')
m



best_models['PCA_1'] <- pca_attr$x[,1]
best_models['PCA_2'] <- pca_attr$x[,2]

#sf_perf['Dryness_Index'] <- sf_perf['CAT_PET']/sf_perf['CAT_PPT7100_ANN']
#sf_perf['Evaporative_Index'] <- sf_perf['CAT_ET']/sf_perf['CAT_PPT7100_ANN']


best_models %>%
  ggplot(aes(x = PCA_1, y = PCA_2))+
  geom_point(aes(fill = model), size = 6, alpha = 0.75, shape = 21)+
  #scale_fill_gradientn(colors = hcl.colors(10, palette = 'plasma'))+
  xlab(paste('PC 1',pca_var_perc[1],'% Explained'))+
  ylab(paste('PC 2',pca_var_perc[2],'% Explained'))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"))

######################################################################
######################################################################
#Heat map between attributes and model performance
library(reshape2)
library(forcats)
basin_char_runs_clean <- basin_char_runs
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
  rename('HYDRO_MEAN_Q' = 'mean_q', 'HYDRO_SD_Q' = 'sd_q')
  
  


basin_char_runs_clean %>%
  filter(run_short == 'Single-site') %>%
  mutate(APBIAS_validation = abs(PBIAS_validation)) %>%
  dplyr::select(-PBIAS_validation, -run_short) %>%
  scale() %>%
  cor() %>%
  melt() %>%
  as_tibble() %>%
  filter(Var1 %in% c("RMSE_training","RMSE_validation","NRMSE_training","NRMSE_validation",
                     "NSE_training","NSE_validation","APBIAS_training" ,"APBIAS_validation")) %>%
  filter(Var2 %nin% c("RMSE_training","RMSE_validation","NRMSE_training","NRMSE_validation",
                      "NSE_training","NSE_validation","APBIAS_training" ,"APBIAS_validation")) %>%
  #mutate(Var2 = fct_reorder(Var2, Var2)) %>%
  mutate(Var2 = as.character(Var2)) %>%
  mutate(Var2 = factor(Var2)) %>%
  filter(abs(value) >0.2) %>%
  ggplot(aes(x = Var1, y = Var2, fill = value))+
  geom_tile(color = 'white')+
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000", limits = c(-1,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  #coord_equal()+
  ggtitle('Single site performance and\nbasin characteristics')+
  guides(fill=guide_legend(title="Pearson r"))
  

######################################################################
#Random forest on attributes
rf_NSE_Valid <- list()
rf_imp_df <- data.frame(matrix(nrow = 53, ncol = 1))
#model_s <- "Single-site"

for(model_s in c("Single-site", "Multi-site","Clustered","Hydro terrane")){
model_run <- runs %>%
  filter(run_short == model_s) %>%
  dplyr::select(Site_number, NSE_validation) %>%
  full_join(site_info, by = c("Site_number" = "site_no")) %>%
  dplyr::select(!c(...1,Site_number, cluster, aggo_cluster, station_nm, hydro_terrane))

rf_NSE_Valid[[model_s]] <- randomForest(NSE_validation~., data = model_run, importance = T, ntree = 500, na.action=na.omit)
imp <- importance(rf_NSE_Valid[[model_s]], type = 1, scale = FALSE)
imp_norm <- imp/max(imp)

colnames(imp_norm) <- model_s

rf_imp_df <- cbind(rf_imp_df, imp_norm)
}

rf_imp_df$attr <- rownames(rf_imp_df)
rf_imp_df %>%
  as_tibble() %>%
  dplyr::select(-1) %>%
  arrange(desc(`Clustered`)) %>%
  top_n(`Clustered`,n = 20) %>%
  ggplot(aes(x = reorder(attr, `Clustered`), y = `Clustered`)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  coord_flip()

plot(basin_char_runs[basin_char_runs$run_short == 'Multi-site',]$TOT_HGC, basin_char_runs[basin_char_runs$run_short == 'Multi-site',]$NRMSE_validation, ylim = c(0,1))

best_models_char <- merge(best_models, basin_char, by = 'site_no')
best_models_char['Dryness_Index'] <- best_models_char['CAT_PET']/best_models_char['CAT_PPT7100_ANN']
best_models_char['Evaporative_Index'] <- best_models_char['CAT_ET']/best_models_char['CAT_PPT7100_ANN']

best_models_char %>%
  filter(min_RMSE_valid < 4) %>%
  ggplot(aes(x = Dryness_Index, y = Evaporative_Index))+
  geom_point(aes(fill = min_RMSE_valid), size = 6, alpha = 0.75, shape = 21)+
  scale_fill_gradientn(colors = hcl.colors(10, palette = 'plasma'))+
  xlab(paste('Dryness Index (PET/P)'))+
  ylab(paste('Evaporative Index (ET/P)'))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


slim_perf <- merge(slim, run05[,c('Site_name', 'Site_number', 'NRMSE_training', 'NRMSE_validation','RMSE_training','RMSE_validation')], by.x = 'site_no', by.y = 'Site_number', all = TRUE, suffixes = 'single_site')
slim_perf <- merge(slim_perf, run06[,c('Site_name', 'Site_number', 'NRMSE_training', 'NRMSE_validation','RMSE_training','RMSE_validation')], by.x = 'site_no', by.y = 'Site_number', all = TRUE, suffixes = c('_single_site','_multi_site'))

slim_perf['Dryness_Index'] <- sf_perf['Dryness_Index']
slim_perf['Evaporative_Index'] <- sf_perf['Evaporative_Index']

par(mfrow = c(2,2))
for( i in 2:17){
  if(i == 105){
    next
  }
  
  pear <- paste(round(cor(slim_perf$NRMSE_training_single_site,slim_perf[,i], method = 'pearson'), digits = 2),
  round(cor.test(slim_perf$NRMSE_training_single_site,slim_perf[,i], method = 'pearson')$p.value, digits = 2), sep = '|')
  plot(slim_perf[,i], slim_perf$NRMSE_training_single_site, xlab = colnames(slim_perf)[i], ylab = 'NRMSE_valid', main = paste('training single site', pear, sep = ':'), col = 'dodgerblue', ylim = c(0,1))
  
  pear <- paste(round(cor(slim_perf$NRMSE_validation_single_site,slim_perf[,i], method = 'pearson'), digits = 2),
  round(cor.test(slim_perf$NRMSE_validation_single_site,slim_perf[,i], method = 'pearson')$p.value, digits = 2), sep = '|')
  plot(slim_perf[,i], slim_perf$NRMSE_validation_single_site, col = 'blue', xlab = colnames(slim_perf)[i], ylab = 'NRMSE_valid', main = paste('validation single site', pear, sep = ':'), ylim = c(0,1))
  
  pear <- paste(round(cor(slim_perf$NRMSE_training_multi_site,slim_perf[,i], method = 'pearson'), digits = 2),
              round(cor.test(slim_perf$NRMSE_training_multi_site,slim_perf[,i], method = 'pearson')$p.value, digits = 2), sep = '|')
  plot(slim_perf[,i], slim_perf$NRMSE_training_multi_site, xlab = colnames(slim_perf)[i], ylab = 'NRMSE_valid', main = paste('training multi site', pear, sep = ':'), pch = 16, col = 'red', ylim = c(0,1))
  
  pear <- paste(round(cor(slim_perf$NRMSE_validation_multi_site,slim_perf[,i], method = 'pearson'), digits = 2),
                round(cor.test(slim_perf$NRMSE_validation_multi_site,slim_perf[,i], method = 'pearson')$p.value, digits = 2), sep = '|')
  
  plot(slim_perf[,i], slim_perf$NRMSE_validation_multi_site, col = 'firebrick', xlab = colnames(slim_perf)[i], ylab = 'NRMSE_valid', main = paste('validation multi site', pear, sep = ':'), pch = 16, ylim = c(0,1))
  
}




basin_char <- c('CAT_N97', 'CAT_CONTACT', 'CAT_CANALDITCH', 'CAT_ARTIFICIAL', 'CAT_BASIN_AREA', 'CAT_SILTAVE', 'CAT_CLAYAVE', 'fert_uN_mt', 'NLCD_11', 'NLCD_21', 'NLCD_22', 'NLCD_23', 'NLCD_24', 'NLCD_31', 'NLCD_41', 'NLCD_42', 'NLCD_43', 'NLCD_52', 'NLCD_71', 'NLCD_81', 'NLCD_82', 'NLCD_90', 'NLCD_95')
basin_char <- colnames(sf_df)[3:144]
char_meta <- discover_nldi_characteristics()

for (i in 1:length(basin_char)){
  if (basin_char[i] %in% colnames(sf_perf)){
  units <- char_meta$local[char_meta$local$characteristic_id == basin_char[i],]$units
  descrip <- char_meta$local[char_meta$local$characteristic_id == basin_char[i],]$characteristic_description
  t <- sf_perf[,c('Site_name_attr_no_bfs',basin_char[i])]
  par(mar = c(4,16,4,4))
  barplot(height = t[,basin_char[i]], names = t$Site_name_attr_no_bfs, horiz = T, xlab = paste(basin_char[i], units, sep = ','), las = 1, cex.names = 0.6, main = descrip)
  }else{
  next
}
}

slim_perf %>%
  ggplot(aes(x = Dryness_Index, y = Evaporative_Index))+
  geom_point(aes(fill = RMSE_training_single_site), size = 6, alpha = 0.75, shape = 21)+
  scale_fill_gradientn(colors = hcl.colors(4, palette = 'plasma'))+
  xlab(paste('Dryness Index (PET/P)'))+
  ylab(paste('Evaporative Index (ET/P)'))+
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ylim(0.4,1)+
  xlim(0.4,1)



prev <- read_csv('../Midwest_USGS/github/WQ_LSTM/07_comparing_nitrate_models/output_data/01646500.csv')

plot(prev$DateTime, prev$Observed, col = 'black', typ = 'l')
lines(prev$DateTime, prev$all_ws_bfs_attr, col = 'red')



hydro_data <- read_csv('01_fetch/out/hydro_filled_220128.csv')

test <- hydro_data %>%
  filter(site_no == '05447500') %>%
  filter(!is.na(nitrate)) %>%
  dim()
  tail()

  
  
  
  contact <- ggplot(data = states) +
    geom_sf()+
    geom_sf(data = site_runs_sf, aes(fill = HYDRO_CONTACT), shape = 21, size = 5, alpha = 0.8) +
    scale_fill_gradient(low = '#ffffe5', high = '#cc4c02')+
    coord_sf(xlim = c(-98, -68), ylim = c(35, 49), expand = FALSE) +
    theme_bw()+
    guides(fill=guide_colourbar(title="Subsurface contact time (days)"))+
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
  contact
  ggsave('04_analysis/figs/Contact.jpeg',height = 7, width = 9, dpi = 500)
  
  