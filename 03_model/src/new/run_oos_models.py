#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 19 17:59:45 2022

@author: galengorski
"""

import lstm_modeling_functions as lmf
import pandas as pd
import os
import numpy as np
#%% ATTRIBUTES NO BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input_rolling.nc'
config_loc = '03_model/multi_site_model_config.yaml'

site_info = pd.read_csv('04_analysis/out/basin_char_w_clusters_hydroterranes_221005.csv',  dtype = {'site_no':str})




#input file location
read_input_data_from_file = False
input_file_loc = None
model_run_id = 'Run_08_OOS_PRED'
model_run_dir = os.path.join('03_model/out/multi_site',model_run_id)
train_model = False
save_results_csv = False
hp_tune = False
hp_tune_vals = {}
multi_site = True


#for each out of samples replicate
for j in range(5):
    rep = 'Rep_0'+str(j)
    #read in the out of sample sites
    oos_sites = pd.read_csv(os.path.join('03_model/out/multi_site/Run_07_OOS',rep,'oos_sites.csv'), dtype = {'site_no':str})
    site_no_list = oos_sites.site_no
    station_nm_list = site_info[site_info.site_no.isin(site_no_list)].station_nm
    #predict using the multi-site model
    run_id = os.path.join(model_run_id,rep ,'multi_site')
    out_dir = os.path.join(model_run_dir,rep,'multi_site')
    weights_dir = os.path.join('03_model/out/multi_site/Run_07_OOS',rep)
    #don't read input data from file
    read_input_data_from_file = False
    input_file_loc = os.path.join(model_run_dir,('Rep_0'+str(j)),'multi_site')
    #run the multi_site_model in prediction mode
    lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                           read_input_data_from_file, input_file_loc, out_dir, run_id,
                           train_model, multi_site, weights_dir)
    #now use the prepped data and make predictions using each cluster's model
    for group in np.sort(site_info.cluster_01.unique()):
        run_id = os.path.join(model_run_id,  rep,'clustered', 'Cluster_0'+str(group))
        out_dir = os.path.join(model_run_dir,  rep ,'clustered', 'Cluster_0'+str(group))
        weights_dir = os.path.join('03_model/out/multi_site/Run_07_OOS',rep,'Cluster_0'+str(group))
        
        read_input_data_from_file = True
        input_file_loc = os.path.join(model_run_dir,rep,'multi_site')
        
        lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                       read_input_data_from_file, input_file_loc, out_dir, run_id,
                       train_model, multi_site, weights_dir)
    #use the prepped data and make predictions using each hydroterrane's model
    for group in np.sort(site_info.hydro_terrane.unique()):
        run_id = os.path.join(model_run_id,  rep,'hydro_terrane', 'Terrane_'+str(group))
        out_dir = os.path.join(model_run_dir,  rep ,'hydro_terrane', 'Terrane_'+str(group))
        weights_dir = os.path.join('03_model/out/multi_site/Run_07_OOS',rep,'Terrane_'+str(group))
        
        read_input_data_from_file = True
        input_file_loc = os.path.join(model_run_dir,rep,'multi_site')
        
        lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                       read_input_data_from_file, input_file_loc, out_dir, run_id,
                       train_model, multi_site, weights_dir)