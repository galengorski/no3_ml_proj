#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct  7 11:15:42 2022

@author: galengorski
"""

import sys
sys.path.append('03_model/src/new')
import lstm_modeling_functions as lmf
import numpy as np
import os
import pandas as pd
#%% ATTRIBUTES NO BASEFLOW SEP
netcdf_loc = '02_munge/out/model_input_rolling.nc'
config_loc = '03_model/multi_site_model_config.yaml'


#site_info = site_info.iloc
#input file location
read_input_data_from_file = False
input_file_loc = None
model_run_id = 'Run_07_OOS'
model_run_dir = os.path.join('03_model/out/multi_site',model_run_id)
train_model = True
save_results_csv = True
hp_tune = False
hp_tune_vals = {}
multi_site = True

#%%
for j in range(5):
    
    site_info = pd.read_csv('04_analysis/out/basin_char_w_clusters_hydroterranes_221005.csv',  dtype = {'site_no':str, 'cluster_01':str, 'cluster_02':str})
    #generate out of sample sites
    oos_sites = site_info.groupby('cluster_01', group_keys = False).apply(lambda x: x.sample(1))[['site_no','cluster_01','hydro_terrane']]
    
    site_info.drop(oos_sites.index, inplace = True)

    rep = 'Rep_0'+str(j)
        
    print("excluding sites",oos_sites)

    
    rep = 'Rep_0'+str(j)
    #clustered models
    for group in np.sort(site_info.cluster_01.unique()):
        print(group,'of ',len(site_info.cluster_01.unique()),' groups')
    
        site_no_list = site_info[site_info['cluster_01'] == group].site_no
        station_nm_list = site_info[site_info['cluster_01'] == group].station_nm
        
        
        run_id = os.path.join(model_run_id, rep,  'Cluster_0'+str(group))
        out_dir = os.path.join(model_run_dir, rep,  'Cluster_0'+str(group))
        
        read_input_data_from_file = False
        input_file_loc = os.path.join(model_run_dir,'Cluster_0'+str(group))
        weights_dir = out_dir
        
        lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                               read_input_data_from_file, input_file_loc, out_dir, run_id,
                               train_model, multi_site, weights_dir)
    
    
    #multi_site model
    site_no_list = site_info.site_no
    station_nm_list = site_info.station_nm

    run_id = os.path.join(model_run_id,  ('Rep_0'+str(j)))
    out_dir = os.path.join(model_run_dir,  ('Rep_0'+str(j)))
    
    read_input_data_from_file = False
    input_file_loc = os.path.join(model_run_dir,('Rep_0'+str(j)))
    weights_dir = out_dir
    
    lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                           read_input_data_from_file, input_file_loc, out_dir, run_id,
                           train_model, multi_site, weights_dir)
    
    #run hydro_terrane model
    rep = 'Rep_0'+str(j)
    
    for group in np.sort(site_info.hydro_terrane.unique()):
        print(group,'of 8 terranes')
    
        site_no_list = site_info[site_info['hydro_terrane'] == group].site_no
        station_nm_list = site_info[site_info['hydro_terrane'] == group].station_nm
        
        
        run_id = os.path.join(model_run_id, rep,  'Terrane_'+str(group))
        out_dir = os.path.join(model_run_dir, rep,  'Terrane_'+str(group))
        
        read_input_data_from_file = False
        input_file_loc = os.path.join(model_run_dir,'Terrane_'+str(group))
        weights_dir = out_dir
        
        lmf.run_multi_site_model_c(netcdf_loc, config_loc, site_no_list, station_nm_list, 
                               read_input_data_from_file, input_file_loc, out_dir, run_id,
                               train_model, multi_site, weights_dir)
    
    pd.DataFrame(oos_sites).to_csv(os.path.join('03_model/out/multi_site', model_run_id, rep,'oos_sites.csv'))


