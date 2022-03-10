#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb 18 12:21:40 2022

@author: galengorski
"""

import lstm_modeling_functions as lmf
import pandas as pd




netcdf_loc = '02_munge/out/model_input.nc'
config_loc = '03_model/single_site_model_config.yaml'
#site_no = '01463500'
out_dir = '03_model/out/Run_03'
site_info = pd.read_csv('01_fetch/out/site_list_220128.csv', dtype = {'site_no':str})
#i = 4
#site_no = site_info['site_no'][i]
#station_nm = site_info['station_nm'][i]
#%%
for i, indv_site in enumerate(site_info['site_no'].unique()):
    site_no = site_info['site_no'][i]
    station_nm = site_info['station_nm'][i]
    try:
        print("Training "+' | '+station_nm+' | '+site_no+' | '+str(i))
        lmf.run_model(netcdf_loc, config_loc, site_no, station_nm, out_dir)
    except:
        print("ERROR! "+site_no+' | '+str(i))