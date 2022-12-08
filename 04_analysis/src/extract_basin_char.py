#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 28 08:52:42 2022

@author: galengorski
"""
import numpy as np
import pandas as pd
import preproc_functions as ppf
#%%
site_info = pd.read_csv('01_fetch/out/site_list_220507.csv', dtype = {'site_no':str})
#This is the full set of features used in multi-site/Run_01 which were narrowed down using
#permutation feature importance. A subset of 30 of these features is used for final modeling, that 
#subset can be found in multi_site/Run_02
feat_list = ['TOT_BASIN_AREA', 'TOT_BASIN_SLOPE', 'lat', 'long', 'TOT_STREAM_SLOPE', 
             'TOT_STREAM_LENGTH', 'TOT_STRM_DENS', 'TOT_CANALDITCH', 'TOT_ARTIFICIAL', 
             'TOT_HGA', 'TOT_HGAD', 'TOT_HGB', 'TOT_HGBC', 'TOT_HGBD', 'TOT_HGC', 'TOT_HGCD', 
             'TOT_HGD', 'TOT_N97', 'TOT_BFI', 'TOT_CONTACT', 'TOT_RECHG', 'TOT_WB5100_ANN', 
             'TOT_CWD', 'TOT_ET', 'TOT_RH', 'TOT_TAV7100_ANN', 'TOT_WDANN', 'TOT_PPT7100_ANN', 
             'TILE_DRAIN', 'TOT_TILES92', 'TOT_NPDES_MAJ', 'TOT_NPDES_MAJ_DENS', 
             'TOT_NORM_STORAGE2013','TOT_MAJOR2013','TOT_NDAMS2013', 'TOT_NID_STORAGE2013',
             'TOT_LAKEPOND', 'TOT_RESERVOIR', 'TOT_SRL55AG', 'DTW', 'TRANSM', 'UNSAT_TT', 'WCON', 
             'NO3_DOM', 'NO3_PUB', 'fert_uN_mt_sqkm', 'NLCD_DEV', 'NLCD_FOR', 'NLCD_AG', 'NLCD_WTLND']
netcdf_location = '02_munge/out/model_input_rolling.nc'
site_data_dict = {}
for i, site_no in enumerate(site_info.site_no):
    site_data = ppf.xarray_to_df_mod_feat(netcdf_location, site_no, feat_list)
    #site_data = xarray.open_dataset('', group = site_no)
    #site_data_df = site_data.to_dataframe()
    site_data_df = site_data.iloc[0,:]
    site_data_dict[site_no] = site_data_df
    #site_data.close()
    print(site_no)

sd_sf = pd.DataFrame(site_data_dict.values(), index = site_data_dict.keys())
sd_sf['site_no'] = sd_sf.index
#%%
hydro_data = pd.read_csv('02_munge/out/all_sites_data_bfs.csv', dtype = {'site_no':str})
#%%
hydro_summary = hydro_data.groupby('site_no').agg(
    mean_no3=pd.NamedAgg('nitrate', np.mean),
    sd_no3 = pd.NamedAgg('nitrate', np.std),
    mean_q = pd.NamedAgg('discharge',np.mean),
    sd_q = pd.NamedAgg('discharge', np.mean))

#%%cq

def cq_slope(data):
    data = data.dropna(subset = ['discharge','nitrate'])
    data = data[data['nitrate'] > 0]
    slope = np.polyfit(np.log(data['baseflow']+1e-06), np.log10(data['nitrate']+1e-06), deg = 1)[0]
    return slope

hydro_cq = hydro_data.groupby('site_no').apply(cq_slope)

#%%
hydro_summary = hydro_summary.join(hydro_cq.rename('cq_slope').to_frame())

basin_char_hydro = sd_sf.join(hydro_summary, on = 'site_no')

basin_char_hydro.to_csv('02_munge/out/basin_char_full.csv')

