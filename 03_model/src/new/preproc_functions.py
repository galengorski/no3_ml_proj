#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb  4 18:57:47 2022

@author: galengorski
"""
#import netCDF4
import datetime
import math
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import pickle
import torch
import xarray
import yaml

if torch.cuda.is_available():
    device = "cuda:0"
else:
    device = "cpu"

def xarray_to_df(netcdf_location, site_no, feat_list):
    '''reads in a single site from the netcdf and '''
    site_data = xarray.open_dataset(netcdf_location, group = site_no)
    site_data_df = site_data.to_dataframe()
    date_time = site_data['Date'].to_index()
    site_data_df = site_data_df.set_index(date_time[:])
    site_data_df.loc[site_data_df.Nitrate < 0 , 'Nitrate'] = np.nan 
    site_data_df = site_data_df[feat_list]
    site_data.close()
    print(site_no,' data read to dataframe')
    return site_data_df

def xarray_to_df_mod_feat(netcdf_location, site_no, feat_list):
    '''reads in a single site from the netcdf and '''
    add_feat = ['NLCD_11', 'NLCD_21', 'NLCD_22', 'NLCD_23', 'NLCD_24', 'NLCD_31', 'NLCD_41', 
                 'NLCD_42', 'NLCD_43', 'NLCD_52', 'NLCD_71', 'NLCD_81', 'NLCD_82', 'NLCD_90', 'NLCD_95']
    feat_list_c = feat_list.copy()
    feat_list_c.extend(add_feat)

    site_data = xarray.open_dataset(netcdf_location, group = site_no)
    site_data_df = site_data.to_dataframe()
    date_time = site_data['Date'].to_index()
    site_data_df = site_data_df.set_index(date_time[:])
    site_data_df.loc[site_data_df.Nitrate < 0 , 'Nitrate'] = np.nan 
    
    site_data_df['NLCD_DEV'] = site_data_df['NLCD_21']+site_data_df['NLCD_22']+site_data_df['NLCD_23']+site_data_df['NLCD_24']
    site_data_df['NLCD_FOR'] = site_data_df['NLCD_41']+site_data_df['NLCD_42']+site_data_df['NLCD_43']
    site_data_df['NLCD_AG'] = site_data_df['NLCD_81']+site_data_df['NLCD_82']
    site_data_df['NLCD_WTLND'] = site_data_df['NLCD_90']+site_data_df['NLCD_95']
    site_data_df['fert_uN_mt_kmAg'] = site_data_df['fert_uN_mt_sqkm']*(site_data_df['NLCD_82']*site_data_df['TOT_BASIN_AREA'])
    
    feat_list_c = [e for e in feat_list_c if e not in add_feat]
    
    site_data_df = site_data_df[feat_list_c]
    
    site_data.close()
    print(site_no,' data read to dataframe')
    return site_data_df

def split_norm_combine(data, seq_len, trn_frac, val_frac, test_frac):
    
    #get rid of nitrate na values
    data_no_nans = data['Nitrate'].dropna()
    #this gets rid of any data before the first available nitrate observation with
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]
    
    #get the split points based on the availability of nitrate data
    start_train_date = data_no_nans_seq_len.index[0]
    end_train_date = data_no_nans_seq_len.index[math.floor(trn_frac*data_no_nans_seq_len.shape[0])-1]
    start_val_date = end_train_date+datetime.timedelta(days=1)
    end_val_date = data_no_nans_seq_len.index[math.floor((1-test_frac)*data_no_nans_seq_len.shape[0])-1]
    start_test_date = end_val_date+datetime.timedelta(days=1)
    end_test_date = data_no_nans_seq_len.index[data_no_nans_seq_len.shape[0]-1] 
        
    #split train validate and test splits
    train = data[:end_train_date]
    nobs_train = train.Nitrate.dropna().shape[0]
    val = data[start_val_date:end_val_date]
    nobs_val = val.Nitrate.dropna().shape[0]
    train_val = data[:end_val_date]
    nobs_train_val = train_val.Nitrate.dropna().shape[0]
    test = data[start_test_date:end_test_date]
    nobs_test = test.Nitrate.dropna().shape[0]
    
    #normalize sets separately
    train_norm = (train - train.mean(axis=0)) / train.std(axis=0)
    val_norm = (val - train.mean(axis=0)) / train.std(axis=0)
    train_val_norm = (train_val - train_val.mean(axis=0)) / train_val.std(axis=0)
    test_norm = (test - train.mean(axis=0)) / train.std(axis=0)
    
    #dictionary of nitrate means and stds
    n_means_stds = {'full_mean' : data.Nitrate.mean(),
    'full_std' : data.Nitrate.std(),
    'train_mean' : train.Nitrate.mean(),
    'train_std' : train.Nitrate.std(),
    'train_val_mean': train_val.Nitrate.mean(),
    'train_val_std': train_val.Nitrate.std(),
    'val_mean' : val.Nitrate.mean(),
    'val_std' : val.Nitrate.std(),
    'test_mean' : test.Nitrate.mean(),
    'test_std' : test.Nitrate.std()}
    


    #recombine into single dataframe
    full_data = pd.concat([train_norm, val_norm, test_norm])
    
    print('data split and normalized')
    
    return full_data, nobs_train, nobs_val, nobs_train_val, nobs_test, n_means_stds, start_train_date, end_train_date, start_val_date, end_val_date, start_test_date, end_test_date

def split_multi_site_data(data, seq_len, trn_frac, val_frac, test_frac):
    
    #get rid of nitrate na values
    data_no_nans = data['Nitrate'].dropna()
    #this gets rid of any data before the first available nitrate observation with
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]
    
    #get the split points based on the availability of nitrate data
    start_train_date = data_no_nans_seq_len.index[0]
    end_train_date = data_no_nans_seq_len.index[math.floor(trn_frac*data_no_nans_seq_len.shape[0])-1]
    start_val_date = end_train_date+datetime.timedelta(days=1)
    end_val_date = data_no_nans_seq_len.index[math.floor((1-test_frac)*data_no_nans_seq_len.shape[0])-1]
    start_test_date = end_val_date+datetime.timedelta(days=1)
    end_test_date = data_no_nans_seq_len.index[data_no_nans_seq_len.shape[0]-1] 
        
    #split train validate and test splits
    train = data[:end_train_date]
    val = data[start_val_date:end_val_date]
    train_val = data[:end_val_date]
    test = data[start_test_date:end_test_date]
    
    
    return data_no_nans, train, val, train_val, test, start_train_date, end_train_date, start_val_date, end_val_date, start_test_date, end_test_date

def normalize_multi_site_data(data, train, val, train_val, test):
    train_site_v = train['site_no']
    val_site_v = val['site_no']
    train_val_site_v = train_val['site_no']
    test_site_v = test['site_no']
    
    #remove site numbers
    train = train.drop(columns = ['site_no'])
    val = val.drop(columns = ['site_no'])
    train_val = train_val.drop(columns = ['site_no'])
    test = test.drop(columns = ['site_no'])

    #normalize sets separately
    train_norm = (train - train.mean(axis=0)) / train.std(axis=0)
    val_norm = (val - train.mean(axis=0)) / train.std(axis=0)
    train_val_norm = (train_val - train_val.mean(axis=0)) / train_val.std(axis=0)
    test_norm = (test - train.mean(axis=0)) / train.std(axis=0)
    
    #add site no back in
    train_norm['site_no'] = train_site_v
    val_norm['site_no'] = val_site_v
    train_val_norm['site_no'] = train_val_site_v
    test_norm['site_no'] = test_site_v
    
    #dictionary of nitrate means and stds
    n_means_stds = {'full_mean' : data.Nitrate.mean(),
    'full_std' : data.Nitrate.std(),
    'train_mean' : train.Nitrate.mean(),
    'train_std' : train.Nitrate.std(),
    'val_mean' : val.Nitrate.mean(),
    'val_std' : val.Nitrate.std(),
    'train_val_mean': train_val.Nitrate.mean(),
    'train_val_std': train_val.Nitrate.std(),
    'test_mean' : test.Nitrate.mean(),
    'test_std' : test.Nitrate.std()}
    
    return train_norm, val_norm, train_val_norm, test_norm, n_means_stds



def prepare_data(data, seq_len, start_train_date, end_train_date, start_val_date, end_val_date, start_test_date, end_test_date):
    '''returns an array with dim [data length, seq_len, num features]'''
    
    #define a function which that finds the row with nan
    which = lambda lst:list(np.where(lst)[0])
        
   
    #get rid of nitrate na values
    data_no_nans = data['Nitrate'].dropna()
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]

    #combined_y = data_no_nans_seq_len
    train_y = data_no_nans_seq_len[start_train_date:end_train_date]
    train_dates = data_no_nans_seq_len[start_train_date:end_train_date].index
    nobs_train = data_no_nans_seq_len[start_train_date:end_train_date].shape[0]
    
    val_y = data_no_nans_seq_len[start_val_date:end_val_date]
    val_dates = data_no_nans_seq_len[start_val_date:end_val_date].index
    nobs_val = data_no_nans_seq_len[start_val_date:end_val_date].shape[0]
    
    test_y = data_no_nans_seq_len[start_test_date:end_test_date]
    test_dates = data_no_nans_seq_len[start_test_date:end_test_date].index
    nobs_test = data_no_nans_seq_len[start_test_date:end_test_date].shape[0]
    
    full_y = data.Nitrate[seq_len:]
    
    
    #this is the number of nitrate observations that have seq_len days of 
    #predictors before them in the dataset, for example if we have a nitrate observation
    #on the first day, we don't have any predictor data for the previous seq_len days for that 
    #observation
    nobs = data_no_nans[data_no_nans.index > data.index[seq_len]].shape[0]   
    #get rid of nitrate in the predictors
    data = data.drop(columns = 'Nitrate')

    #this will only prepare the data on days with nitrate data available
    #create empty array 
    combined_x = np.empty([nobs, seq_len, data.shape[1]])

    for i, curr_idx in enumerate(data_no_nans_seq_len.index):
        date_lst = list(data.index == curr_idx)
        data_date_idx = int(which(date_lst)[0])
        if data_date_idx < seq_len:
            continue
        temp_split = data.iloc[(data_date_idx-seq_len):data_date_idx,:]
        combined_x[i,:,:] = temp_split
     
    #this will prepare the data for all days when drivers are available
    #prepare the full data set
    full_combined_x = np.empty([len(data)-seq_len, seq_len, data.shape[1]])
    
    for i, curr_idx in enumerate(data.iloc[seq_len:,].index):
        date_lst = list(data.index == curr_idx)
        data_date_idx = int(which(date_lst)[0])
        if data_date_idx < seq_len:
            continue
        temp_split = data.iloc[(data_date_idx-seq_len):data_date_idx,:]
        full_combined_x[i,:,:] = temp_split
        
    train_x = combined_x[0:nobs_train,:,:]
    val_x = combined_x[nobs_train:(nobs_train+nobs_val),:,:]
    trainval_x = combined_x[0:(nobs_train+nobs_val),:,:]
    test_x = combined_x[(nobs_train+nobs_val):(nobs_train+nobs_val+nobs_test),:,:]
    
    
    train_x_t, train_y_t = torch.from_numpy(np.array(train_x)).float(), torch.from_numpy(np.array(train_y)).float()
    val_x_t, val_y_t = torch.from_numpy(np.array(val_x)).float(), torch.from_numpy(np.array(val_y)).float()
    trainval_x_t, trainval_y_t = torch.from_numpy(np.array(trainval_x)).float(), torch.from_numpy(np.array(train_y.append(val_y))).float()
    test_x_t, test_y_t = torch.from_numpy(np.array(test_x)).float(), torch.from_numpy(np.array(test_y)).float()
    full_x_t, full_y_t = torch.from_numpy(np.array(full_combined_x)).float(), torch.from_numpy(np.array(full_y)).float()    
   
    print('data prepped')
    
    return full_x_t, train_x_t, val_x_t, trainval_x_t, test_x_t, full_y_t, train_y_t, val_y_t, trainval_y_t, test_y_t, train_dates, val_dates, test_dates

def full_prepare_multi_site_data(netcdf_loc, config_loc, site_no_list, station_nm_list, out_dir):

    with open(config_loc) as stream:
        config = yaml.safe_load(stream)
    
    
    seq_len = config['seq_len']
    trn_frac = config['trn_frac']
    val_frac = config['val_frac']
    test_frac = config['test_frac']
    
    feat_list = config['feat_list']
    if config['static_features_used']:
        static_features = config['static_features']
        feat_list.extend(static_features)
    
    train_data_all_sites = pd.DataFrame()
    val_data_all_sites = pd.DataFrame()
    train_val_data_all_sites = pd.DataFrame()
    test_data_all_sites = pd.DataFrame()
    
    train_range = {}
    val_range = {}
    train_val_range = {}
    test_range = {}
    #full_range = {}
    
    for site_no in site_no_list:
        #convert single site from xarray group to dataframe
        df = xarray_to_df_mod_feat(netcdf_loc, site_no, feat_list)
        if len(df.columns[df.isna().any()].tolist()) > 0:
            print(site_no+ " nans found:",df.columns[df.isna().any()].tolist())
        #add individual site no
        df['site_no'] = site_no
        #split data into train, val, test splits individually by site
        data_no_nans, train, val, train_val, test, start_train_date, end_train_date, start_val_date, end_val_date, start_test_date, end_test_date = split_multi_site_data(df, seq_len, trn_frac, val_frac, test_frac)
        #print(nobs_train)
        #recombine into single dataframe
        train_data_all_sites = pd.concat([train_data_all_sites, train])
        val_data_all_sites = pd.concat([val_data_all_sites, val])
        train_val_data_all_sites = pd.concat([train_val_data_all_sites, train_val])
        test_data_all_sites = pd.concat([test_data_all_sites, test])
        
        train_range[site_no] = {'From':0, 'To':0}
        train_range[site_no]['From'] = start_train_date
        train_range[site_no]['To'] = end_train_date
        
        val_range[site_no] = {'From':0, 'To':0}
        val_range[site_no]['From'] = start_val_date
        val_range[site_no]['To'] = end_val_date
        
        train_val_range[site_no] = {'From':0, 'To':0}
        train_val_range[site_no]['From'] = start_train_date
        train_val_range[site_no]['To'] = end_val_date
        
        test_range[site_no] = {'From':0, 'To':0}
        test_range[site_no]['From'] = start_test_date
        test_range[site_no]['To'] = end_test_date
        
    
    #normalize data as a full data set wtih all sites
    full_data_all_sites = pd.concat([train_data_all_sites,val_data_all_sites,test_data_all_sites])
    train_norm, val_norm, train_val_norm, test_norm, n_means_stds = normalize_multi_site_data(full_data_all_sites, train_data_all_sites, val_data_all_sites, train_val_data_all_sites, test_data_all_sites)
    
    #initiate empty tensors for the input data
    train_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    train_y = torch.empty((0), dtype=torch.float, device = device)
    train_dates = np.empty([0], dtype='datetime64[ns]')
    
    val_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    val_y = torch.empty((0), dtype=torch.float, device = device)
    val_dates = np.empty([0], dtype='datetime64[ns]')
    
    train_val_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    train_val_y = torch.empty((0), dtype=torch.float, device = device)
    train_val_dates = np.empty([0], dtype='datetime64[ns]')

    test_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    test_y = torch.empty((0), dtype=torch.float, device = device)
    test_dates = np.empty([0], dtype='datetime64[ns]')
    
    full_x = torch.empty((0,seq_len,len(feat_list)-1), dtype=torch.float, device = device)
    full_y = torch.empty((0), dtype=torch.float, device = device)
    full_dates = np.empty([0], dtype='datetime64[ns]')
    
    solute_mean = {}
    solute_std = {}
    
    #the indices dictionaries are for storing the indices for each watershed's data
    #so that the data can be split correctly after modelings
    train_indices = {}
    val_indices = {}
    train_val_indices = {}
    test_indices = {}
    full_indices = {}

    #split data back up for creating the input sequences
    for site_no in site_no_list:
        print(site_no)
        train_single_site = train_norm[train_norm['site_no'] == site_no]
        val_single_site = val_norm[val_norm['site_no'] == site_no]
        train_val_single_site = train_val_norm[train_val_norm['site_no'] == site_no]
        test_single_site = test_norm[test_norm['site_no'] == site_no]
        
        #create a dirctory for the site
        out_path = os.path.join(out_dir,site_no)
        os.makedirs(out_path, exist_ok = True)
        #save a plot of the time series showing the splits
        save_nitrate_plot(train_single_site, val_single_site, test_single_site, out_dir, site_no)
        
        full_data_single_site = pd.concat([train_single_site, val_single_site, test_single_site])
        
        #some static attributes might have nans if all the sites  have the same value
        cols_with_nan = full_data_single_site.columns[full_data_single_site.isna().any()].tolist()
        cols = [s for s in cols_with_nan if s != 'Nitrate']
        
        if len(cols) >0:
            full_data_single_site[cols] = full_data_single_site[cols].fillna(0)
            print('nans found and filled with zeroes in '+site_no+':',cols)
        
        full_data_single_site = full_data_single_site.drop(columns = 'site_no')
        
        site_start_train_date = train_range[site_no]['From']
        site_end_train_date = train_range[site_no]['To']
        site_start_val_date = val_range[site_no]['From']
        site_end_val_date = val_range[site_no]['To']
        site_start_test_date = test_range[site_no]['From']
        site_end_test_date = test_range[site_no]['To']
        
        full_x_site, train_x_site, val_x_site, train_val_x_site, test_x_site, full_y_site, train_y_site, val_y_site, train_val_y_site, test_y_site, train_dates_site, val_dates_site, test_dates_site = prepare_data(full_data_single_site, seq_len, site_start_train_date, site_end_train_date, site_start_val_date, site_end_val_date, site_start_test_date, site_end_test_date)
        
        #iteratively fill all the tensors by concatenation with their constituent parts
        train_indices[site_no] = {"From":0,"To":0}
        train_indices[site_no]["From"] = train_x.shape[0]
        train_x = torch.cat((train_x, train_x_site), dim = 0)
        train_indices[site_no]["To"] = train_x.shape[0]
        train_y = torch.cat((train_y, train_y_site), dim = 0)
        
        train_dates = np.concatenate((train_dates, train_dates_site), axis = 0)

        #validation
        val_indices[site_no] = {"From":0,"To":0}
        val_indices[site_no]["From"] = val_x.shape[0]
        val_x = torch.cat((val_x, val_x_site), dim = 0)
        val_indices[site_no]["To"] = val_x.shape[0]
        val_y = torch.cat((val_y, val_y_site), dim = 0)
        
        val_dates = np.concatenate((val_dates, val_dates_site), axis = 0)
        
        #train validation together
        train_val_indices[site_no] = {"From":0,"To":0}
        train_val_indices[site_no]["From"] = train_val_x.shape[0]
        train_val_x = torch.cat((train_val_x, train_val_x_site), dim = 0)
        train_val_indices[site_no]["To"] = train_val_x.shape[0]
        train_val_y = torch.cat((train_val_y, train_val_y_site), dim = 0)
        
        train_val_dates = np.concatenate((train_val_dates, train_dates_site, val_dates_site), axis = 0)

        
        #testing
        test_indices[site_no] = {"From":0,"To":0}
        test_indices[site_no]["From"] = test_x.shape[0]
        test_x = torch.cat((test_x, test_x_site), dim = 0)
        test_indices[site_no]["To"] = test_x.shape[0]
        test_y = torch.cat((test_y, test_y_site), dim = 0)
        
        test_dates = np.concatenate((test_dates, test_dates_site), axis = 0)
        
        #full
        full_indices[site_no] = {"From":0,"To":0}
        full_indices[site_no]["From"] = full_x.shape[0]
        full_x = torch.cat((full_x, full_x_site), dim = 0)
        full_indices[site_no]["To"] = full_x.shape[0]
        full_y = torch.cat((full_y, full_y_site), dim = 0)
        
        full_dates = np.concatenate((full_dates, full_data_single_site.index), axis = 0)
        
    concat_data = {}
    concat_data['train_x'] = train_x
    concat_data['train_y'] = train_y
    concat_data['train_dates'] = train_dates
    
    concat_data['val_x'] = val_x
    concat_data['val_y'] = val_y
    concat_data['val_dates'] = val_dates
    
    concat_data['train_val_x'] = train_val_x
    concat_data['train_val_y'] = train_val_y
    concat_data['train_val_dates'] = train_val_dates

    concat_data['test_x'] = test_x
    concat_data['test_y'] = test_y
    concat_data['test_dates'] = test_dates
    
    concat_data['full_x'] = full_x
    concat_data['full_y'] = full_y
    concat_data['full_dates'] = full_dates

    concat_data['solute_mean'] = solute_mean
    concat_data['solute_std'] = solute_std
    concat_data['train_indices'] = train_indices
    concat_data['val_indices'] = val_indices
    concat_data['train_val_indices'] = train_val_indices
    concat_data['test_indices'] = test_indices
    concat_data['full_indices'] = full_indices
    
    concat_data_file = open(os.path.join(out_dir,'prepped_data'),'wb')
    pickle.dump(concat_data, concat_data_file)
    concat_data_file.close()

    n_means_stds_file = open(os.path.join(out_dir,'n_means_stds'),'wb')
    pickle.dump(n_means_stds, n_means_stds_file)
    n_means_stds_file.close()

    return concat_data, n_means_stds

def full_prepare_single_site_data(netcdf_loc, config_loc, site_no, station_nm, out_dir, hp_tune, hp_tune_vals):
    with open(config_loc) as stream:
            config = yaml.safe_load(stream)
    
    
    if hp_tune:
        seq_len = hp_tune_vals['seq_len']
    else:
        seq_len = config['seq_len']

    trn_frac = config['trn_frac']
    val_frac = config['val_frac']
    test_frac = config['test_frac']
    
    feat_list = config['feat_list']
    if config['static_features_used']:
        static_features = config['static_features']
        feat_list.extend(static_features)
    
    df = xarray_to_df_mod_feat(netcdf_loc, site_no, feat_list)
    if len(df.columns[df.isna().any()].tolist()) > 0:
        print(site_no+ " nans found:",df.columns[df.isna().any()].tolist())
    #add individual site no
    #split and normalize input data
    full_data, nobs_train, nobs_val, nobs_train_val, nobs_test, n_means_stds, start_train_date, end_train_date, start_val_date, end_val_date, start_test_date, end_test_date = split_norm_combine(df, seq_len, trn_frac, val_frac, test_frac)
    #arrange input data into arrays of [data_len, seq_len, num_feat]
    full_x, train_x, val_x, train_val_x, test_x, full_y, train_y, val_y, train_val_y, test_y, train_dates, val_dates, test_dates = prepare_data(full_data, seq_len, start_train_date, end_train_date, start_val_date, end_val_date, start_test_date, end_test_date)

    site_data = {}
    site_data['train_x'] = train_x
    site_data['train_y'] = train_y
    site_data['train_dates'] = train_dates
    
    site_data['val_x'] = val_x
    site_data['val_y'] = val_y
    site_data['val_dates'] = val_dates
    
    site_data['train_val_x'] = train_val_x
    site_data['train_val_y'] = train_val_y
    site_data['train_val_dates'] = np.concatenate((train_dates, val_dates), axis = 0)

    site_data['test_x'] = test_x
    site_data['test_y'] = test_y
    site_data['test_dates'] = test_dates
    
    site_data['full_x'] = full_x
    site_data['full_y'] = full_y
    #site_data['full_dates'] = full_dates
    
    site_data_file = open(os.path.join(out_dir,'prepped_data'),'wb')
    pickle.dump(site_data, site_data_file)
    site_data_file.close()

    n_means_stds_file = open(os.path.join(out_dir,'n_means_stds'),'wb')
    pickle.dump(n_means_stds, n_means_stds_file)
    n_means_stds_file.close()

    return site_data, n_means_stds

def save_nitrate_plot(train_single_site, val_single_site, test_single_site, out_dir, site_no):
    plt.plot(train_single_site.Nitrate, 'b', label = 'Training')
    plt.plot(val_single_site.Nitrate, 'g', label = 'Validation')
    plt.plot(test_single_site.Nitrate, 'r', label = 'Testing')
    plt.ylabel('Nitrate (mg/L)')
    plt.title(train_single_site.site_no[0])
    plt.legend()
    plt.savefig(os.path.join(out_dir,site_no,'splits.png'))
    plt.close()