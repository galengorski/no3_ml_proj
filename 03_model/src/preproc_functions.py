#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb  4 18:57:47 2022

@author: galengorski
"""
#import netCDF4
import datetime
import math
import numpy as np
import pandas as pd
import torch
import xarray


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


def split_norm_combine(data, seq_len, trn_frac, val_frac, test_frac):
    
    #get rid of nitrate na values
    data_no_nans = data['Nitrate'].dropna()
    #this gets rid of any data before the first available nitrate observation with
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]
    
    #get the split points based on the availability of nitrate data
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
    test = data[start_test_date:end_test_date]
    nobs_test = test.Nitrate.dropna().shape[0]
    
    #normalize sets separately
    train_norm = (train - train.mean(axis=0)) / train.std(axis=0)
    val_norm = (val - val.mean(axis=0)) / val.std(axis=0)
    test_norm = (test - test.mean(axis=0)) / test.std(axis=0)
    
    #dictionary of nitrate means and stds
    n_means_stds = {'full_mean' : data.Nitrate.mean(),
    'full_std' : data.Nitrate.std(),
    'train_mean' : train.Nitrate.mean(),
    'train_std' : train.Nitrate.std(),
    'val_mean' : val.Nitrate.mean(),
    'val_std' : val.Nitrate.std(),
    'test_mean' : test.Nitrate.mean(),
    'test_std' : test.Nitrate.std()}
    


    #recombine into single dataframe
    full_data = pd.concat([train_norm, val_norm, test_norm])
    
    print('data split and normalized')
    
    return full_data, nobs_train, nobs_val, nobs_test, n_means_stds

def prepare_data(data, seq_len, nobs_train, nobs_val, nobs_test):
    '''returns an array with dim [data length, seq_len, num features]'''
    
    which = lambda lst:list(np.where(lst)[0])
    
    #get rid of nitrate na values
    data_no_nans = data[data['Nitrate'].notnull()]
    #this gets rid of any data before the first available nitrate observation with
    #seq len predictors before it
    data_no_nans_seq_len = data_no_nans[data_no_nans.index > data.index[seq_len]]
    
    #split the nitrate data
    combined_y = data_no_nans_seq_len.Nitrate.to_numpy()
    train_y = data_no_nans_seq_len.Nitrate.iloc[0:nobs_train].to_numpy()
    train_dates = np.stack(data_no_nans_seq_len.Nitrate.iloc[0:nobs_train].index)
    val_y = data_no_nans_seq_len.Nitrate.iloc[nobs_train:(nobs_train+nobs_val)].to_numpy()
    val_dates = np.stack(data_no_nans_seq_len.Nitrate.iloc[nobs_train:(nobs_train+nobs_val)].index)
    test_y = data_no_nans_seq_len.Nitrate.iloc[(nobs_train+nobs_val):(nobs_train+nobs_val+nobs_test)].to_numpy()
    test_dates = np.stack(data_no_nans_seq_len.Nitrate.iloc[(nobs_train+nobs_val):(nobs_train+nobs_val+nobs_test)].index)
    
    #this is the number of nitrate observations that have seq_len days of 
    #predictors before them in the dataset, for example if we have a nitrate observation
    #on the first day, we don't have any predictor data for the previous seq_len days for that 
    #observation
    nobs = data_no_nans[data_no_nans.index > data.index[seq_len]].shape[0]   
    #get rid of nitrate in the predictors
    data = data.drop(columns = 'Nitrate')
    #create empty array 
    combined_x = np.empty([nobs, seq_len, data.shape[1]])
    
    for i, curr_idx in enumerate(data_no_nans_seq_len.index):
        date_lst = list(data.index == curr_idx)
        data_date_idx = int(which(date_lst)[0])
        if data_date_idx < seq_len:
            continue
        temp_split = data.iloc[(data_date_idx-seq_len):data_date_idx,:]
        combined_x[i,:,:] = temp_split
        
    train_x = combined_x[0:nobs_train,:,:]
    val_x = combined_x[nobs_train:(nobs_train+nobs_val),:,:]
    test_x = combined_x[(nobs_train+nobs_val):(nobs_train+nobs_val+nobs_test),:,:]
    
    train_x, train_y = torch.from_numpy(np.array(train_x)).float(), torch.from_numpy(np.array(train_y)).float()
    val_x, val_y = torch.from_numpy(np.array(val_x)).float(), torch.from_numpy(np.array(val_y)).float()
    full_x, full_y = torch.from_numpy(np.array(combined_x)).float(), torch.from_numpy(np.array(combined_y)).float()    

    
    print('data prepped')
    
    return full_x, train_x, val_x, test_x, full_y, train_y, val_y, test_y, train_dates, val_dates, test_dates
