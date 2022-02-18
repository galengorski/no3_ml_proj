#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb  4 18:57:47 2022

@author: galengorski
"""
#import netCDF4
import datetime
import math
import netCDF4
from netCDF4 import Dataset,num2date,date2num
import numpy as np
import pandas as pd
import time
import xarray

#%%USEFUL CODE
a = xarray.open_dataset('02_munge/out/model_input_01.nc', group = '01408500')#, decode_times=False)
a = xarray.load_dataset('02_munge/out/model_input_01.nc', group = '01408500')
df = a.to_dataframe()
date_time = a.variables['Date'].to_index()
df = df.set_index(date_time[:])

a.close()

start = time.time()
df = xarray_to_df('02_munge/out/model_input_01.nc', '01463500')
end = time.time()
print(end - start)

#%%FUNCTIONS
def xarray_to_df(netcdf_location, site_no):
    '''reads in a single site from the netcdf and '''
    site_data = xarray.open_dataset(netcdf_location, group = site_no)
    site_data_df = site_data.to_dataframe()
    date_time = site_data['Date'].to_index()
    site_data_df = site_data_df.set_index(date_time[:])
    site_data_df.loc[site_data_df.Nitrate < 0 , 'Nitrate'] = np.nan    
    site_data.close()
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
    data.Nitrate.mean()

    #recombine into single dataframe
    full_data = pd.concat([train_norm, val_norm, test_norm])
    
    return full_data, nobs_train, nobs_val, nobs_test

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
    train_dates = list(data_no_nans_seq_len.Nitrate.iloc[0:nobs_train].index)
    val_y = data_no_nans_seq_len.Nitrate.iloc[nobs_train:(nobs_train+nobs_val)].to_numpy()
    val_dates = list(data_no_nans_seq_len.Nitrate.iloc[nobs_train:(nobs_train+nobs_val)].index)
    test_y = data_no_nans_seq_len.Nitrate.iloc[(nobs_train+nobs_val):(nobs_train+nobs_val+nobs_test)].to_numpy()
    test_dates = list(data_no_nans_seq_len.Nitrate.iloc[(nobs_train+nobs_val):(nobs_train+nobs_val+nobs_test)].index)
    
    #this is the number of nitrate observations that have seq_len days of 
    #predictors before them in the dataset, for example if we have a nitrate observation
    #on the first day, we don't have any predictor data for the previous seq_len days for that 
    #observation
    nobs = data_no_nans[data_no_nans.index > data.index[seq_len]].shape[0]
       
    combined = np.empty([nobs, seq_len, data_no_nans.shape[1]])    
    
    data = data.drop(columns = 'Nitrate')
    
    for i, curr_idx in enumerate(data_no_nans_seq_len.index):
        date_lst = list(data.index == curr_idx)
        data_date_idx = int(which(date_lst)[0])
        if data_date_idx < seq_len:
            continue
        temp_split = data.iloc[(data_date_idx-seq_len):data_date_idx,:]
        combined[i,:,:] = temp_split
        
    train_x = combined[0:nobs_train,:,:]
    val_x = combined[nobs_train:(nobs_train+nobs_val),:,:]
    test_x = combined[(nobs_train+nobs_val):(nobs_train+nobs_val+nobs_test),:,:]
    
    return combined, train_x, val_x, combined_y, test_x, train_y, val_y, test_y#, train_dates, val_dates, test_dates
#%%    
step = []
step.append(time.time())
df = xarray_to_df('02_munge/out/model_input_01.nc', '01463500')
step.append(time.time())
df = df.drop(columns = 'Date')
data_split, nobs_train, nobs_val, nobs_test = split_norm_combine(df, 365, .5625, .1875, .25)
step.append(time.time())
#df_dyn = df.loc[:,'Discharge':'TempMin']
full_x, train_x, val_x, test_x, full_y, train_y, val_y, test_y = prepare_data(data_split, 365, nobs_train, nobs_val, nobs_test)
step.append(time.time())
print(step[1]-step[0], step[2] - step[1], step[3] - step[2]) 