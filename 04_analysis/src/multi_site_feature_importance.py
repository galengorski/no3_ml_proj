#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug  7 18:42:26 2022

@author: galengorski
"""
import lstm_modeling_functions as lmf
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pickle
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
import yaml

#%% #read in model configs
with open('03_model/multi_site_model_config.yaml') as stream:
    config = yaml.safe_load(stream)  

device = 'cpu'
learning_rate = config['learning_rate']
seq_len = config['seq_len']
num_layers = config['num_layers']
    
feat_list = config['feat_list']

static_features = config['static_features']    
feat_list.extend(static_features)
num_features = config['num_features']

batch_size = config['batch_size']
dropout = config['dropout']
weight_decay = config['weight_decay']
hidden_size = config['hidden_size']
shuffle = config['shuffle']

#%%
with open('03_model/out/multi_site/Run_04_DAM/Rep_00/prepped_data', 'rb') as input_data:
            concat_model_data = pickle.load(input_data)
            
#prepare the train val dataset
train_val_dataset = lmf.CatchmentDataset(concat_model_data['train_val_x'], concat_model_data['train_val_y'])

#%%Define a permuatation feature importance function
def calc_permutation_feature_importance(model, concat_model_data, feat_list):
    train_val_dataset = lmf.CatchmentDataset(concat_model_data['train_val_x'], concat_model_data['train_val_y'])
    _,_,mse_original = model.report_mse(train_val_dataset)
    fi_ls = []
    for i in range(len(feat_list)-1): 
        print(feat_list[i+1])
        x_hypothesis = concat_model_data['train_val_x'].detach().clone()
        var_range = torch.quantile(x_hypothesis[:,:,i].flatten(), torch.tensor([.1,.9]))
        x_hypothesis[:,:,i] = (var_range[0]-var_range[1])*torch.rand_like(x_hypothesis[:, :, i])+var_range[1]
        train_val_dataset_hypothesis = lmf.CatchmentDataset(x_hypothesis, concat_model_data['train_val_y'])
        _,_,mse_hypothesis = model.report_mse(train_val_dataset_hypothesis)
        delta_rmse = (np.sqrt(mse_hypothesis) - np.sqrt(mse_original))/np.sqrt(mse_original)
        fi_ls.append(delta_rmse)
    return np.array(fi_ls)

#%%calculate feature importance
feat_imp_reps = np.zeros([len(feat_list)-1,5])
for i in range(5):
# initialize the model
    model = lmf.LSTM_layer(num_features, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay)
    model = model.to(device)
    #load the model weights
    model.load_state_dict(torch.load("03_model/out/multi_site/Run_04_DAM/Rep_0"+str(i)+"/model_weights.pt"))

    feat_imp = calc_permutation_feature_importance(model, concat_model_data, feat_list)
    
    feat_imp_reps[:,i] = feat_imp

#%%
feat_imp_mean = np.mean(feat_imp_reps, axis = 1)
feat_imp_min = np.min(feat_imp_reps, axis = 1)
feat_imp_max = np.max(feat_imp_reps, axis = 1)
imp_error = np.std(feat_imp_reps, axis = 1)

ordered_feat_imp = np.argsort(feat_imp_mean)
np.array(feat_list[1:])[ordered_feat_imp]
y_pos = np.arange(len(feat_list)-1)

plt.rcdefaults()
fig, ax = plt.subplots()
fig.set_size_inches(18.5, 10.5)

ax.barh(np.array(feat_list[1:])[ordered_feat_imp],feat_imp_mean[ordered_feat_imp], 
        xerr = imp_error[ordered_feat_imp], align = 'center')
#ax.invert_yaxis()
ax.set_xlabel('Delta mse')
ax.set_title('Feature importance')
plt.tight_layout()
plt.show()
fig.savefig('04_analysis/figs/Multi_Site_Feature_Importance_DAM.png')

#%%
multi_site_feature_imp = pd.DataFrame({'feat': feat_list[1:], 'feat_imp_mean' : feat_imp_mean, 'feat_imp_std' : imp_error})


multi_site_feature_imp.to_csv('04_analysis/out/multi_site_ensemble_feature_importance.csv')

multi_site_feature_imp[multi_site_feature_imp['feat_imp_mean'] >= 0.25].sort_values(by = 'feat_imp_mean', ascending= False)
