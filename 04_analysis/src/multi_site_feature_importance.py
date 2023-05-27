#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug  7 18:42:26 2022

@author: galengorski
"""
import sys
sys.path.append('03_model/src/new')
import lstm_modeling_functions as lmf
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import pickle
import torch
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

with open('03_model/multi_site_run_config.yaml') as stream:
    run_config = yaml.safe_load(stream) 

model_run_id = run_config['model_run_id']
n_reps = run_config['n_reps']


#%%Define a permuatation feature importance function
def calc_permutation_feature_importance(model, concat_model_data, feat_list):
    train_val_dataset = lmf.CatchmentDataset(concat_model_data['train_val_x'], concat_model_data['train_val_y'])
    preds_original,labs_original,mse_original = model.report_mse(train_val_dataset)
    #fi_ls = []
    feat_imp_df = pd.DataFrame()
    for i in range(len(feat_list)-1): 
        print(feat_list[i+1])
        x_hypothesis = concat_model_data['train_val_x'].detach().clone()
        var_range = torch.quantile(x_hypothesis[:,:,i].flatten(), torch.tensor([.1,.9]))
        x_hypothesis[:,:,i] = (var_range[0]-var_range[1])*torch.rand_like(x_hypothesis[:, :, i])+var_range[1]
        train_val_dataset_hypothesis = lmf.CatchmentDataset(x_hypothesis, concat_model_data['train_val_y'])
        preds_hypothesis,labs_hypothesis,mse_hypothesis = model.report_mse(train_val_dataset_hypothesis)
        delta_rmse = (np.sqrt(mse_hypothesis) - np.sqrt(mse_original))/np.sqrt(mse_original)
        #fi_ls.append(delta_rmse)
        
        sites = list(concat_model_data['train_val_indices'].keys())
        site_rmse_vals = {}

        for site in sites:
            
                site_preds_hypothesis = preds_hypothesis[concat_model_data['train_val_indices'][site]['From']:concat_model_data['train_val_indices'][site]['To']]
                site_labels_hypothesis = labs_hypothesis[concat_model_data['train_val_indices'][site]['From']:concat_model_data['train_val_indices'][site]['To']]
                rmse_hypothesis = np.sqrt(np.mean((site_preds_hypothesis-site_labels_hypothesis)**2))
                
                site_preds_original = preds_original[concat_model_data['train_val_indices'][site]['From']:concat_model_data['train_val_indices'][site]['To']]
                site_labels_original = labs_original[concat_model_data['train_val_indices'][site]['From']:concat_model_data['train_val_indices'][site]['To']]
                rmse_original = np.sqrt(np.mean((site_preds_original-site_labels_original)**2))
                
                site_delta_rmse = (rmse_hypothesis-rmse_original)/rmse_original
                
                site_rmse_vals[site] = site_delta_rmse

        site_rmse_vals['all_sites'] = delta_rmse
        site_rmse_df = pd.DataFrame(site_rmse_vals.items(), columns = ['site','Feature_Importance'])
        site_rmse_df['Feature'] = feat_list[i+1]
        
        feat_imp_df = feat_imp_df.append(site_rmse_df)
        
    return(feat_imp_df)

#%%calculate feature importance
def calc_feature_importance_reps(model_run_id, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay, concat_model_data, feat_list, n_reps):
    feat_imp_reps = pd.DataFrame()
    for i in range(n_reps):
    # initialize the model
        print('Calculating feature importance rep '+str(i+1))
        model = lmf.LSTM_layer(num_features, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay)
        model = model.to(device)
        #load the model weights
        model.load_state_dict(torch.load(os.path.join("03_model/out/multi_site/"+model_run_id+"/Rep_0"+str(i)+"/model_weights.pt")))
    
        feat_imp_df = calc_permutation_feature_importance(model, concat_model_data, feat_list)
        feat_imp_df['rep'] = i
        
        feat_imp_reps = feat_imp_reps.append(feat_imp_df)
        
    return(feat_imp_reps)

#%%
def save_plot_feature_importance(model_run_id, feat_imp_reps, feat_list):
    
    print('Saving feature importance plot and data')
    feat_imp_mean = feat_imp_reps.groupby(['site','Feature'])['Feature_Importance'].mean().to_frame().reset_index()
    all_sites = feat_imp_mean[feat_imp_mean.site == 'all_sites']
    #feat_imp_min = np.min(feat_imp_reps, axis = 1)
    #feat_imp_max = np.max(feat_imp_reps, axis = 1)
    imp_error = feat_imp_reps.groupby(['site','Feature'])['Feature_Importance'].std().to_frame().reset_index()
    all_sites_error = imp_error[imp_error.site == 'all_sites']
    
    ordered_feat_imp = np.argsort(all_sites.Feature_Importance)
    #np.array(feat_list[1:])[ordered_feat_imp]
    #y_pos = np.arange(len(feat_list)-1)
    
    # plt.rcdefaults()
    # fig, ax = plt.subplots()
    # fig.set_size_inches(18.5, 10.5)
    
    # ax.barh(np.array(feat_list[1:])[ordered_feat_imp],all_sites[ordered_feat_imp], 
    #         xerr = all_sites_error[ordered_feat_imp], align = 'center')
    # #ax.invert_yaxis()
    # ax.set_xlabel('Delta mse')
    # ax.set_title('Feature importance')
    # plt.tight_layout()
    # plt.show()
    # fig.savefig(os.path.join('04_analysis/figs/Multi_Site_Feature_Importance'+model_run_id+'.png'))

    multi_site_feature_imp = pd.DataFrame({'site': feat_imp_mean.site, 'feat': feat_imp_mean.Feature, 'feat_imp_mean' : feat_imp_mean.Feature_Importance, 'feat_imp_std' : imp_error.Feature_Importance})
    
    
    multi_site_feature_imp.to_csv(os.path.join('04_analysis/out/multi_site_ensemble_feature_importance'+model_run_id+'.csv'))

    multi_site_feature_imp[multi_site_feature_imp['feat_imp_mean'] >= 0.25].sort_values(by = 'feat_imp_mean', ascending= False)
    
#%%
if __name__ == "__main__":
    with open(os.path.join('03_model/out/multi_site',model_run_id,'Rep_00/prepped_data'), 'rb') as input_data:
            concat_model_data = pickle.load(input_data)
                
    feat_imp_reps = calc_feature_importance_reps(model_run_id, hidden_size, seq_len, num_layers, dropout, learning_rate, weight_decay, concat_model_data, feat_list, n_reps)
    
    save_plot_feature_importance(model_run_id, feat_imp_reps, feat_list)
    
    