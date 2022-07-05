#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 30 14:30:39 2022

@author: galengorski
"""

#import netCDF4
from datetime import datetime
#import netCDF4
#from netCDF4 import Dataset,num2date,date2num
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
#%%
hydro_data = pd.read_csv('02_munge/out/all_sites_data_bfs.csv', 
                         usecols = ['site_no','Date','discharge','baseflow','quickflow','nitrate','discharge_interp'],
                         dtype = {'site_no':str,
                                  'discharge': float,
                                  'baseflow': float,
                                  'quickflow':float,
                                  'nitrate': float,
                                  'discharge_interp':str
                                  },
                         parse_dates = ['Date'],
                         index_col = 'Date')
#%%
single_site = hydro_data.site_no.unique()[0]
hydro_data_site = hydro_data[hydro_data.site_no == single_site].copy()
hydro_data_site['nitrate_rolling'] = hydro_data_site['nitrate'].rolling("7D", min_periods = 3, center=False).mean()

plt.plot(hydro_data_site['nitrate'], 'black')
plt.plot(hydro_data_site['nitrate_rolling'], 'red')