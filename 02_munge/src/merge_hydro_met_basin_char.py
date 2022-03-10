#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jan 29 11:13:26 2022

@author: galengorski
"""

#import netCDF4
from datetime import datetime
import netCDF4
from netCDF4 import Dataset,num2date,date2num
import numpy as np
import pandas as pd
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
sites = hydro_data.site_no.unique()

i = 1
site_info = pd.read_csv('01_fetch/out/site_list_220128.csv', dtype = {'site_no':str})

model_input_nc = netCDF4.Dataset('02_munge/out/model_input.nc',mode='w')
model_input_nc.title='Modeling input data'

for i, single_site in enumerate(sites):

    hydro_data_temp = hydro_data[hydro_data.site_no == single_site]
    met_data = pd.read_csv('01_fetch/out/met_data/'+single_site+'_met_data.csv',
                           parse_dates = ['date'], index_col = 'date')
    #make sure they have the same length
    len(hydro_data_temp) == len(met_data)
    #join met data with hydro data
    hydro_met = hydro_data_temp.join(met_data, how = 'outer')
    hydro_met['nitrate'] = hydro_met['nitrate'].fillna(-999)
    #
    #
    #read in basin char data
    basin_char = pd.read_csv('01_fetch/out/basin_char/'+single_site+'_basin_char.csv')
    #read in land cover
    land_cover = pd.read_csv('01_fetch/out/nlcd_data/land_cover_'+single_site+'.csv', 
                             header = 0, sep = ' ',
                             dtype = {'cat':str,
                                      'value':float})
    #clean up column names
    land_cover['characteristic_id'] = 'NLCD_'+land_cover['cat']
    land_cover['characteristic_value'] = land_cover['value']
    land_cover['percent_nodata'] = np.nan
    land_cover = land_cover[['characteristic_id','characteristic_value','percent_nodata']]
     #merge with basin char
    basin_char_lc = basin_char.append(land_cover)
    #
    #fill netcdf
    site = model_input_nc.createGroup(single_site)
    site.site_name = site_info.station_nm[i]
    site.lat_long = [str(site_info.dec_lat_va[i]), str(site_info.dec_long_va[i])]
    site.date_range = [datetime.strftime(hydro_met.index.min(), '%Y-%m-%d'), datetime.strftime(hydro_met.index.max(), '%Y-%m-%d')]
    today = datetime.today()
    site.history = "Created " + today.strftime("%Y-%m-%d")
    
    # dimensions.
    time = site.createDimension('time', None)
    basin_char = site.createDimension('basin_char',None)
    
    #create variables
    # latitude = site.createVariable('Latitude','f4','lat')
    # longitude = site.createVariable('Longitude','f4','lon')
    
    #static variables
    
    for j in range(len(basin_char_lc)):
        var_name = basin_char_lc.iloc[j,0]
        #create the variable
        temp_var = site.createVariable(var_name,'f8', 'basin_char')
        #fill the variable
        temp_var[:] =  basin_char_lc.iloc[j,1]
    
    #dynamic variables
    date = site.createVariable('Date','f8','time')
    discharge = site.createVariable('Discharge','f8','time')
    baseflow = site.createVariable('Baseflow','f8','time')
    quickflow = site.createVariable('Quickflow','f8','time')
    nitrate = site.createVariable('Nitrate','f8','time')
    precip = site.createVariable('Precip','f8','time')
    tmax = site.createVariable('TempMax','f8','time')
    tmin = site.createVariable('TempMin','f8','time')
    
    
    #fill in dynamic data
    date.units = 'hours since 0001-01-01 00:00:00.0'
    date.calendar = 'gregorian'
    date[:] = date2num(hydro_met.index.to_list(), units=date.units,calendar=date.calendar)
    date.units = 'hours since 0001-01-01 00:00:00.0'
    date.calendar = 'gregorian'
    date.range = [datetime.strftime(hydro_met.index.min(), '%Y-%m-%d'), datetime.strftime(hydro_met.index.max(), '%Y-%m-%d')]
    discharge[:] = hydro_met.discharge
    discharge.units = 'cfs'
    baseflow[:] = hydro_met.baseflow
    baseflow.units = 'cfs'
    quickflow[:] = hydro_met.quickflow
    quickflow.units = 'cfs'
    nitrate[:] = hydro_met.nitrate
    nitrate.units = 'mg/L [NO3-NO2]'
    precip[:] = hydro_met.prcp
    precip.units = 'mm'
    tmax[:] = hydro_met.tmax
    tmax.units = 'K'
    tmin[:] = hydro_met.tmin
    tmin.units = 'K'
    
    print(site_info.site_no[i]+' | '+single_site+' | '+site_info.station_nm[i])
    
model_input_nc.close()
    
#%%  
# model_input_nc = netCDF4.Dataset('02_munge/out/model_input_01.nc')
# model_input_nc.groups.keys()
# model_input_nc.close()
