#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 10 20:20:02 2022

@author: galengorski
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs
from sklearn.cluster import KMeans, AgglomerativeClustering
from sklearn.metrics import silhouette_score
from sklearn.preprocessing import StandardScaler

#%%
basin_char = pd.read_csv('02_munge/out/basin_char_full.csv', dtype = {'Unnamed: 0':str})
basin_char = basin_char.rename(columns = {'Unnamed: 0':'site_no'})
basin_char['NLCD_DEV'] = basin_char['NLCD_21']+basin_char['NLCD_22']+basin_char['NLCD_23']+basin_char['NLCD_24']
basin_char['NLCD_FOR'] = basin_char['NLCD_41']+basin_char['NLCD_42']+basin_char['NLCD_43']
basin_char['NLCD_AG'] = basin_char['NLCD_81']+basin_char['NLCD_82']
basin_char['NLCD_WTLND'] = basin_char['NLCD_90']+basin_char['NLCD_95']
basin_char['fert_uN_mt_kmAg'] = basin_char['fert_uN_mt_sqkm']*(basin_char['NLCD_82']*basin_char['TOT_BASIN_AREA'])

basin_char = basin_char[['site_no','TOT_BASIN_AREA','TOT_BASIN_SLOPE', 'lat','long',
                    'TOT_STREAM_SLOPE','TOT_STREAM_LENGTH','TOT_STRM_DENS',
                    'TOT_CANALDITCH','TOT_ARTIFICIAL',
                    'TOT_HGA','TOT_HGAC','TOT_HGAD','TOT_HGB','TOT_HGBC','TOT_HGBD',
                    'TOT_HGC','TOT_HGCD','TOT_HGD',
                    'TOT_N97',''
                    'TOT_BFI', 'TOT_CONTACT', 'TOT_RECHG', 'TOT_WB5100_ANN', 'TOT_CWD', 'TOT_ET','TOT_RH',
                    'TOT_TAV7100_ANN', 'TOT_WDANN','TOT_PPT7100_ANN',
                    'TOT_DITCHES92','TOT_TILES92','TOT_NPDES_MAJ','TOT_NPDES_MAJ_DENS','TOT_NORM_STORAGE2013',
                    'TOT_LAKEPOND','TOT_RESERVOIR','TOT_SRL55AG',
                    'DTW','TRANSM','UNSAT_TT','WCON','NO3_DOM','NO3_PUB',
                    'fert_uN_mt_sqkm',
                    'NLCD_DEV','NLCD_FOR','NLCD_AG','NLCD_WTLND']]

#%%
hydro_data = pd.read_csv('02_munge/out/all_sites_data_bfs.csv', dtype = {'site_no':str})
#%%
problem = hydro_data[hydro_data.site_no == '05451210']

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

basin_char_hydro = basin_char.join(hydro_summary, on = 'site_no')

basin_char_hydro_cluster = basin_char_hydro.copy()

basin_char_hydro_cluster = basin_char_hydro_cluster.drop(columns = ['site_no','mean_no3','sd_no3'])
#%%
n_clusters = 6
scaler = StandardScaler()
scaled_features = scaler.fit_transform(basin_char_hydro_cluster)

kmeans = KMeans(
    init="random",
    n_clusters=n_clusters,
    n_init=10,
    max_iter=300,
    random_state=42
)

kmeans.fit(scaled_features)

kmeans.labels_
print(np.unique(kmeans.labels_, return_counts=True))

site_info = pd.read_csv('01_fetch/out/site_list_220507.csv', dtype = {'site_no':str})

aggo = AgglomerativeClustering(n_clusters=n_clusters, affinity='euclidean', linkage='ward')

basin_char_hydro['cluster'] = kmeans.labels_
basin_char_hydro['aggo_cluster'] = aggo.fit_predict(scaled_features)
basin_char_hydro['lat'] = site_info['dec_lat_va']
basin_char_hydro['long'] = site_info['dec_long_va']
basin_char_hydro['station_nm'] = site_info['station_nm']


basin_char_hydro.to_csv('04_analysis/out/basin_char_w_clusters_'+str(n_clusters)+'.csv')


#%%
kmeans_kwargs = {
    "init": "random",
    "n_init": 10,
    "max_iter": 300,
    "random_state": 42,
}

# A list holds the SSE values for each k
sse = []
for k in range(1, 21):
    kmeans = KMeans(n_clusters=k, **kmeans_kwargs)
    kmeans.fit(scaled_features)
    sse.append(kmeans.inertia_)

#%%
plt.plot(range(1, 21), sse)
plt.xticks(range(1, 21))
plt.xlabel("Number of Clusters")
plt.ylabel("SSE")
plt.show()


#%%
# A list holds the silhouette coefficients for each k
silhouette_coefficients = []

# Notice you start at 2 clusters for silhouette coefficient
for k in range(2, 21):
    kmeans = KMeans(n_clusters=k, **kmeans_kwargs)
    kmeans.fit(scaled_features)
    score = silhouette_score(scaled_features, kmeans.labels_)
    silhouette_coefficients.append(score)

plt.plot(range(2, 21), silhouette_coefficients)
plt.xticks(range(2, 21))
plt.xlabel("Number of Clusters")
plt.ylabel("Silhouette Coefficient")
plt.show()