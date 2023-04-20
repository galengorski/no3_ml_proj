#===================================================================================#
# NOTES: This is a recipe file for running the 01_fetch step of the model pipeline
#
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# galengorski@berkeley.edu                                                          #
# 2023-04-20                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
# install.packages('tidyverse')
library(tidyverse)
#####
#===================================================================================#

#Assume working directory is the project directory, the scripts should not be run as is
#this is just an archiving step

# This script is for generating a list of sites and pulling nitrate and 
# discharge data from those sites from NWIS, generates a list of sites and writes it to
# 01_fetch/out
source('01_fetch/src/nwis_query.r')


# fetching the datasets from Wieczorek data release
# Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, 
# Select Attributes for NHDPlus Version 2.1 Reach Catchments and Modified Network 
# Routed Upstream Watersheds for the Conterminous United States 
# (ver. 3.0, January 2021): U.S. Geological Survey data release, 
# https://doi.org/10.5066/F7765D7V.
source('01_fetch/src/fetch_Wieczorek_sb.R')

#download NLCD land cover data using the FedData package
source('01_fetch/src/download_nlcd.R')

# incorporating gw data from Zell and Sanford 
# https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019WR026724 
# data: https://water.usgs.gov/GIS/metadata/usgswrd/XML/zell2020_wrr.xml #
# https://water.usgs.gov/GIS/dsdl/gwmodels/zell2020_wrr/readme.txt
source('01_fetch/src/incorp_gw_data.R')

# script to incorporate tile drainage data from: 
# Valayamkunnath, P., Barlage, M., Chen, F. et al. 
# Mapping of 30-meter resolution tile-drained croplands using a geospatial 
# modeling approach. Sci Data 7, 257 (2020). 
# https://doi.org/10.1038/s41597-020-00596-x  
source('01_fetch/src/incorp_tile_drains.R')

#he script takes the list of sites generated in nwis_query.R and downloads
# the contributing basin from nhdplusTools, static characteristics for the basin, 
# and climate data from gridmet.
source('01_fetch/src/delineate_watersheds_basin_char.R')
