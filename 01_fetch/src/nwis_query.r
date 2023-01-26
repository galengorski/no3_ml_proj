#===================================================================================#
# NOTES: This script is for generating a list of sites and pulling nitrate and 
# discharge data from those sites. There is gap filling and interpolation performed
# on some of the sites
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# ggorski@usgs.gov                                                                  #
# 2022-03-05                                                                        #  
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####
library(tidyverse)
library(rnoaa) # NOAA GHCND meteorological data queries
library(dataRetrieval) # USGS NWIS hydrologic data queries
library(baytrends) # interpolation
library(padr) # filling in timestamp gaps
#####
#===================================================================================#



end_date <- "2021-12-31" # most recent date we want to analyze. Probably best to leave out at least the past few days, maybe more.

# NWIS Site Selection-----------------------------------------------------------

US_states <- c(#"AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
  #"HI", "ID", 
  "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
  #"MA", 
  "MI", "MN", "MS", "MO", "MT", 
  "NE", 
  #"NV", "NH", 
  "NJ",
  #"NM", "NY", "NC", 
  "ND", "OH", "OK", "OR", "PA", 
  #"RI", "SC",
  "SD", 
  #"TN", "TX", "UT", "VT", "VA", "WA", "WV", 
  "WI"#, 
  #"WY"
)

nitrate_sites <- list()

for (i in 1:length(US_states)) {
  try(nitrate_sites[[i]] <- whatNWISdata(stateCd = US_states[i], 
                                         service = "dv", 
                                         statCd = "00003", 
                                         parameterCd = "99133"))
  
  print(paste(US_states[i],':',nrow(nitrate_sites[[i]])))
}

nitrate_sites <- nitrate_sites[lengths(nitrate_sites) > 0]

nitrate_sites <- nitrate_sites[lapply(nitrate_sites, nrow) > 0]

nitrate_sites <- lapply(nitrate_sites, function(x) select(x, -alt_acy_va)) %>%
  bind_rows()

# nitr_delin <- read_csv("download/delineated_sites.csv") %>% # file contains ~19,000 delineated watersheds
#   right_join(nitrate_sites, by = c("SITE_NO" = "site_no")) %>%
#   filter(!is.na(SQMI))

nitr_delin <- nitrate_sites

data_avail <- whatNWISdata(siteNumbers = nitr_delin$site_no, 
                           statCd = c("00003"),#,"00008"), # 00003 is mean, 00008 is median (median needed for pH) 
                           service = "dv") 

parm_key <- readNWISpCode(unique(data_avail$parm_cd)) %>%
  arrange(parameter_cd)

candidates_lengths <- left_join(data_avail, parm_key, by = c("parm_cd" = "parameter_cd")) %>%
  group_by(station_nm, parm_cd, site_no) %>%
  summarize(count_nu = sum(count_nu)) %>%
  pivot_wider(names_from = parm_cd, values_from = count_nu) %>%
  filter(`00060` > 1000, #dishcharge
         `99133` > 300, #nitrate
         #`00010` > 500, #water temp
         #`00095` > 500, #specific conductivity
         #`00300` > 500, #DO
         #`00400` > 500, #ph
         #`63680` > 500, #turbidity
         #as.numeric(site_no) > 3e6,
         site_no != "05599490") %>%
  arrange(site_no) %>%
  select(station_nm, site_no, 
         `00060`, 
         #`00010`, 
         `99133`, 
         everything())

# NWIS Data Query--------------------------------------------------------------

candidates_meta <- whatNWISsites(siteNumbers = candidates_lengths$site_no)

write_csv(candidates_meta, "data_out/site_list_220127.csv")

nwis_data <- readNWISdv(siteNumbers = candidates_meta$site_no, 
                        parameterCd = c("00060",
                                        "99133"),#,
                                        #"00010",
                                        #"00095",
                                        #"00300",
                                        #"00400",
                                        #"63680"), 
                        statCd = c("00003", "00008"), 
                        startDate = "2010-01-01",
                        endDate = end_date)

nwis_tidy <- nwis_data %>%
  select(-contains("cd")) %>%
  transmute(site_no, Date, 
            #water_temp = rowMeans(select(., contains("00010")), na.rm = TRUE), # Some variables have different instruments, some of which have character "NA" data
            discharge  = rowMeans(select(., contains("00060")), na.rm = TRUE), # for when they're not reporting measurements. This seemed a straightforward remedy.
            #spec_cond  = rowMeans(select(., contains("00095")), na.rm = TRUE),
            #dissolv_O  = rowMeans(select(., contains("00300")), na.rm = TRUE),
            #pH         = rowMeans(select(., contains("00400")), na.rm = TRUE),
            #turbidity  = rowMeans(select(., contains("63680")), na.rm = TRUE),
            nitrate    = rowMeans(select(., contains("99133")), na.rm = TRUE)) %>%
  group_by(site_no, Date) %>%
  summarise(across(.fns = ~ mean(.x, na.rm = TRUE)))

# NWIS Gap Filling--------------------------------------------------------------

### Check and fix continuity of Date variable

#411955088280601 is at a culvert off of a gravel pit, likely not relevant to the other sites
#03338780 stopped collecting discharge data in 2010
#07374000 and 05420500 are two sites on the Mississippi; Baton Rouge and Clinton IA, while
#06934500 they all have massive drainage areas, at least an order of magntude higher than the rest
#that made them seem like outliers so I got rid of those sites too
nwis_tidy <- nwis_tidy %>%
  filter(site_no != '411955088280601' & site_no != '03338780' & site_no != '07374000' & site_no != '06934500'& site_no != '05420500')

#05576195 the record ends on 2018-11-13 but there are some
#trailing rows with NA values so those need to be excised
nwis_tidy <- nwis_tidy %>%
  filter(!(site_no == '05576195'&Date > as.Date('2018-11-13')))



#view gaps before dates are filled in, here we remove NA values in discharge first
#this removes any data gaps where a timestamp was reported but the value was NA for discharge
nwis_tidy %>%
  drop_na(discharge) %>%
  mutate(lag_data = Date-lag(Date), test = (Date - lag(Date) == 1)) %>%
  filter(!test) %>%
  View()

#fill gaps in time stamps, this will leave a single NA value for each site
#at the beginning of the record where there is no previous time stamp
nwis_date_filled <- pad(nwis_tidy) %>%
  group_by(site_no) %>%
  mutate(lag_data = Date-lag(Date), test = (Date - lag(Date) == 1)) 


#filter our sites with more than 1% of discharge values as negative 
#and > 1% of discharge values missing
keep_sites <- nwis_date_filled %>%
  mutate(q_neg = sum(discharge < 0, na.rm = T), q_na = sum(is.na(discharge))) %>%
  summarise(q_neg = first(q_neg), q_na = first(q_na), q = n(), frac_neg = q_neg/q, frac_na = q_na/q) %>%
  filter(frac_neg <= 0.01&frac_na <= 0.0101) %>%
  select(site_no)

#select only those sites
nwis_date_filled <- nwis_date_filled %>% 
  filter(site_no %in% keep_sites$site_no)


#This should produce a data frame with each site and the proportion of negative values and missing values
#all sites should have < 1% for each
nwis_date_filled %>%
  mutate(q_neg = sum(discharge < 0, na.rm = T), q_na = sum(is.na(discharge))) %>%
  summarise(q_neg = first(q_neg), q_na = first(q_na), q = n(), frac_neg = q_neg/q, frac_na = q_na/q) %>%
  View()


#fill gaps less than 10 days with linear interpolation
nwis_lin_filled <- nwis_date_filled %>%
  mutate(discharge_interp = if_else(is.na(discharge),
                                    true = "linear",
                                    false = "raw")) %>%
  # linear interpolation: https://www.rdocumentation.org/packages/baytrends/versions/2.0.5/topics/fillMissing
  mutate(discharge = fillMissing(discharge, span = 1, max.fill = 10))

#remove anything beyond the last non-na discharge value, basically we don't want any sites
#that don't have actual data bracketing some interpolated data
nwis_lin_filled <- nwis_lin_filled %>%
  slice(seq_len(max(which(!is.na(discharge)))))

gaps <- nwis_lin_filled %>% filter(is.na(discharge))

if(nrow(gaps) == 0){
  print('All Gaps Filled')
}else{
  print('Some gaps remain')
}


#this checks to make sure that the number of days in the date range and the number of observations matach
#it should produce an empty dataframe if everything goes right
nwis_lin_filled %>%
  summarise(date_range = as.numeric(last(Date) - first(Date))+1, n_obs_no_na = sum(!is.na(discharge)), n_obs = n(), full = date_range == n_obs_no_na) %>%
  filter(!full)

sites <- unique(nwis_lin_filled$site_no)
sites_info <- readNWISsite(siteNumbers = sites)
#write_csv(nwis_lin_filled, 'out/hydro_filled_220128.csv')
#write_csv(sites_info, 'out/site_list_220128.csv')
