#Load libraries
library(dplyr)
library(nlatools) #nla tools is from http://github.com/lawinslow/nlatools
## to install:
#' devtools::install_github('lawinslow/nlatools')


tox   = readnlafile('extdata/nla2007/NLA2007_Recreational_ConditionEstimates_20091123.csv')
lulc  = readnlafile('extdata/nla2007/NLA2007_Basin_Landuse_Metrics_20061022.csv')
## Skip CHEM. Mostly overlaps with WQ data
#chem  = readnlafile('extdata/nla2007/NLA2007_Chemical_ConditionEstimates_20091123.csv')
wq    = readnlafile('extdata/nla2007/NLA2007_WaterQuality_20091123.csv')
secchi = readnlafile('extdata/nla2007/NLA2007_Secchi_20091008.csv')
prof = readnlafile('extdata/nla2007/NLA2007_Profile_20091008.csv')
site = readnlafile('extdata/nla2007/NLA2007_SampledLakeInformation_20091113.csv')


morpho = nlatools::get_morpho()


### Clean up columns before big merge  
tox_thin    = tox %>% select(SITE_ID, VISIT_NO, MCYST_TL_UGL)
lulc_thin   = lulc
secchi_thin = secchi %>% select(SITE_ID, VISIT_NO, SECMEAN)
#chem_thin   = chem %>% select(SITE_ID, VISIT_NO, PTL, NTL, TURB, ANC, DOC, COND, CHLA)
wq_thin     = wq %>% select(SITE_ID, VISIT_NO, COND, ANC, TURB, TOC, DOC, NO3_NO2, NTL, PTL, CL, SO4, NO3, CA, 
                            MG, NA., K, COLOR, SIO2, NH4ION, CHLA)
site_thin   = site %>% select(SITE_ID, VISIT_NO, DATE_COL, LAT_DD, LON_DD, LAKEAREA, LAKENAME, 
                              AREA_HA, DEPTHMAX, ELEV_PT, HUC_2, HUC_8)

prof_surf   = prof %>% select(SITE_ID, VISIT_NO, DEPTH, TEMP_FIELD, DO_FIELD, COND_FIELD, PH_FIELD) %>%
  group_by(SITE_ID, VISIT_NO) %>% slice(which.min(DEPTH)) %>%
  rename(DEPTH_SURF=DEPTH, TEMP_SURF=TEMP_FIELD, DO_SURF=DO_FIELD, COND_SURF=COND_FIELD, PH_SURF=PH_FIELD)

prof_deep   = prof %>% select(SITE_ID, VISIT_NO, DEPTH, TEMP_FIELD, DO_FIELD, COND_FIELD, PH_FIELD) %>%
  group_by(SITE_ID, VISIT_NO) %>% slice(which.max(DEPTH)) %>% 
  rename(DEPTH_BOT=DEPTH, TEMP_BOT=TEMP_FIELD, DO_BOT=DO_FIELD, COND_BOT=COND_FIELD, PH_BOT=PH_FIELD)


big_table = plyr::join_all(list(tox_thin, lulc_thin, secchi_thin, site_thin, wq_thin, prof_surf, prof_deep), by=c('SITE_ID', 'VISIT_NO')) 
big_table = filter(big_table, VISIT_NO==1)

### selectively cut out very redundant fields
big_table = select(big_table, -SITE_TYPE, -LAKE_SAMP, -BASINAREA_HA, 
                   -BASINAREA_LU_KM2, -MISSDATA_KM2_BSN, -LANDUSE_KM2_BSN)

big_table = select(big_table, -matches('NLCD.*_KM2_BSN')) 
big_table = select(big_table, -starts_with('FLAG_'), 
                   -starts_with('COMMENT_')) 

write.table(big_table, 'data/g19_2007_bigtable.csv', sep=',', row.names=FALSE)
