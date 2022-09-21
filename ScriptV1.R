library(cancensus)
library(sf)
library(geojsonsf)
options(cancensus.api_key='CensusMapper_f622f3df6a9035ecbfee37004202c139')
options(cancensus.cache_path = "C:/Users/james/OneDrive/Documents/R cache")
census_data <- get_census(dataset='CA21', regions=list(CMA="24462"), vectors=c("v_CA21_1","v_CA21_4","v_CA21_6","v_CA21_386","v_CA21_434","v_CA21_452","v_CA21_497","v_CA21_498","v_CA21_593","v_CA21_596","v_CA21_671","v_CA21_719","v_CA21_767","v_CA21_905","v_CA21_1139","v_CA21_1144"), labels="detailed", geo_format="sf", level='DA')


