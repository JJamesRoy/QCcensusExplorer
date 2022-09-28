library(cancensus)
library(sf)
library(geojsonsf)
library(leaflet)
library(RColorBrewer)
library(classInt)
library(tidyverse)
library(stringr)
library(janitor)


options(cancensus.api_key='CensusMapper_f622f3df6a9035ecbfee37004202c139')
options(cancensus.cache_path = "C:/Users/james/OneDrive/Documents/R cache")
dat_DA <- get_census(dataset='CA21', regions=list(CD=c("2466","2465")), vectors=c("v_CA21_1","v_CA21_4","v_CA21_6","v_CA21_386","v_CA21_434","v_CA21_452","v_CA21_497","v_CA21_498","v_CA21_593","v_CA21_596","v_CA21_671","v_CA21_719","v_CA21_767","v_CA21_905","v_CA21_1140","v_CA21_1144","v_CA21_906","v_CA21_907","v_CA21_923","v_CA21_944"), labels="detailed", geo_format="sf", level='DA')
# dat_CT <- get_census(dataset='CA21', regions=list(CD=c("2466","2465")), vectors=c("v_CA21_1","v_CA21_4","v_CA21_6","v_CA21_386","v_CA21_434","v_CA21_452","v_CA21_497","v_CA21_498","v_CA21_593","v_CA21_596","v_CA21_671","v_CA21_719","v_CA21_767","v_CA21_905","v_CA21_1140","v_CA21_1144","v_CA21_906","v_CA21_907","v_CA21_923","v_CA21_944"), labels="detailed", geo_format="sf", level='CT')


dat_DA = dat_DA %>% 
  rename_with(str_replace, pattern = "v_", replacement = "")

dat_DA = dat_DA %>% 
  rename_with(str_replace, pattern = ":.*", replacement = "")

dat_pc <- read.csv("~/R/QcCensusExplorer - Rstudio/pccf_fccp_V2209_2021.txt", header=FALSE, sep=";")
 # Read FCCP file to link postal code to DA

dat_pc$V2 = substr(dat_pc$V1, 1, 6)
dat_pc$V3 = substr(dat_pc$V1, 16, 22)
dat_pc$V3 = as.numeric(dat_pc$V3)

dat_pc = dat_pc %>% filter(grepl("^24",V3)) %>% mutate(CSD_UID = V3) %>%  mutate(PC = V2)

dat_pc$CSD_UID = as.character(dat_pc$CSD_UID)

dat_pc = subset(dat_pc, select = c(CSD_UID, PC))

inner_join(dat_DA, dat_pc, by = 'CSD_UID')
dat = full_join(dat_pc,dat_DA)
#test = merge(dat_DA, dat_pc, by = "CSD_UID")

breaks_qt <- classIntervals(dat_DA$CA21_906, n = 9, style = "quantile")
#Break the variable into quantiles

pal_fun <- colorQuantile("YlOrRd", NULL, n = 9)
#Create a function for the color palette
p_popup <- paste0("<strong>Revenu médian des ménages ($) : </strong>", dat_DA$CA21_906)



leaflet(dat_DA) %>%
  addPolygons(
    stroke = TRUE, color = "black", opacity = 0.2, weight = 1.5,
    fillColor = ~pal_fun(CA21_906),
    fillOpacity = 0.75, smoothFactor = 0.5,
    highlightOptions = highlightOptions(color = "white", weight = 2,
                                        bringToFront = TRUE),
    popup = p_popup) %>%
  addTiles() %>%
  addLegend("bottomright", 
            colors = brewer.pal(9, "YlOrRd"), 
            labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
            title =  'Revenu moyen des ménages en $')
