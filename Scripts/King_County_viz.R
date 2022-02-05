library(tmap)
library(readr)
library(tidyverse)
library(sf)
KingsCountyHomeSales_1_ <- read_csv("Data/KingsCountyHomeSales (1).csv")
DataDictionary <- read_csv("Data/DataDictionary.csv")

tdir=tempdir()
ZCTAurl="https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_zcta510_500k.zip"
stateurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip"
countyurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip"
tracturl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_53_tract_500k.zip"

if(file.exists(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))==F){
  download.file(countyurl,destfile = file.path(tdir,"Counties.zip"))
  unzip(file.path(tdir,"Counties.zip"),exdir=tdir)}
USA_Counties=read_sf(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))
Washington_Counties = USA_Counties %>%
  filter(STATEFP==53)

if(file.exists(paste(tdir,"/cb_2018_53_tract_500k.shp",sep=""))==F){
  download.file(tracturl,destfile = file.path(tdir,"Tracts.zip"))
  unzip(file.path(tdir,"Tracts.zip"),exdir=tdir)}
Washington_Census_tracts = read_sf(paste(tdir,"/cb_2018_53_tract_500k.shp",sep = ""))

if(file.exists(paste(tdir,"/cb_2018_us_zcta510_500k.shp",sep=""))==F){
  download.file(ZCTAurl,destfile = file.path(tdir,"ZCTAs.zip"))
  unzip(file.path(tdir,"ZCTAs.zip"),exdir=tdir)}
USA_ZCTAs=read_sf(paste(tdir,"/cb_2018_us_zcta510_500k.shp",sep="")) %>%
  mutate(STATEFP=as.numeric(substring(.$ZCTA5CE10,1,2)))
Washington_ZCTAs = USA_ZCTAs %>%
  filter(STATEFP==98)

if(file.exists(paste(tdir,"/cb_2018_us_state_500k.shp",sep=""))==F){
  download.file(stateurl, destfile = file.path(tdir, "States.zip"))
  unzip(file.path(tdir,"States.zip"),exdir=tdir)}
USA_States = read_sf(paste(tdir,"/cb_2018_us_state_500k.shp",sep=""))
Washington_State = USA_States %>%
  filter(STATEFP==53)

sf_salesdat = KingsCountyHomeSales_1_ %>%
  st_as_sf(.,coords=c('longitude','latitude'))

tm_shape(Washington_Census_tracts,bbox = st_bbox(sf_salesdat))+
  tm_borders(col = 'black')+
tm_shape(sf_salesdat)+
  tm_dots(alpha=.3)
