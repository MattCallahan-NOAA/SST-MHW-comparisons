#GOA bottom temp
#

library(tidyverse)
library(sf)
library(akmarineareas2)
library(odbc)
library(getPass)
library(lubridate)

#connect
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass())

#query LLS data
#this is fine scale data, mostly on the continental slope
lls_temp<- dbFetch(dbSendQuery(con, "select year,  day_of_year, station_number, latitude, longitude, temperature, tdr_depth
from afsc.lls_intrp_temp
where esr_region in ('Eastern Gulf of Alaska', 'Western Gulf of Alaska') "))

#get date
lls_temp<-lls_temp %>%
  rename_with(tolower) %>%
  mutate(date=as.Date(day_of_year, origin= paste0(year, "-01-01")))

#plot

xmin<-min(lls_temp$longitude) 
xmax<-max(lls_temp$longitude)
ymin<-min(lls_temp$latitude)
ymax<-max(lls_temp$latitude)
ggplot()+
  geom_sf(data=ak%>%st_transform(crs=4326))+
  geom_point(data=lls_temp, aes(x=longitude, y=latitude))+
  xlim(c(xmin, xmax))+ylim(c(ymin, ymax))+
  theme_bw()

write.csv(lls_temp, "survey_temp/lls_temp.csv")

 # GAP (bottom trawl) survey
gap_temp<- dbFetch(dbSendQuery(con, "select start_time, haul, stationid, stratum, start_latitude, start_longitude, end_latitude, end_longitude, gear_depth, bottom_depth, surface_temperature, gear_temperature
from afsc.race_haulaigoa
where region = 'GOA'")) %>%
  rename_with(tolower)

xmin<-min(gap_temp$start_longitude) 
xmax<-max(gap_temp$start_longitude)
ymin<-min(gap_temp$start_latitude)
ymax<-max(gap_temp$start_latitude)
ggplot()+
  geom_sf(data=ak%>%st_transform(crs=4326))+
  geom_point(data=gap_temp, aes(x=start_longitude, y=start_latitude))+
  xlim(c(xmin, xmax))+ylim(c(ymin, ymax))+
  theme_bw()

write.csv(gap_temp, "survey_temp/gap_temp.csv")
