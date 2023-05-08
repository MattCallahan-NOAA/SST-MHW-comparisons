library(tidyverse)
library(httr)
library(lubridate)

#plot 

sst<-httr::content(
  httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?start_date=19850102&end_date=20230506'),
  type = "application/json") %>%
  bind_rows()

#Let's look at a correlation between CAI and WGOA SST with a lag.

sst<-sst %>%
  rename_with(tolower) 

aigoa <- sst %>% filter(ecosystem_sub %in% c("Western Aleutians", "Western Gulf of Alaska") & year<2023) %>%
  mutate(eco=recode_factor(ecosystem_sub, "Western Aleutians"="wai", "Western Gulf of Alaska"="wgoa")) %>%
  group_by(year, eco) %>%
  summarize(sst=mean(meansst)) %>%
  pivot_wider(names_from=eco, values_from = sst) 

ggplot()+geom_text(data=aigoa, aes(x=wgoa, y=wai, label=year))

cor(aigoa$wgoa, aigoa$wai)

#Now lagged one year

shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

aigoa$wai_lag1<-shift(aigoa$wai,1)

ggplot()+geom_text(data=aigoa, aes(x=wgoa, y=wai_lag1, label=year))

cor((aigoa %>%filter(year<2022))$wgoa, (aigoa %>%filter(year<2022))$wai_lag1 )

#worse correlation
#try two years
aigoa$wai_lag2<-shift(aigoa$wai,2)

ggplot()+geom_text(data=aigoa, aes(x=wgoa, y=wai_lag2, label=year))

cor((aigoa %>%filter(year<2021))$wgoa, (aigoa %>%filter(year<2021))$wai_lag2 )

#back up to 0.34

#try 3 years
aigoa$wai_lag3<-shift(aigoa$wai,3)

ggplot()+geom_text(data=aigoa, aes(x=wgoa, y=wai_lag3, label=year))

cor((aigoa %>%filter(year<2020))$wgoa, (aigoa %>%filter(year<2020))$wai_lag3 )
 #even higher

#4 years?
aigoa$wai_lag4<-shift(aigoa$wai,4)

ggplot()+geom_text(data=aigoa, aes(x=wgoa, y=wai_lag4, label=year))

cor((aigoa %>%filter(year<2019))$wgoa, (aigoa %>%filter(year<2019))$wai_lag4 )

#OK, that breaks down.

