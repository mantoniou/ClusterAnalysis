


#' Important indicators
#' 
#' https://www.tutor2u.net/geography/reference/the-8-key-gap-indicators-of-development 
#' GDP, Gross National Product (GNP), GNP per capita, 
#' The Human Development Index (HDI) {Life expectancy index, Education index, 
#' Mean years of schooling index, Expected years of schooling index, Income index},
#' Infant mortality rate, Life expectancy
#' 
#' Stock Market, Manufacturing Activity, Inventory Levels, Retail Sales, Building Permits,
#' Housing Market, Level of New Business Startups, Unemployment Rate, Consumer Price Index (Inflation),
#' Interest Rates, Corporate Profits, Balance of Trade
#' 
#' Final - Clustering for EU countries based on consumer & business confidence 
#' indicators of the last 4-5 years 



library(tidyverse)
library(rvest)
library(eurostat)


# Eurostat Consumers data ######################################################
dat <- get_eurostat("ei_bsco_m", time_format = "date")
dat <- label_eurostat(dat)

cons <- 
  dat %>% 
  filter(s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
  select(geo, time, indic, values) %>% 
  spread(indic, values) %>% 
  rename(date = time)


# ggplot(dat) + geom_line(aes(time, values)) 




# Eurostat Business data #######################################################
dat <- get_eurostat("ei_bsin_m_r2", time_format = "date")
dat <- label_eurostat(dat)

bus <- 
  dat %>% 
  filter(s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
  select(geo, time, indic, values) %>% 
  spread(indic, values) %>% 
  rename(date = time)




final <- 
  left_join(cons, bus) %>% 
  filter(date >= '2014-01-01') %>%
  select(-date) %>% 
  group_by(geo) %>%
  summarise_all(median, na.rm = TRUE )


# DESCRIPTIVE ##################################################################

data.frame(geo = final$geo, sums = rowSums(final[-1], na.rm = TRUE)) %>% View()

# MAP PLOT #####################################################################

library(ggmap)
library(ggplot2)
library(dplyr)
library(PBSmapping) # to clip polygons
require(ggthemes) # for theme_map, if desired

# define data (a simple dataset is constructed here
# for illustration purposes) and background map
countryData<-data.frame(region=factor(c("France", "Germany", "Greece")), data=c(2, 15, 1))
nMap <- get_map("Vienna, Austria",zoom=4,maptype="toner",source="stamen")

#get country polygon data
mapdata <- map_data("world")
mapdata <- left_join(mapdata, countryData, by="region")

#get bounding box for map
bb<-attr(nMap, "bb");
ylim<-c(bb$ll.lat, bb$ur.lat)
xlim<-c(bb$ll.lon, bb$ur.lon)

#clip polygons to map
colnames(mapdata)[1:6] <- c("X","Y","PID","POS","region","subregion")
mapdata<-clipPolys(mapdata, xlim=xlim, ylim=ylim, keepExtra=TRUE)

#plot map overlay
ggmap(nMap)+coord_map(xlim=xlim,ylim=ylim) +
  geom_polygon(data=mapdata, aes(x=X, y=Y, group=PID, fill=data), alpha=0.5) +
  ggthemes::theme_map()
