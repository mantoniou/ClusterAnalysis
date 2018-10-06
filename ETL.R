

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
#' @info https://ec.europa.eu/eurostat/cache/metadata/en/ei_bcs_esms.htm
#' 



library(tidyverse)
library(rvest)
library(eurostat)


# # Eurostat Consumers data ######################################################
# dat <- get_eurostat("ei_bsco_m", time_format = "date")
# dat <- label_eurostat(dat)
# 
# cons <- 
#   dat %>% 
#   filter(s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
#   select(geo, time, indic, values) %>% 
#   spread(indic, values) %>% 
#   rename(date = time) %>% 
#   mutate(total_cons = Reduce("+",.[3:15]))
# 
# 
# 
# # Eurostat Business-Industry data ##############################################
# dat <- get_eurostat("ei_bsin_m_r2", time_format = "date")
# dat <- label_eurostat(dat)
# 
# bus <- 
#   dat %>% 
#   filter(s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
#   select(geo, time, indic, values) %>% 
#   spread(indic, values) %>% 
#   rename(date = time) %>% 
#   mutate(total_bus = Reduce("+",.[3:10]))

# Eurostat Business-Sentiment data ##############################################
dat <- get_eurostat("ei_bssi_m_r2", time_format = "date")
dat <- label_eurostat(dat)

sent <- 
  dat %>% 
  filter(s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
  select(geo, time, indic, values) %>% 
  spread(indic, values) %>% 
  rename(date = time) %>% 
  mutate(business = Reduce("+",.[3,5:7]))

# # Eurostat Business-Construction data ##############################################
# dat <- get_eurostat("ei_bsbu_m_r2", time_format = "date")
# dat <- label_eurostat(dat)
# 
# constr <- 
#   dat %>% 
#   filter(s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
#   select(geo, time, indic, values) %>% 
#   spread(indic, values) %>% 
#   rename(date = time) %>% 
#   mutate(total_constr = Reduce("+",.[3:14]))
# 
# 
# # Eurostat Business-Retail sale data ##############################################
# dat <- get_eurostat("ei_bsrt_m_r2", time_format = "date")
# dat <- label_eurostat(dat)
# 
# retail <- 
#   dat %>% 
#   filter(s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
#   select(geo, time, indic, values) %>% 
#   spread(indic, values) %>% 
#   rename(date = time) %>% 
#   mutate(total_retail = Reduce("+",.[3:8]))
# 
# 
# 
# # Eurostat Business-Services data ##############################################
# dat <- get_eurostat("ei_bsse_m_r2", time_format = "date")
# dat <- label_eurostat(dat)
# 
# serv <- 
#   dat %>% 
#   filter(s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
#   select(geo, time, indic, values) %>% 
#   spread(indic, values) %>% 
#   rename(date = time) %>% 
#   mutate(total_serv = Reduce("+",.[3:9]))
# 
# 
# # Eurostat |???????????????????????????????????????????? ########################
# dat <- get_eurostat("tsc00011", time_format = "date")
# dat <- label_eurostat(dat)
# 
# serv <- 
#   dat %>% 
#   filter(s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)") %>% 
#   select(geo, time, indic, values) %>% 
#   spread(indic, values) %>% 
#   rename(date = time) %>% 
#   mutate(total_serv = Reduce("+",.[3:9]))
# 


# Create final dataset #########################################################

# Only Consumer & business indicators
final <-
  sent %>%
  filter(date >= '2014-01-01') %>%
  select(-date) %>%
  group_by(geo) %>%
  summarise_all(median, na.rm = TRUE) %>% 
  mutate(Business = `Construction confidence indicator` + `Industrial confidence indicator` +
           `Retail confidence indicator` + `Services Confidence Indicator`,
         Consumers = `Consumer confidence indicator`)

ggplot(final) + geom_label(aes(Consumers, Business, label = geo))




# # Only Consumer & business indicators
# final <-
#   left_join(cons, bus) %>%
#   filter(date >= '2014-01-01') %>%
#   select(-date) %>%
#   group_by(geo) %>%
#   summarise_all(median, na.rm = TRUE )
# 
# ggplot(final) + geom_label(aes(total_cons, total_bus, label = geo))
# 
# # Only Consumer & sentiment indicators
# final <-
#   left_join(cons, sent) %>%
#   filter(date >= '2014-01-01') %>%
#   select(-date) %>%
#   group_by(geo) %>%
#   summarise_all(median, na.rm = TRUE )
# 
# ggplot(final) + geom_label(aes(total_cons, total_bus, label = geo))
# 

# # Only Consumer & Services indicators
# final <- 
#   left_join(cons, serv) %>% 
#   filter(date >= '2014-01-01') %>%
#   select(-date) %>% 
#   group_by(geo) %>%
#   summarise_all(median, na.rm = TRUE ) 
# 
# ggplot(final) + geom_label(aes(total_cons, total_serv, label = geo))

# # Only Consumer & Construction indicators
# final <- 
#   left_join(cons, constr) %>% 
#   filter(date >= '2014-01-01') %>%
#   select(-date) %>% 
#   group_by(geo) %>%
#   summarise_all(median, na.rm = TRUE ) 
# 
# ggplot(final) + geom_label(aes(total_cons, total_constr, label = geo))

# # Only Consumer & Retail indicators
# final <- 
#   left_join(cons, retail) %>% 
#   filter(date >= '2014-01-01') %>%
#   select(-date) %>% 
#   group_by(geo) %>%
#   summarise_all(median, na.rm = TRUE ) 
# 
# ggplot(final) + geom_label(aes(total_cons, total_retail, label = geo))


# 
# # All data ...
# final_all <- 
#   left_join(cons, bus, by = c("geo", "date")) %>%
#       left_join(., cons, by = c("geo", "date")) %>% 
#           left_join(., retail, by = c("geo", "date")) %>% 
#               left_join(., serv, by = c("geo", "date")) %>% 
#   filter(date >= '2014-01-01') %>%
#   select(-date) %>% 
#   group_by(geo) %>%
#   summarise_all(median, na.rm = TRUE )
# 





# DESCRIPTIVE ##################################################################

data.frame(geo = final$geo, sums = rowSums(final[-1], na.rm = TRUE)) %>% View()

# MAP PLOT #####################################################################

## test 1
jam <- final %>% 
  ungroup() %>%
  mutate(Country = geo) %>% 
  select(Country, Business)

download.file("https://github.com/mtennekes/tmap/blob/master/data/World.rda",
              destfile = "World.rda")

download.file(url = "https://github.com/mtennekes/tmap/blob/master/data/World.rda", destfile = "World.rda")

load("World.rda")

load("C:/Users/eam/Downloads/World.rda")


Europe <- filter(World, continent == "Europe")

Europe.jam <- append_data(Europe, as.data.frame(jam),key.shp = 'name',key.data = "Country")
# Europe.jam$income_grp <- as.character(Europe.jam$income_grp)
# 
# write_shape(Europe.jam,"europe_jam.shp")
# 
# zip("europe_jam_shp.zip",dir(".","europe_jam.*"))

filter(Europe.jam, is.na(Business) == FALSE & name!= "Denmark") %>% View()
tm_shape() +
  tm_polygons(col='Business',title = "Jam consumption (mg/day)") +
  tm_text("iso_a3", size="AREA", root=5)  +
  tm_style("grey") +
  tm_format_Europe2() +
  tm_legend(legend.show	= FALSE)



## test 2

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