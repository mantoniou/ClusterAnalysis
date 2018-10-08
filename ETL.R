

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
library(ggmap)
library(PBSmapping) # to clip polygons
require(ggthemes) # for theme_map, if desired
library(tmap)
library(tmaptools)
library(tidyverse)

## test 1
jam <- final %>% 
  ungroup() %>%
  mutate(Country = geo) %>% 
  select(Country, Business)

# download.file("https://github.com/mtennekes/tmap/blob/master/data/World.rda",
#                         destfile = "World.rda")
# 
# download.file(url = "https://github.com/mtennekes/tmap/blob/master/data/World.rda", destfile = "World.rda")
# 
# load("World.rda")

load("C:/Users/eam/Downloads/World.rda")


Europe <- filter(World, continent == "Europe")

Europe.jam <- append_data(Europe, as.data.frame(jam),key.shp = 'name',key.data = "Country")

Europe.jam$geometry[Europe.jam$name == "France"] <- st_geometry(list(list(c(285334.6850579, 341986.225768694, 382721.384297743, 453958.037893798, 472145.703840478, 495172.5579432, 533929.141432105, 650325.719090665, 612622.47168615, 605300.704981072, 583710.937415699, 546395.866025453, 549934.956651473, 492339.798423369, 492583.510216101, 531097.575535854, 560747.65707958, 558375.629753355, 583918.976274914, 556453.546067028, 580455.052246576, 625836.613380326, 617970.795619435, 544502.934339, 379415.857036664, 258639.029808574, 249972.988123855, 153044.718899834, 58623.4499668167, 28282.1190928927, -125392.930812737, -158286.002738838, -114821.070108164, -97802.0050227051, -181100.535927486, -240302.120244087, -363292.291637685, -369595.139633858, -264849.183616663, -130133.523555453, -154420.045439468, -79265.2933174695, 106652.895060436, 129795.233487865, 198757.66170499, 210756.579824503, 247637.631287541, 285334.6850579, 6109243.7403906, 6062235.96818479, 6070021.10022385, 6024330.10245433, 6015597.24518931, 6017724.21229018, 5991331.53787519, 5972712.79424233, 5903094.99323216, 5830015.27141864, 5812396.4702713, 5821894.08886383, 5795646.78399352, 5737310.69901187, 5690017.7261033, 5706411.75296043, 5660451.77460475, 5630712.27682037, 5591043.75029096, 5558740.94138623, 5476183.8666781, 5462580.5237927, 5415896.92412507, 5354810.91276629, 5384132.39505517, 5348986.5384381, 5283440.54677389, 5269277.71514544, 5318618.10776224, 5295065.65974725, 5344516.54590306, 5386635.63587957, 5451276.3487441, 5662949.1858247, 5772508.0198527, 5824835.5705293, 5864395.80481799, 5938871.44597482, 5960953.67995024, 5934830.68407436, 6049106.10228408, 6006000.25344206, 6084175.34190537, 6165428.6167606, 6185305.48622748, 6150648.28035136, 6149019.41467407, 6109243.7403906))))


tmap_style("classic")
filter(Europe.jam, is.na(Business) == FALSE & name!= "gdfgdg") %>% 
  tm_shape() +
  tm_polygons(col='Business',title = "Jam consumption (mg/day)") +
  tm_text("iso_a3", size="AREA", root=5) 


# test 2 #####

geo_file <- 
  get_eurostat_geospatial() %>% 
  filter(LEVL_CODE == 0)

geo_file$geo <- c("Austria", "Bulgaria", "Cyprus", "Czech Republic", "Switzerland", "Belgium", "Germany (until 1990 former territory of the FRG)",
                  "Denmark", "Estonia", "Greece", "Spain", "Croatia", "Hungary", "Finland", "France", "Ireland", "Liechtenstein", "Lithuania",                                     
                  "Luxembourg","Latvia", "Montenegro", "Former Yugoslav Republic of Macedonia, the", "Malta", "Netherlands",
                  "Island", "Italy", "Portugal", "Romania",  "Norway", "Poland", "Sweden", "Slovakia", "Turkey", "United Kingdom", "Slovenia" )


final2 <- 
  append_data(geo_file, final, key.shp = 'geo',key.data = "geo")

tmap_style("classic")
final2 %>% 
  tm_shape() +
  tm_polygons(col='Business',title = "Jam consumption (mg/day)") +
  tm_text("CNTR_CODE", root=4) 





# tmap_style("classic")
# Europe %>% 
#   tm_shape() +
#   tm_polygons(col = 'income_grp', title = 'GRP') +
#   tm_text("iso_a3", size="AREA", root=5) +
#   tm_format_Europe()
#   
# 
# tm_shape(World, filter = World$continent=="Europe") +
#   tm_polygons("HPI", id = "name")
# 
