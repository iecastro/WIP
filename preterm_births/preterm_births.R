library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(RSocrata)
library(classInt)


options(tigris_class = "sf", tigris_use_cache = TRUE)
Sys.getenv("CENSUS_API_KEY")


### county data

allbirths <-  read.socrata("https://health.data.ny.gov/resource/49hc-tdpn.json") %>%
  filter(year == "2015") %>% select(-c(table)) %>% 
  mutate(number_of_births =as.numeric(number_of_births)) %>%
  spread(gestation_in_weeks,number_of_births) %>%
  mutate(preterm = `<20` + `20-23`+`24-27`+`28-31`+`32-33`+`34-36`+`37-38`)

allbirths <- allbirths %>%  filter(county != "New York State") %>% 
  mutate(county = paste(county, "County"))

### shapefile
counties <- get_acs(geography = "county", state = "NY",
                   variables = c("B01001_001"),
                   output = "wide", geometry = TRUE, cb = FALSE) %>%
                  separate(NAME, into=c("County", "State"), sep = ",") %>%
                  select(-c(State)) 



###############  water areas  - needs debug to bind all counties #####
names <- unique(counties$County)[1:62]

st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

water <- reduce(map_df(names, function (x){
    area_water("NY", x, class = "sf")}),
    rbind)
NYwater <- water(names)
NYS_land  <- st_erase(counties, water)


intersect <- st_intersection(blockgrp_land,OndCnty)
####################################################################

### merge to shapefile
alldata <- merge(counties,allbirths, by.x = "County", by.y= "county", all.y = TRUE)

ggplot() + geom_sf(data = alldata, aes(fill = Total)) +
  scale_fill_viridis_c()


ggplot() + geom_sf(data = alldata, aes(fill = preterm)) + 
  scale_fill_viridis_c()

####### standardized incidence ratio


r <- sum(alldata$preterm)/sum(alldata$Total)
alldata$Expected <- alldata$Total * r
alldata$SIR <- alldata$preterm/alldata$Expected

ggplot() + geom_sf(data = alldata, aes(fill = SIR)) +
  geom_sf(data = counties, fill = NA, color = "#7f7f7f", size = .5) +
  scale_fill_viridis_c() + theme_minimal()

library(epitools)

SIRCI <-  pois.exact(alldata$SIR, conf.level = 0.95)

prob.counts <-  countydata %>% select(loc.type,totalfem, fempctBG,Total,preterm) %>%
  mutate(PBpct = Total / preterm, LBprob = Total*fempctBG, 
         PBprobcounts = LBprob*PBpct) 

ggplot() + geom_sf(data=prob.counts, aes(fill=PBprobcounts))+
  scale_fill_viridis_c()
