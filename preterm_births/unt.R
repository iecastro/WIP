library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(RSocrata)

options(tigris_class = "sf", tigris_use_cache = TRUE)
Sys.getenv("CENSUS_API_KEY")


### county data

allbirths <-  read.socrata("https://health.data.ny.gov/resource/49hc-tdpn.json") %>%
  filter(county == "Onondaga" & year == "2015") %>% select(-c(table)) %>% 
  mutate(number_of_births =as.numeric(number_of_births)) %>%
  spread(gestation_in_weeks,number_of_births) %>%
  mutate(preterm = `<20` + `20-23`+`24-27`+`28-31`+`32-33`+`34-36`+`37-38`)

### shapefile
OndCnty <- get_acs(geography = "county", state = "NY",
                   variables = c("B01001_001"),
                   output = "wide", geometry = TRUE, cb = FALSE) %>%
                  separate(NAME, into=c("County", "State"), sep = 8) %>% 
                  filter(County == "Onondaga") %>%  select(-c(State))

### ACS data - 2014

vars <- c(female15to17 = "B01001_030E",female18to19 = "B01001_031E",
          female20 = "B01001_032E", female21 = "B01001_033E",
          female22to24 = "B01001_034E", female25to29 = "B01001_035E", 
          female30to34 = "B01001_036E", female35to39 = "B01001_037E",
          female40to44 = "B01001_038E")

blockgrp <- get_acs(geography = "block group", state = "NY", county = "Onondaga",
        variables = vars, year = 2014, 
        output = "wide", geometry = TRUE, cb = FALSE) %>%
        mutate(totalfem = sum(female15to17 +female18to19+female20 + female21 +female22to24+ female25to29 +
                       female30to34 + female35to39 +female40to44)) %>%
            group_by(GEOID) %>%  mutate(femaleBG = sum(female15to17 +female18to19+female20 + female21 +female22to24+ female25to29 +
              female30to34 + female35to39 +female40to44), fempctBG = femaleBG / totalfem)

###
st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

cnty_water <- area_water("NY", "Onondaga", class = "sf")
blockgrp_land  <- st_erase(blockgrp, cnty_water)

ggplot() + geom_sf(data = blockgrp_land, aes(fill = fempctBG)) +
  scale_fill_viridis_c()
#####

intersect <- st_intersection(blockgrp_land,OndCnty)

### merge to shapefile
countydata <- geo_join(intersect,allbirths, by_sp = "County", by_df = "county")

countydata$loc.type[as.numeric(countydata$GEOID) < 360670061040] <- "City"
countydata$loc.type[as.numeric(countydata$GEOID) >= 360670061040] <- "County"

ggplot() + geom_sf(data = countydata, aes(fill = preterm))
ggplot() + geom_sf(data = countydata, aes(fill = fempctBG))

#######

prob.counts <-  countydata %>% select(loc.type,totalfem, fempctBG,Total,preterm) %>%
  mutate(PBpct = Total / preterm, LBprob = Total*fempctBG, 
         PBprobcounts = LBprob*PBpct) 

ggplot() + geom_sf(data=prob.counts, aes(fill=PBprobcounts))+
  scale_fill_viridis_c()
