library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(RSocrata)
library(psych)

options(tigris_class = "sf", tigris_use_cache = TRUE)
Sys.getenv("CENSUS_API_KEY")


### county data

allbirthsNY <-  read.socrata("https://health.data.ny.gov/resource/49hc-tdpn.json") %>%
  filter(year == "2015") %>% select(-c(table)) %>% 
  mutate(number_of_births =as.numeric(number_of_births)) %>%
  spread(gestation_in_weeks,number_of_births) %>%
  mutate(preterm = `<20` + `20-23`+`24-27`+`28-31`+`32-33`+`34-36`+`37-38`)



#### NDI 

vars <- c("B17001_002", "B17001_001", "B06009_002" , "B06009_001",
          "B09008_011", "B09008_001","B08124_002", "B08124_001", "B25014_005", 
          "B25014_006",  "B25014_007","B25014_011", "B25014_012", "B25014_013",  
          "B25014_001", "B19058_002", "B19058_001","C23002C_021", "C23002D_008", 
          "C23002C_017", "C23002D_003","B19001_002", "B19001_003", "B19001_004", 
          "B19001_005", "B19001_006", "B19001_001")
acs_NY <- get_acs(geography = "county",state = "NY", variables = vars, year = 2015,
                  output = "wide") %>%
  mutate(pct_poverty = B17001_002E/B17001_001E,
         pct_noHS = B06009_002E / B06009_001E,
         pct_FHH = B09008_011E / B09008_001E,
         pct_mgmt = B08124_002E /  B08124_001E, 
         pct_crowd =  (B25014_005E +B25014_006E+ B25014_007E + 
                         B25014_011E + B25014_012E + B25014_013E) / B25014_001E,
         pct_pubassist = B19058_002E/B19058_001E,
         pct_unempl = (C23002C_021E + C23002D_008E)  / (C23002C_017E + C23002D_003E),
         pct_under30K =( B19001_002E+B19001_003E+B19001_004E+B19001_005E +
                           B19001_006E) / B19001_001E)
## select transformed variables
values  <-  acs_NY %>% select(pct_poverty,pct_noHS,pct_FHH,pct_mgmt,pct_crowd,
                              pct_pubassist, pct_unempl,pct_under30K) %>% as.matrix()
values[is.nan(values)] <- 0
## PCA
ND <- principal(values,nfactors = 1)          
NDI_NY <- cbind(acs_NY,ND$scores) 
NDI_NY <- NDI_NY %>% select(NAME,GEOID,PC1) %>% 
  separate(NAME, into = c("County","State"), sep = ",")

####
NYcounties <- get_acs(geography = "county", state = "NY", variables = c("B01001_001"), 
                       output = "wide", geometry = TRUE)

mapdata <- geo_join(NYcounties,NDI_NY, by_sp = "GEOID", by_df = "GEOID") %>% 
  select(-c(NAME)) %>% separate(County, into = c("Name", "Place"))

merged <- merge(mapdata,allbirthsNY, by.x = "Name", by.y = "county", all.x=TRUE)
####

ggplot() + geom_sf(data = merged, aes(fill = PC1)) +
  theme_minimal() + theme(axis.text = element_blank(), legend.position = "bottom") +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(fill = "Index", caption = "Data: US Census 5-year ACS estimates, 2015")+
  ggtitle(" ", subtitle = "County-level Deprivation")





