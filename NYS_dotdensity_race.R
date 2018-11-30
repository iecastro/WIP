library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)


options(tigris_class = "sf", tigris_use_cache = TRUE)
Sys.getenv("CENSUS_API_KEY")

### 

vars <- c(White = "B02001_002E",
          Black = "B02001_003E",
          AmericanIndian = "B02001_004E",
          Asian = "B02001_005E",
          NativeHawaiian = "B02001_006E" )

NY_race <- get_acs(geography = "tract", state = "NY", variables = vars,
                   geometry = TRUE,
                   output = "wide") %>% select(White,  Black, AmericanIndian,
                                               Asian,  NativeHawaiian)


###### ref - ## https://www.cultureofinsight.com/blog/2018/05/02/2018-04-08-multivariate-dot-density-maps-in-r-with-sf-ggplot2/
##### generate dots 
random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

# data frame of number of dots to plot for each party (1 for every 100 votes)
num_dotsNY <- as.data.frame(NY_race) %>% 
  select(White:NativeHawaiian) %>% 
  mutate_all(funs(. / 100)) %>% 
  mutate_all(random_round)

# generates data frame with coordinates for each point 
sf_dotsNY <- map_df(names(num_dots), 
                  ~ st_sample(NY_race, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                    st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                          # pull out coordinates into a matrix
                    as_tibble() %>%                                               # convert to tibble
                    setNames(c("lon","lat")) %>%                                  # set column names
                    mutate(Race = .x)                                            # add categorical party variable
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

head(sf_dotsNY)

ggplot() +
  geom_sf(data = NY_race, fill = "transparent",colour = "white") +
  geom_point(data = sf_dotsNY, aes(lon, lat, color = Race)) + 
  scale_color_viridis_d()

ggplot() +
  geom_sf(data = NY_race, fill = NA,color = "#fffff") +
  geom_point(data = sf_dotsNY, aes(lon, lat, color = Race)) + 
  scale_color_viridis_d() + facet_wrap(.~Race) + theme_minimal()
