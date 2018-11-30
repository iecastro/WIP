library(tidycensus)
library(tidyverse)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)


## Insurance coverage - VAHC only
vars <- c(v18to34 = "B27010_025E", V35to64 = "B27010_041E", v65over = "B27010_057E")

## county names
NYfips <- fips_codes %>% filter(state == "NY")
NYcounties <- unique(NYfips$county)[1:62]

## fetch data
## all blockgroups function
VAHC_bg <- reduce(map(NYcounties, function(x){
  get_acs(geography = "block group", state = "NY",
          county = x, variables = vars, geometry = TRUE,
                output = "wide") %>% 
  mutate(total = v18to34 + V35to64 + v65over)
}),
rbind
)

## enter geography parameter - tract or county
VAHC <- get_acs(geography = "county", state = "NY",
        variables = vars, geometry = TRUE,
        output = "wide") %>% 
  mutate(total = v18to34 + V35to64 + v65over)

## county map
counties <- get_acs(geography = "county", state = "NY", 
                    variables = vars, geometry = TRUE, output = "wide") %>% 
            mutate(total = v18to34 + V35to64 + v65over)


VAHC %>% filter(total > 0) %>% 
  #ggplot(data = VAHC) + 
  ggplot() + 
  geom_sf(aes(fill = total), color = NA) +
  geom_sf(data = counties, fill = NA,color = "#ffffff", size=.5) + 
  theme_minimal() + 
  theme(axis.text = element_blank()) +
  scale_fill_viridis_c(option = "cividis")
 
