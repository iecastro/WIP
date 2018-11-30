library(tidycensus)
library(tidyverse)
library(tigris)
library(reshape2)
library(psych)
library(tis)

options(tigris_use_cache = TRUE)

census_api_key("")

v16 <- load_variables(2016, "acs5", cache = TRUE)
View(v15)
v10 <- load_variables(2010, "acs5", cache = TRUE)



### median income  B19013_001   
### median rent    B25031_001  # 2010 - B25064_001
### prop. college  B23006_016  # 2010 - B16010_028

###2016
Income2016 <- get_acs(geography = "tract", 
                      variables = c(Income2016 = "B19013_001"),year = 2016, state = "NY",
                      county = "Onondaga") %>% rename(Income2016 = estimate)%>% select(-c(variable,moe))
Rent2016 <- get_acs(geography = "tract", 
                      variables = c(Rent2016 = "B25031_001"),year = 2016, state = "NY",
                      county = "Onondaga") %>% rename(Rent2016 = estimate)%>% select(-c(variable,moe))
College2016 <- get_acs(geography = "tract", 
                   variables = "B23006_016",year = 2016, state = "NY",
                   county = "Onondaga")  %>% rename(College2016 = estimate) %>% select(-c(variable,moe))

Y2016 <- left_join(Income2016,Rent2016)
Y2016 <- left_join(Y2016,College2016)


###2010
Income2010 <- get_acs(geography = "tract", 
                      variables = c(Income2010 = "B19013_001"),year = 2010, state = "NY",
                      county = "Onondaga") %>% rename(Income2010 = estimate)%>% select(-c(variable,moe))
Rent2010 <- get_acs(geography = "tract", 
                    variables = c(Rent2010 = "B25064_001"),year = 2010, state = "NY",
                    county = "Onondaga") %>% rename(Rent2010 = estimate)%>% select(-c(variable,moe))
College2010 <- get_acs(geography = "tract", 
                       variables = "B16010_028", year = 2010, state = "NY",
                       county = "Onondaga")  %>% rename(College2010 = estimate) %>% select(-c(variable,moe))

Y2010 <- left_join(Income2010,Rent2010)
Y2010 <- left_join(Y2010,College2010)

###2013
Income2013 <- get_acs(geography = "tract", 
                      variables = c(Income2013 = "B19013_001"),year = 2013, state = "NY",
                      county = "Onondaga") %>% rename(Income2013 = estimate)%>% select(-c(variable,moe))
Rent2013 <- get_acs(geography = "tract", 
                    variables = c(Rent2013 = "B25064_001"),year = 2013, state = "NY",
                    county = "Onondaga") %>% rename(Rent2013 = estimate)%>% select(-c(variable,moe))
College2013 <- get_acs(geography = "tract", 
                       variables = "B16010_028", year = 2013, state = "NY",
                       county = "Onondaga")  %>% rename(College2013 = estimate) %>% select(-c(variable,moe))

Y2013 <- left_join(Income2013,Rent2013)
Y2013 <- left_join(Y2013,College2013)

##### merged

##GentData <- merge(Y2010,Y201, by.x = "GEOID","NAME", by.y = "GEOID","NAME",  
##                  all.x = TRUE, all.y = FALSE) %>% select(-c(NAME.y))

GentData <- merge(Y2010,Y2016, by.x = "GEOID","NAME", by.y = "GEOID","NAME",  
                  all.x = TRUE, all.y = FALSE)
GentData <- GentData %>% select(-c(NAME.y))
as.tibble(GentData)

### rank/change vars 

Gent2 <- GentData %>% separate(NAME.x, c("Tract", "County", "State"), sep = ",") %>% separate(Tract,
                          c("name","Tract"), sep = 12)
Gent2 <- Gent2 %>% transmute(Tract = Tract, Inc10 = rank(Income2010), Rnt10 = rank(Rent2010), Coll10 = rank(College2010),
                        Income.growth = rank(Income2016 - Income2010), Rent.growth = rank(Rent2016 - Rent2010),
                        College.growth = rank(College2016 - College2010))
rownames(Gent2) <- Gent2[,1]
Gent2 <- Gent2 %>% select(-c(Tract))


PC <- principal(Gent2,2,scores = TRUE)
biplot.psych(PC, labels = rownames(Gent2))
biplot(PC)

                                                      

### map 
NY <-  places("NY",cb = FALSE)
plot(NY)
