library(tidyverse)
library(readxl)
library(tigris)
library(stringr)


shootings <- read_csv("https://raw.githubusercontent.com/washingtonpost/data-school-shootings/master/school-shootings-data.csv")
shootings

##### NCES school locale
##location
shootings$location[shootings$ulocale == 11] <- 'City'
shootings$location[shootings$ulocale == 12] <- 'City'
shootings$location[shootings$ulocale == 13] <- 'City'

shootings$location[shootings$ulocale == 21] <- 'Suburb'
shootings$location[shootings$ulocale == 22] <- 'Suburb'
shootings$location[shootings$ulocale == 23] <- 'Suburb'

shootings$location[shootings$ulocale == 31] <- 'Town'
shootings$location[shootings$ulocale == 32] <- 'Town'
shootings$location[shootings$ulocale == 33] <- 'Town'

shootings$location[shootings$ulocale == 41] <- 'Rural'
shootings$location[shootings$ulocale == 42] <- 'Rural'
shootings$location[shootings$ulocale == 43] <- 'Rural'

##type
shootings$type[shootings$ulocale == 11] <- 'Large'
shootings$type[shootings$ulocale == 12] <- 'Mid-size'
shootings$type[shootings$ulocale == 13] <- 'Small'

shootings$type[shootings$ulocale == 21] <- 'Large'
shootings$type[shootings$ulocale == 22] <- 'Mid-size'
shootings$type[shootings$ulocale == 23] <- 'Small'

shootings$type[shootings$ulocale == 31] <- 'Fringe'
shootings$type[shootings$ulocale == 32] <- 'Distant'
shootings$type[shootings$ulocale == 33] <- 'Remote'

shootings$type[shootings$ulocale == 41] <- 'Fringe'
shootings$type[shootings$ulocale == 42] <- 'Distant'
shootings$type[shootings$ulocale == 43] <- 'Remote'

shootings$SRO <- as.factor(shootings$resource_officer)

shootings$rateCas <- shootings$casualties / shootings$enrollment  
shootings$rateK <- shootings$killed / shootings$enrollment 


weapType <- c("revolver","handgun","shotgun","rifle")
weap_match <- str_c(weapType, collapse = "|")
shootings$weap2 <- str_extract(shootings$weapon, weap_match)

#########

ggplot(shootings, aes(location)) + geom_bar(aes(fill = type))
ggplot(shootings, aes(type)) + geom_bar(aes(fill = location))


ggplot(shootings, aes(day_of_week)) + geom_bar(aes(fill = location))

ggplot(shootings, aes(school_year)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(shootings, aes(school_year)) + geom_bar(aes(fill = location)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(shootings, aes(state)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#####
shootings %>% group_by(year) %>%
  summarise(count=n()) %>%
ggplot(aes(year,count)) +geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###
shootings %>%
  filter(shooting_type %in% c("indiscriminate",
                    "targeted","targeted and indiscriminate")) %>%
  group_by(year,shooting_type) %>% summarise(count=n()) %>%
ggplot(aes(year,count, group=shooting_type, color = shooting_type)) +geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
### loss of life 
ggplot(shootings,aes(shooting_type, killed)) + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### total casualties - death/injury
ggplot(shootings,aes(shooting_type, casualties)) + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###SRO
ggplot(shootings,aes(SRO, killed)) + geom_boxplot()
ggplot(shootings,aes(SRO, casualties)) + geom_boxplot()


options(tigris_class = "sf")
County <- counties()

x <- unique(shootings$district_name)
st <- unique(shootings$state)

district <-map_df(st, function(x){school_districts(x)})
                 
district %>% filter(NAME == x)


## countyMap <- geo_join(County,shootings, by_sp = "COUNTYFP", by_df = "county_fips", how = "left")

#########

shoot2 <- shootings %>% filter(shooting_type %in% c("indiscriminate",
                                                "targeted"))
shoot2$rankW[shoot2$weap2 == "handgun"] <- 1
shoot2$rankW[shoot2$weap2 == "revolver"] <- 1
shoot2$rankW[shoot2$weap2 == "shotgun"] <- 2
shoot2$rankW[shoot2$weap2 == "rifle"] <- 3

shoot2 <- shoot2 %>% filter(weap2 != "NA") %>% group_by(weap2,shooting_type) %>%
  mutate(max = max(killed), min = min(killed))

ggplot(shoot2,aes(weap2, casualties)) + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################

ggplot(shoot2, aes(x = weap2, y = killed ,fill = shooting_type)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  coord_flip() +  # Flip axes
  labs(title="") + theme_minimal() +
  theme(axis.text = element_text(color = "black"), 
        axis.ticks = element_blank()) +   # Centre plot title
   scale_fill_brewer(palette = "Set1")  # Color palette

ggplot(shoot2, aes(killed, weap2)) +
  geom_line(aes(group =weap2)) +
  geom_point(aes(max))+
  facet_grid(.~shooting_type) +
  theme_minimal()

ggplot(shoot2, aes(weap2, age_shooter1)) + geom_boxplot()
ggplot(shoot2, aes(weap2, casualties)) + geom_boxplot()

######
## needs weapon source recode

ggplot(shoot2, aes(weapon_source)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  coord_flip()

  