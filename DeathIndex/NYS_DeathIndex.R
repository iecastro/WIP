library(tidyverse)
library(RSocrata)
library(sf)


martinos <- read.socrata("https://health.data.ny.gov/resource/tepf-4hw3.csv") %>%
  filter(decedent_ln == "MARTINO") %>% as.tibble()


res_codes <- read_sf("https://health.data.ny.gov/resource/ufmv-zwcb.geojson") 
res_codes$district <- as.factor(res_codes$district)

martinos %>% 
  ggplot(aes(as.factor(data_year))) + geom_bar()

martinos %>% unite(name,decedent_fn,decedent_mn) %>%
ggplot( aes( as.factor(data_year),name)) + 
  geom_segment(aes(x = "1956", y = name, xend = as.factor(data_year), yend = name),
               color = "grey50") + 
  geom_point() + theme_minimal() + 
  theme(axis.text.y = element_text(color = "black", size = 7),
        axis.text.x = element_text(color = "black", size = 9)) + 
  labs(x = " ", y = " ")


ggplot(martinos,(aes(decedent_age))) + geom_histogram(aes(fill = gender), position = "dodge") +
  theme_minimal() + scale_fill_viridis_d(option = "cividis") + 
  labs(fill = " ", x = "Decedent Age")


martinos %>% select(First = decedent_fn, Last = decedent_ln, `M/F` = gender,
                    Date = dateof_death,`State File No.`=stfileno) %>%
  separate(Date, into = c("Date", "T"), sep = 10) %>% select(-c(T))




