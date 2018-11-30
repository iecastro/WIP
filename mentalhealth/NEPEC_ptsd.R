library(tidyverse)

## 2014
ptsd <- jsonlite::fromJSON("https://www.data.va.gov/sites/default/files/NEPEC_Overview_PTSD_FY14.json",
                           "text", simplifyDataFrame = TRUE) %>% as.tibble()

## 2015
ptsd <- jsonlite::fromJSON("https://www.data.va.gov/sites/default/files/NEPEC_Overview_PTSD_FY15.json",
                           "text", simplifyDataFrame = TRUE) %>% as.tibble()

glimpse(ptsd)

ptsd$Value <- as.numeric(ptsd$Value)

unique(ptsd$Category)
unique(ptsd$Location)

## location codes VISN 2
# 528A8	ALBANY
# 528A6	BATH
# 528A5	CANANDAIGUA
# 528A7	SYRACUSE
# 528	WESTERN NEW YORK HCS


visn2 <- ptsd %>% filter(Location %in% c("VISN2", "528A8", "528A6", "528A5", "528A7", "528"))
unique(visn2$Item)
                
visn2 %>% filter(Location == "528A5" & ValueType == "Number")

visn2$site[visn2$Location == "528A8"] <- "Albany"
visn2$site[visn2$Location == "528A6"] <- "Bath"
visn2$site[visn2$Location == "528A5"] <- "Canandaigua"
visn2$site[visn2$Location == "528A7"] <- "Syracuse"
visn2$site[visn2$Location == "528"] <- "Western NY"

visn2 %>% spread(key = Item, value = Value) %>% 
  filter(!is.na(site)) %>%
  ggplot(aes(reorder(site,-`N with PTSD`), `N with PTSD`)) + 
  geom_col()


visn2 %>% spread(key = Item, value = Value) %>% 
  filter(!is.na(site)) %>%
  ggplot(aes(site, `Average total mental health stops when visits >=1`)) + 
  geom_col()

visn2 %>% spread(key = Item, value = Value) %>% 
  filter(!is.na(site)) %>%
  ggplot(aes(site, `Average psych stops for PTSD dx >=1`)) + 
  geom_col()

visn2 %>% 
  filter(Item %in% c("N with PTSD", "Veterans with PTSD with any VA MH care") & !is.na(site)) %>% 
  spread(key = Item, value = Value) %>% 
  mutate(pct_mh = `Veterans with PTSD with any VA MH care`/ `N with PTSD`,
         pt_control = (`N with PTSD` - `Veterans with PTSD with any VA MH care`) * 365, 
         pt_cases = `Veterans with PTSD with any VA MH care` * 182.5,
         PAR = pt_control + pt_cases, rate = (`Veterans with PTSD with any VA MH care` / PAR)) %>%
  ggplot(aes(site,Value, fill = Item)) + geom_col(position = "dodge") +
  coord_flip() + theme_minimal() + theme(axis.text = element_text(color = "black")) +
  scale_fill_viridis_d()


