########################################
# Stop & Frisk                         #
# Data prep for analysis (non-spatial) #
# Updated July 25, 2020                #
########################################

library(tidyverse)
library(readxl)
library(ggmap)
library(pdftools)
library(sf)


# SF 2012-2014 ----
# beat and address (no geocoding, remove address)
SF2014a <- read_csv("initial/2014withSF.csv") 
SF2014a <- SF2014a %>% 
  mutate(SFTYPE = "STOP WITH SEARCH OR FRISK",
         Date = as.Date(Date, "%m/%d/%y"),
         TIME = as.character(TIME),
         `INCIDENT #` = as.character(`INCIDENT #`)) %>% 
  select(-c(`Stop & Frisk`, Address))

SF2014b <- read_csv("data/2014withoutSF.csv")
SF2014b <- SF2014b %>% 
  mutate(SFTYPE = "Search WITHOUT Stop-Frisk",
         Date = as.Date(Date, "%m/%d/%y")) %>% 
  select(-c(MONTH, Address))

SF2014 <- SF2014a %>% bind_rows(SF2014b) 
rm(SF2014a, SF2014b)


# SF 2016 ---- 
# beat, no address (no geocoding)
SF2016a <- read_csv("initial/2016withSF.csv")
SF2016a <- SF2016a %>% 
  mutate(SFTYPE = "STOP WITH SEARCH OR FRISK",
         Date = as.Date(Date, "%m/%d/%y")) %>% 
  select(-`Stop & Frisk`)

SF2016b <- read_csv("initial/2016withoutSF.csv") 
SF2016b <- SF2016b %>%
  mutate(SFTYPE = "Search WITHOUT Stop-Frisk",
         Date = as.Date(Date, "%m/%d/%y"),
         Arrest = "No")

SF2016 <- SF2016a %>% bind_rows(SF2016b) 
rm(SF2016a, SF2016b)

# Additional 2016 records - November-December
#   is there code to generate this file (2015_Stop_and_Frisk.csv)?
#   or other documentation on how it was translated from the pdf?
# also, this looks like it might only be adding November-December stops with search/frisk
#   to what degree should we be worried about this distorting the 2016 records?
SF2016_add <- read_csv("data/2016 Stop and Frisk.csv") 
SF2016_add <- SF2016_add %>%
  rename(Date = `REPORT DATE`, 
         `Dispatched/Self Initiated` = `Dispatched/Self-Initiated`) %>% 
  mutate(SFTYPE = "STOP WITH SEARCH OR FRISK",
         Date = as.Date(Date, "%m/%d/%y"), 
         AUTHORITY = NA) %>% 
  select(-c("Stop& Frisk", X11, X12, GENDER)) %>% 
  filter(Date > "2016-10-15")
  
SF2016 <- SF2016 %>% bind_rows(SF2016_add) 
rm(SF2016_add)


# SF 2017 ---- 
# no beat, address (needs geocoding)
SF2017_read <- read.xlsx("initial/2017 Stop and Frisk SDP Reviewed.xlsx", sheet = 1)

addresses <- read_csv("data/Master_Address_Points.csv")
addresses_table <- read_csv("data/Master_Address_Table.csv")

race_characters<- c("1|\\{|\\[|i|j|\\\\|\\!|\\|" )

SF2017_clean <-
SF2017_read %>%
  mutate(OFFENSE = str_trim(str_replace_all(OFFENSE, "\\[", "")),
         RACE = toupper(
                   str_trim(
                      str_replace_all(RACE, race_characters, "")
                           )
                        ),
         ADDRESS = toupper(str_trim(ADDRESS))
          ) 

# locations 
sf_for_join <-
  SF2017_clean %>% 
  separate(ADDRESS, c("NUMBER", "STREET"), sep = " ", extra = "merge") %>%
  mutate(NUMBER = as.numeric(NUMBER)) %>%
  as_tibble()

address_points_cleaned <-
  addresses %>% 
  mutate_at(vars( "geo_MAT_PREDIR", "geo_MAT_ST_NAME", "geo_MAT_SUFFIX", "geo_MAT_POSTDIR"), funs(ifelse(is.na(.), "", . ))) %>%
  unite(STREET, geo_MAT_PREDIR, geo_MAT_ST_NAME, geo_MAT_SUFFIX, geo_MAT_POSTDIR, sep = " ", remove = FALSE)  %>%
  mutate(STREET = str_trim(STREET))

setdiff(sf_for_join$STREET, address_points_cleaned$STREET)

address <- "5TH"
sf_for_join$STREET[grepl(address, sf_for_join$STREET)]
address_points_cleaned$STREET[grepl(address, address_points_cleaned$STREET)]

SF2017Locations <-
  sf_for_join %>%
  mutate(id = 1:n()) %>%
  left_join(address_points_cleaned) %>% 
  mutate(house_dist = abs(geo_MAT_ST_NUMBER - NUMBER)) %>%
  group_by(id) %>%
  arrange(id, house_dist ) %>% 
  slice(1) 

cumsum(round(table(SF2017Locations$house_dist)/sum(table(SF2017Locations$house_dist))*100, 2))

SF2017Locations %>%
  filter(house_dist > 5) %>% 
  arrange(house_dist) %>%
  select(NUMBER, STREET, geo_MAT_ST_NUMBER, address, house_dist) %>% View()

# add beats to 2017 records
beatmap <- readRDS("data/beat_pop_map.Rds")

SF2017_sf <- as.data.frame(SF2017Locations) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=FALSE) # convert to sf object

# overlay visually (another check)
plot(SF2017_sf$geometry)
plot(beatmap$geometry)

# join detentions to beat polygons
SF2017 <- st_join(SF2017_sf, beatmap[c("BEAT_NO", "NAME")], left = TRUE)

rm(SF2017_read, SF2017_clean, SF2017_sf, SF2017Locations, sf_for_join)


# SF 2017 (arrest) ---- 
# no beat or address, but arrest (for use in outcome analysis)
SF2017_arrest <- read_xlsx("initial/July 11 2017 - Investigative Detention FOIA.xlsx")
SF2017_arrest <- SF2017_arrest %>% 
  rename(SFTYPE = `Stop & Frisk Type`) %>% 
  mutate(SFTYPE = recode(SFTYPE,
                         "Search WITHOUT Stop-Frisk" = "STOP WITHOUT SEARCH OR FRISK"))


# SF 2015  ----
# no beat, address (needs geocoding)
#   is there code to generate this file (2015_Stop_and_Frisk.csv)?
#   or other documentation on how it was translated from the pdf?
SF2015_read <- read_csv("data/2015_Stop_and_Frisk.csv") 

SF2015_clean <- SF2015_read %>%
  rename(Address = "Individuals Stopped by Location", Arrest = "Arrest/Summons",
         SFTYPE = "Search/Frisk") %>%
  mutate(SFTYPE = recode(SFTYPE,
                         YES = "STOP WITH SEARCH OR FRISK",
                         NO = "Search WITHOUT Stop-Frisk")) %>% 
  select(c(Address, Race, Arrest, SFTYPE)) %>%
  filter(Race %in% c("B", "W"))

SF2015_clean <- SF2015_clean %>% 
  separate(Address, c("NUMBER", "STREET"), sep = " ", extra = "merge") %>%
  mutate(NUMBER = as.numeric(NUMBER)) %>%
  as_tibble()

SF2015Locations <- SF2015_clean %>%
  mutate(id = 1:n()) %>%
  left_join(address_points_cleaned) %>% 
  mutate(house_dist = abs(geo_MAT_ST_NUMBER - NUMBER)) %>%
  group_by(id) %>%
  arrange(id, house_dist ) %>%
  slice(1)  %>%
  ungroup()

cumsum(round(table(SF2015Locations$house_dist)/sum(table(SF2015Locations$house_dist))*100, 2))

# add beats to 2017 records
SF2015_sf <- as.data.frame(SF2015Locations) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=FALSE) # convert to sf object

# join detentions to beat polygons
SF2015 <- st_join(SF2015_sf, beatmap[c("BEAT_NO", "NAME")], left = TRUE)

rm(SF2015_read, SF2015_clean, SF2015_sf, SF2015Locations)
# write_csv(Stop_Frisk_2015_Final, path = "data/Stop_Frisk_2015_Final") # for spatial

rm(address_points_cleaned, addresses, addresses_table,  beatmap, address, race_characters)


# COMBINE ---- 
# reduce each df to common/key variables and save
SF2014brief <- SF2014 %>% 
  rename(date = Date, year = YEAR, type = SFTYPE,
         beat = BEAT, offense = OFFENSE, race = RACE,
         arrest = Arrest) %>% 
  mutate(period = "2012-2014") %>% 
  select(date, year, period, type, beat, offense, race, arrest)

SF2016brief <- SF2016 %>% 
  rename(date = Date, type = SFTYPE, beat = BEAT,
         offense = OFFENSE, race = RACE, arrest = Arrest) %>% 
  mutate(year = lubridate::year(date),
         period = "2016-2017") %>% 
  select(date, year, period, type, beat, offense, race, arrest)

SF2017brief <- SF2017 %>% 
  rename(type = SFTYPE, beat = BEAT_NO, offense = OFFENSE,
         race = RACE) %>% 
  mutate(year = 2017, date = NA, period = "2016-2017", arrest = NA_character_) %>% 
  select(date, year, period, type, beat, offense, race, arrest) %>% 
  st_drop_geometry()

SF2017brief_arrest <- SF2017_arrest %>% 
  rename(date = Date, type = SFTYPE, offense = Offense, race = Race, arrest = Arrest) %>% 
  mutate(year = 2017, period = "2016-2017", beat = NA) %>% 
  select(date, year, period, type, beat, offense, race, arrest)

SF2015brief <- SF2015 %>% 
  rename(type = SFTYPE, beat = BEAT_NO,
         race = Race, arrest = Arrest) %>% 
  mutate(year = 2015, date = NA,  offense = NA, period = NA) %>% 
  select(date, year, period, type, beat, offense, race, arrest) %>% 
  st_drop_geometry()

# combine
SF_beat <- rbind(SF2014brief, SF2015brief, SF2016brief, SF2017brief)
SF_arrest <- rbind(SF2014brief, SF2015brief, SF2016brief, SF2017brief_arrest)

# SF_arrest <- SF_arrest %>% 
#   mutate(arrest1 = ifelse(str_length(arrest)>2, 0, 1))
# Are NA's definitionally arrests or missing?
# Are all of these outcomes equivalent to an arrest or are there other distinctions?


saveRDS(SF_beat, "data/sf_combined.Rds")
saveRDS(SF_arrest, "data/sf_arrest.Rds")


# check - change over time 
#  (are the missing months of 2016 likely to be misleading; e.g., if these are heavy detention months?)
SF2014 %>% filter(YEAR == 2013) %>% 
  ggplot(aes(x = lubridate::month(Date))) + geom_bar()

SF2016 %>% 
  ggplot(aes(x = lubridate::month(Date))) + geom_bar()


# NOT USED
# read in 2019-2020 from google, prep and save
googlesheets4::sheets_deauth()
url_sheet <- "https://docs.google.com/spreadsheets/d/1JNULbOmVD_FWc5kVgM5nQFi2nLkobqDoYvb7z5dgcy4/edit?usp=sharing"
sf1920_data <- googlesheets4::read_sheet(url_sheet,
                          sheet = "Summary")

sf1920_data <- sf1920_data %>% # remove summed row at bottom
  filter(!is.na(beat))
# whoa


