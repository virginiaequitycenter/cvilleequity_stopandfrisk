################
# Stop & Frisk #
################
library(tidyverse)
library(openxlsx)
library(ggmap)
library(pdftools)

#Sam original -------
# setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/cvilleequity_stopandfrisk")

addresses <- read_csv("data/Master_Address_Points.csv")
addresses_table <- read_csv("data/Master_Address_Table.csv")


## Clean SF 2017 ##
SF2017_read <- read.xlsx("./data/2017 Stop and Frisk SDP Reviewed.xlsx", sheet = 1)

race_characters<- c("1|\\{|\\[|i|j|\\\\|\\!|\\|" )

SF2017 <-
SF2017_read %>%
  mutate(OFFENSE = str_trim(str_replace_all(OFFENSE, "\\[", "")),
         
         RACE = toupper(
                   str_trim(
                      str_replace_all(RACE, race_characters, "")
                           )
                        ),
         ADDRESS = toupper(str_trim(ADDRESS))
          ) 

# View(SF2017)

# locations 
sf_for_join <-
  SF2017 %>% 
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

SF2017Locations<-
  sf_for_join %>%
  mutate(id = 1:n()) %>%
  left_join(address_points_cleaned) %>% 
  mutate(house_dist = abs(geo_MAT_ST_NUMBER - NUMBER)) %>%
  group_by(id) %>%
  arrange(id, house_dist ) %>% 
  slice(1) 

cumsum(round(table(SF2017Locations$house_dist)/sum(table(SF2017Locations$house_dist))*100, 2))


# View(SF2017Locations)

SF2017Locations %>%
  filter(house_dist > 5) %>% 
  arrange(house_dist) %>%
  select(NUMBER, STREET, geo_MAT_ST_NUMBER, address, house_dist) %>% View()



## Clean SF 2016 ##
SF2016WithSF <- read_csv("./data/2016withSF.csv") %>%
  mutate(SFTYPE = "STOP WITH SEARCH OR FRISK",
         Date = as.Date(Date, "%m/%d/%y"))  

SF2016WithoutSF <- read_csv("./data/2016withoutSF.csv") %>%
  mutate(SFTYPE = "Search WITHOUT Stop-Frisk",
         Date = as.Date(Date, "%m/%d/%y"))  

SF2016 <-
  SF2016WithSF %>% bind_rows(SF2016WithoutSF) 


## Clean SF 2014 ##
SF2014WithSF <- read_csv("data/2014withSF.csv") %>% 
  mutate(SFTYPE = "STOP WITH SEARCH OR FRISK",
         Date = as.Date(Date, "%m/%d/%y"))  

SF2014WithoutSF <- read_csv("data/2014withoutSF.csv") %>% 
  mutate(SFTYPE = "Search WITHOUT Stop-Frisk",
         Date = as.Date(Date, "%m/%d/%y"))  

SF2014 <-
SF2014WithSF %>% bind_rows(SF2014WithoutSF) %>%
    mutate(Address = str_replace_all(Address, "  ", " ")) %>%
    mutate(Address = str_replace_all(Address, "  ", " ")) %>%
    mutate(Address = str_replace_all(Address, "  ", " "))

View(SF2014)


# mpc added -----------------
# add beats to 2017 records
library(sf)
beatmap <- readRDS("data/beat_pop_map.Rds")

SF2017_sf <- as.data.frame(SF2017Locations) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=FALSE) # convert to sf object

# overlay visually (another check)
plot(SF2017_sf$geometry)
plot(beatmap$geometry)

# join detentions to beat polygons
SF2017_wbeat <- st_join(SF2017_sf, beatmap[c("BEAT_NO", "NAME")], left = TRUE)


# checks - change over time 
#  (are the missing months of 2016 likely to be misleading; e.g., if these are heavy detention months?)
SF2014 %>% filter(YEAR == 2013) %>% 
  ggplot(aes(x = lubridate::month(Date))) + geom_bar()

SF2016 %>% 
  ggplot(aes(x = lubridate::month(Date))) + geom_bar()


# save for use
# reduce 2014 to key variables and save
SF2014brief <- SF2014 %>% 
  rename(date = Date, year = YEAR, type = "Stop & Frisk",
         beat = BEAT, offense = OFFENSE, race = RACE) %>% 
  mutate(period = "2012-2014") %>% 
  select(date, year, period, type, beat, offense, race)

SF2016brief <- SF2016 %>% 
  rename(date = Date, type = "Stop & Frisk", beat = BEAT,
         offense = OFFENSE, race = RACE) %>% 
  mutate(year = lubridate::year(date),
         period = "2016-2017") %>% 
  select(date, year, period, type, beat, offense, race)

SF2017brief <- SF2017_wbeat %>% 
  rename(type = SFTYPE, beat = BEAT_NO, offense = OFFENSE,
         race = RACE) %>% 
  mutate(year = 2017, date = NA, period = "2016-2017") %>% 
  select(date, year, period, type, beat, offense, race) %>% 
  st_drop_geometry()

# combine
SF <- rbind(SF2014brief, SF2016brief, SF2017brief)

view(SF)

saveRDS(SF, "sf_combined.Rds")


# read in 2019-2020 from google, prep and save
googlesheets4::sheets_deauth()
url_sheet <- "https://docs.google.com/spreadsheets/d/1JNULbOmVD_FWc5kVgM5nQFi2nLkobqDoYvb7z5dgcy4/edit?usp=sharing"
sf1920_data <- googlesheets4::read_sheet(url_sheet,
                          sheet = "Summary")

sf1920_data <- sf1920_data %>% # remove summed row at bottom
  filter(!is.na(beat))
# whoa

#Enrique added ----------

Stop_Frisk_2015_read <- read_csv("data/2015_Stop_and_Frisk.csv") %>%
  rename(Address = "Individuals Stopped by Location", Arrest = "Arrest/Summons",
         SFType = "Search/Frisk") %>%
  select(c(Address, Race, Arrest, SFType)) %>%
  filter(Race %in% c("B", "W"))

Stop_Frisk_2015 <-
  Stop_Frisk_2015_read %>% 
  separate(Address, c("NUMBER", "STREET"), sep = " ", extra = "merge") %>%
  mutate(NUMBER = as.numeric(NUMBER)) %>%
  as_tibble()


Stop_Frisk_2015_Locations<-
  Stop_Frisk_2015 %>%
  mutate(id = 1:n()) %>%
  left_join(address_points_cleaned) %>% 
  mutate(house_dist = abs(geo_MAT_ST_NUMBER - NUMBER)) %>%
  group_by(id) %>%
  arrange(id, house_dist ) %>%
  slice(1)  %>%
  ungroup()

cumsum(round(table(Stop_Frisk_2015_Locations$house_dist)/sum(table(Stop_Frisk_2015_Locations$house_dist))*100, 2))

#Checking for csv typos
#Stop_Frisk_2015_Locations %>%
 # filter(is.na(house_dist)) %>% 
  #arrange(house_dist) %>%
  #select(NUMBER, STREET, geo_MAT_ST_NUMBER, address, house_dist) %>% View()

Stop_Frisk_2015_Spatial <-
  Stop_Frisk_2015_Locations %>%
  ungroup() %>%
  st_as_sf(
    coords = c("lon", "lat"),
    #   agr = "constant",
    crs = 4326,
    #    stringAsFactors = FALSE,
    remove = TRUE
  )

#Borrowed from datacleaning1
beats <- st_read("data/Police_Neighborhood_Area-shp/Police_Neighborhood_Area.shp")
View(beats)

point_in_beat_2015 <- st_join(Stop_Frisk_2015_Spatial, beats, join = st_within)

#View(point_in_beat_2015)

## Print them out ##
Stop_Frisk_2015_Final <-
  point_in_beat_2015 %>%
  ungroup() %>%
  select(SFType, NUMBER, STREET, Arrest, Race, geometry, BEAT_NO, POPULATION)

write_csv(Stop_Frisk_2015_Final, path = "data/Stop_Frisk_2015_Final")


Stop_Frisk_2015_sf <- as.data.frame(Stop_Frisk_2015_Locations) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=4326, remove=FALSE)

#plot(Stop_Frisk_2015_sf$geometry)
#plot(beatmap$geometry)


## 2016 SF Data ## 
Stop_Frisk_2016 <- read_csv("data/2016 Stop and Frisk.csv")



###Adding 2015 data into sf compiled dataset
sf <- read_rds("data/sf_combined.Rds")


SF2015brief <- Stop_Frisk_2015_Final %>% 
  rename(type = SFType, beat = BEAT_NO, 
         offense = Arrest, race = Race) %>% 
  mutate(year = 2015, period = "2015", date = NA) %>% 
  mutate(beat_letter = "C") %>%
  unite(beat,beat_letter, beat, sep = "") %>%
  select(date, year, period, type, beat, offense, race) %>%
  st_drop_geometry()

#Matching variable names to larger dataset  
SF2015brief$type[SF2015brief$type == "YES"]  <- "STOP WITH SEARCH OR FRISK"
SF2015brief$type[SF2015brief$type == "NO"]  <- "Search WITHOUT Stop-Frisk"

  
Stop_Frisk_Together <- rbind(sf, SF2015brief)%>%
  as_tibble() %>%
  arrange(year)
  

saveRDS(Stop_Frisk_Together, "data/sf_combined.Rds")


