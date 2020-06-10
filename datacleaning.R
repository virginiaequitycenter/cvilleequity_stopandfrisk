################
# Stop & Frisk #
################
library(tidyverse)
library(openxlsx)
library(ggmap)
library(pdftools)
library(sf)
library(leaflet)
library(htmltools)


setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/cvilleequity_stopandfrisk")

addresses <- read_csv("data/Master_Address_Points.csv")
addresses_table <- read_csv("data/Master_Address_Table.csv")


####### 2019-2020 #######
SF1920input <- read_csv("data/SF20192020Input.csv")

SF1920 <-
SF1920input %>%
  gather(Month, Number, - c(beat, beatnum, race)) %>%
  separate(Month, c("Month", "Year"), sep = "-") %>%
  mutate(Year = as.numeric(paste0(20, Year))) %>%
  filter(race %in% c("B", "W"))

write_csv(SF1920, path = "data/finaldata/SF1920.csv")

##### 2017 ########
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
          ) %>%
  filter(RACE %in% c("B", "W"))

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



## Try to get at the beats ##
beats <- st_read("data/Police_Neighborhood_Area-shp/Police_Neighborhood_Area.shp")
beats

SF2017Spatial <-
SF2017Locations %>%
  st_as_sf(
    coords = c("lon", "lat"),
 #   agr = "constant",
    crs = 4326,
#    stringAsFactors = FALSE,
    remove = TRUE
  )

point_in_beat <- st_join(SF2017Spatial, beats, join = st_within)

View(point_in_beat)

## Print them out ##
SF2017FINAL <-
point_in_beat %>%
  ungroup() %>%
  select(SFTYPE, NUMBER, STREET, OFFENSE, RACE, geometry, BEAT_NO, POPULATION)
  

write_csv(SF2017FINAL , path = "data/finaldata/SF2017.csv")

####
leaflet(data = beats) %>%
  addProviderTiles("Stamen.Toner") %>%
  addPolygons(
    weight = 2,
    fillOpacity = .1
  ) %>%
  addCircleMarkers(
    data = SF2017FINAL,
    color = "red",
    weight = 0,
    radius = 3,
    fillOpacity = .5,
    label = ~htmlEscape(paste0(NUMBER," ", STREET))
    
  )

######## 2016 ########
## Clean SF 2016 ##
SF2016WithSF <- read_csv("./data/2016withSF.csv") %>%
  mutate(SFTYPE = "STOP WITH SEARCH OR FRISK",
         Date = as.Date(Date, "%m/%d/%y"))  

SF2016WithoutSF <- read_csv("./data/2016withoutSF.csv") %>%
  mutate(SFTYPE = "Search WITHOUT Stop-Frisk",
         Date = as.Date(Date, "%m/%d/%y"))  

SF2016 <-
  SF2016WithSF %>% bind_rows(SF2016WithoutSF)  %>%
  filter(RACE %in% c("B", "W"))

SF2016 %>%
  write_csv(. , path = "data/finaldata/SF2016.csv")


###### 2014 #######
## Clean SF 2014 ##
SF2014WithSF <- read_csv("data/2014withSF.csv") %>% 
  mutate(SFTYPE = "STOP WITH SEARCH OR FRISK",
         Date = as.Date(Date, "%m/%d/%y"))  
View(SF2014WithSF)
SF2014WithoutSF <- read_csv("data/2014withoutSF.csv") %>% 
  mutate(SFTYPE = "Search WITHOUT Stop-Frisk",
         Date = as.Date(Date, "%m/%d/%y"))  

SF2014 <-
SF2014WithSF %>% bind_rows(SF2014WithoutSF) %>%
    mutate(Address = str_replace_all(Address, "  ", " ")) %>%
    mutate(Address = str_replace_all(Address, "  ", " ")) %>%
    mutate(Address = str_replace_all(Address, "  ", " ")) %>%
  filter(RACE %in% c("B", "W"))

SF2014 %>%
  write_csv(. , path = "data/finaldata/SF2014.csv")




