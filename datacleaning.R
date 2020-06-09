################
# Stop & Frisk #
################
library(tidyverse)
library(openxlsx)
library(ggmap)
library(pdftools)


setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/cvilleequity_stopandfrisk")

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


## Geocode with Google Maps ##

google_geo_codes <- geocode(SF2017$ADDRESS)




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



