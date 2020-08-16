#setup-----
library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(ggplot2)
library(ggmap)

setwd("/Users/enriqueunruh/Documents/Equity Center/GitHub/cvilleequity_stopandfrisk")

#2019 UPD Arrests Data -------

UPD_Arrests <- read_csv("data/UPD Data from Cav Daily/2019 Arrest Data Demographics.csv") %>%
  select(starts_with("Arrest"))

names(UPD_Arrests)[1] <- "Address"
names(UPD_Arrests)[2] <- "Crime_Description"
names(UPD_Arrests)[3] <- "Date"
names(UPD_Arrests)[4] <- "Arrest_Type"
names(UPD_Arrests)[5] <- "Age"
names(UPD_Arrests)[6] <- "Ethnicity"
names(UPD_Arrests)[7] <- "Race"
names(UPD_Arrests)[8] <- "Sex"

pal <- c("#EFAAC4","#4F4789","#87BFFF", "#95D7AE")


#UVA Population data -------

Undergrad_Pop <- data.frame(Race = c("Asian or Pacific Islander", "Hispanic", "Black",  "White"),		
                            values = c(2551, 1096, 1095, 9270)) %>%
  mutate(Percentage = paste0((round(values/sum(values), 4)*100), "%"),
         Group = "Undergrad_Pop",
         label_pos = 1 - (cumsum(values) - values/2)/sum(values))

Grad_Pop <- data.frame(Race = c("Asian or Pacific Islander", "Hispanic", "Black",  "White"),		
                            Population = c(331, 346, 325, 3817)) %>%
  mutate(Percentage = paste0((round(Population/sum(Population), 4)*100), "%"))

Staff_Pop <- data.frame(Race = c("Asian or Pacific Islander", "Hispanic", "Black",  "White"),		
                       Population = c(343, 163, 812, 5361)) %>%
  mutate(Percentage = paste0((round(Population/sum(Population), 4)*100), "%"))

Faculty_Pop <- data.frame(Race = c("Asian or Pacific Islander", "Hispanic", "Black",  "White"),		
                        Population = c(345, 80, 108, 2141)) %>%
  mutate(Percentage = paste0((round(Population/sum(Population), 4)*100), "%"))

UVA_Pop <- data.frame(Race = c("Asian or Pacific Islander", "Hispanic", "Black",  "White"),
                      values = c(2551+331+343+345, 1096+346+163+80, 1095+325+812+108, 9270+3817+5361+2141)) %>%
  mutate(Percentage = paste0((round(values/sum(values), 4)*100), "%"),
         Group = "UVA_Pop",
         label_pos = 1 - (cumsum(values) - values/2)/sum(values))

Cville_Pop <- data.frame(Race = c("Asian or Pacific Islander", "Hispanic", "Black",  "White"),
                      values = c(3404+47, 2741, 8697, 33417)) %>%
  mutate(Percentage = paste0((round(values/sum(values), 4)*100), "%"),
         Group = "UVA_Pop",
         label_pos = 1 - (cumsum(values) - values/2)/sum(values))

#Arrests by Race compared to total UVA Population ------

UPD_Arrests_ByRace <- UPD_Arrests  %>%
  transform(Race = ifelse(Ethnicity == "Hispanic Origin", "Hispanic", Race)) %>%
  filter(Race %in% c("Black", "White", "Hispanic", "Asian or Pacific Islander")) %>%
  group_by(Race) %>%
  summarise(values = n()) %>%
  transform(values = as.numeric(as.character(values)),
            Race = as.factor(Race)) %>%
 # mutate(#Total = (paste0((round(values/118, 4)*100), "%")),
  mutate(Percentage = paste0((round(values/108, 4))*100, "%"),
         Group = "ByRace/Ethnicity",
         label_pos = 1 - (cumsum(values) - values/2)/sum(values)) %>%
  arrange(desc(values)) %>%
  rbind(UVA_Pop)



ggplot(UPD_Arrests_ByRace,
       aes(x= Group, y = values, fill = factor(Race,
                                               levels = c("Asian or Pacific Islander", "Hispanic", "Black", "White")))) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = pal, name = "Race/Ethnicity") +
  coord_polar("y", direction = 1) +
  scale_x_discrete(limits = c(" ", "UVA_Pop","ByRace/Ethnicity")) +
  theme_void() +
  geom_text(data = filter(UPD_Arrests_ByRace, Race %in% c("Black","White")),
            aes(x = Group, y = label_pos,label = Percentage), size = 3.5, color = "white" ) #+
  geom_text(data = labellingdata,
          aes(x = group, y = pos, label = label) ,inherit.aes = FALSE, size = 3, color = "white")
 
#UPD Arrests of 18-22 Year old compared to Undergrad Population  -------
  
UPD_Arrests_ByRace_18_22 <- UPD_Arrests  %>%
  transform(Race = ifelse(Ethnicity == "Hispanic Origin", "Hispanic", Race)) %>%
  filter(Race %in% c("Black", "White", "Hispanic", "Asian or Pacific Islander")) %>%
  transform(Age = as.numeric(Age)) %>%
  filter(Age != is.na(Age)) %>%
  mutate(AgeGroup = ifelse(Age < 23, "18-22", 
                           ifelse((Age > 22 & Age < 33), "23-32", 
                                  ifelse((Age > 32 & Age < 55), "33-54", "55+")))) %>%
  filter(AgeGroup == "18-22") %>%
  group_by(Race) %>%
  summarise(values = n()) %>%
  transform(values = as.numeric(as.character(values)),
            Race = as.factor(Race)) %>%
  # mutate(#Total = (paste0((round(values/118, 4)*100), "%")),
  mutate(Percentage = paste0((round(values/38, 4))*100, "%"),
         Group = "ByRace/Ethnicity",
         label_pos = 1 - (cumsum(values) - values/2)/sum(values)) %>%
  arrange(desc(values)) %>%
  rbind(Undergrad_Pop)         


  
ggplot(UPD_Arrests_ByRace_18_22,
       aes(x= Group, y = values, fill = factor(Race,
                                               levels = c("Asian or Pacific Islander", "Hispanic", "Black", "White")))) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = pal, name = "Race/Ethnicity") +
  coord_polar("y", direction = 1) +
  scale_x_discrete(limits = c(" ", "Undergrad_Pop","ByRace/Ethnicity")) +
  theme_void() +
  geom_text(data = filter(UPD_Arrests_ByRace, Race %in% c("Black","White")),
            aes(x = Group, y = label_pos, label = Percentage), size = 3.5, color = "white" ) #+
geom_text(data = labellingdata,
          aes(x = group, y = pos, label = label) ,inherit.aes = FALSE, size = 3, color = "white")  


#UPD Arrests compared to Cville Population  -------
UPD_Arrests_ByRace_Cville <- UPD_Arrests  %>%
  transform(Race = ifelse(Ethnicity == "Hispanic Origin", "Hispanic", Race)) %>%
  filter(Race %in% c("Black", "White", "Hispanic", "Asian or Pacific Islander")) %>%
  group_by(Race) %>%
  summarise(values = n()) %>%
  transform(values = as.numeric(as.character(values)),
            Race = as.factor(Race)) %>%
  # mutate(#Total = (paste0((round(values/118, 4)*100), "%")),
  mutate(Percentage = paste0((round(values/108, 4))*100, "%"),
         Group = "ByRace/Ethnicity",
         label_pos = 1 - (cumsum(values) - values/2)/sum(values)) %>%
  arrange(desc(values)) %>%
  rbind(Cville_Pop)



ggplot(UPD_Arrests_ByRace_Cville,
       aes(x= Group, y = values, fill = factor(Race,
                                               levels = c("Asian or Pacific Islander", "Hispanic", "Black", "White")))) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = pal, name = "Race/Ethnicity") +
  coord_polar("y", direction = 1) +
  scale_x_discrete(limits = c(" ", "UVA_Pop","ByRace/Ethnicity")) +
  theme_void() +
  geom_text(data = filter(UPD_Arrests_ByRace, Race %in% c("Black","White")),
            aes(x = Group, y = label_pos,label = Percentage), size = 3.5, color = "white" ) #+
geom_text(data = labellingdata,
          aes(x = group, y = pos, label = label) ,inherit.aes = FALSE, size = 3, color = "white")


#Leaflet Arrests Map ---------

#register_google(key = "AIzaSyDOyaBrr_XOg72G-fqj-DRha29xWvjQeas", write = TRUE)

#UPD_Arrest_Geo <- UPD_Arrests %>%
 # filter(Crime_Description != is.na(Crime_Description)) %>%
 # select(Address) %>%
 # transform(Address = paste0(Address, ", Charlottesville, VA")) %>%
 # mutate_geocode(Address)

#write_csv(UPD_Arrest_Geo, path="data/UPD_Arrests_Geo.csv")

UPD_Arrest_Geo <- read_csv("data/UPD_Arrests_Geo.csv")

UPD_Arrest_Location <- UPD_Arrests %>%
  filter(Crime_Description != is.na(Crime_Description)) %>%
  add_column(lon = UPD_Arrest_Geo$lon, lat = UPD_Arrest_Geo$lat) %>%
  transform(Race = ifelse(Ethnicity == "Hispanic Origin", "Hispanic", Race)) %>%
  filter(Race %in% c("Black", "White", "Hispanic", "Asian or Pacific Islander")) %>%
  group_by(Address, lon, lat, Race) %>%
  summarise(Total = n())


UPD_Arrest_Map <- leaflet(data = UPD_Arrest_Location) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron",
                   options = providerTileOptions(minZoom = 13))   %>%
  addMapPane("polygons", zIndex = 410) %>%
  addMapPane("circlemarkers", zIndex = 420) 


UPD_race.df <- split(UPD_Arrest_Location, UPD_Arrest_Location$Race)

UPD_pal <- c("#EFAAC4","#87BFFF","#4F4789","#95D7AE")

UPD_racelevels <- names(UPD_race.df)
UPD_racepal <- colorFactor(palette = UPD_pal,
                           domain = UPD_racelevels)

names(UPD_race.df) %>%
  purrr::walk( function(df) {
    UPD_Arrest_Map <<- UPD_Arrest_Map %>%
      addCircleMarkers(
        data = UPD_race.df[[df]],
        color = ~UPD_racepal(df),
        weight = 0,
        radius = 3.5,
        fillOpacity = 1,
        group = df,
        label = ~htmlEscape(paste0("Total:", Total))
        
      )
    
  })


UPD_Arrest_Map <- UPD_Arrest_Map %>%
  addLayersControl(
    overlayGroups =  names(UPD_race.df),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend("bottomright", pal = UPD_racepal, values = names(UPD_race.df),
            title = "Race",
            opacity = 1
  ) 

UPD_Arrest_Map



UPD_Arrest_Location_2 <- UPD_Arrests %>%
  filter(Crime_Description != is.na(Crime_Description)) %>%
  mutate(Location = case_when(
    str_detect(Address, "UNIVERSITY|ELLIEWOOD") ~ "The Corner",
    str_detect(Address, "TREE HOUSE|ALDERMAN") ~ "New Dorms",
    str_detect(Address, "14TH") ~ "14th",
    str_detect(Address, "LEE|HOSPITAL|CRISPELL") ~ "Hospital",
    str_detect(Address, "HEREFORD|GOOCH|FLOYD") ~ "Runk Dorms Area",
    str_detect(Address, "IVY|LAMBETH|MASSIE") ~ "Near Lambeth",
    str_detect(Address, "MADISON|RUBGY|CULBRETH") ~ "Rugby/MadBowl",
    str_detect(Address, "MAIN|HOLLY") ~ "Main Street",
    str_detect(Address, "RUPPEL|STADIUM") ~ "JPA/Stadium",
    str_detect(Address, "MCCORMICK") ~ "McCormick",
    TRUE ~ "Other"
  )) %>%
  filter(Race != "Unknown") %>%
  filter(Location != "Hospital") %>%
  group_by(Race) %>%
  summarize(Totals = n()) %>%
  mutate(Proportion = Totals/sum(Totals))




UPD_Arrest_ByAge <- UPD_Arrests %>%
  transform(Age = as.numeric(Age)) %>%
  filter(Age != is.na(Age)) %>%
  mutate(AgeGroup = ifelse(Age < 23, "18-22", 
                           ifelse((Age > 22 & Age < 33), "23-32", 
                                  ifelse((Age > 32 & Age < 55), "33-54", "55+")))) %>%
  group_by(Race, AgeGroup) %>%
  #filter(AgeGroup != "18-22" & AgeGroup != "23-32") %>%
  summarise(AgeTotals = n()) %>%
  transform(AgeTotals = as.numeric(as.character(AgeTotals))) %>%
  mutate(Proportion = AgeTotals/117)

UPD_Arrest_ByCrime <- UPD_Arrests %>%
  filter(Crime_Description != is.na(Crime_Description)) %>%
  mutate(Crime = case_when(
    str_detect(Crime_Description, "ASSAULT|ROBBERY|PURSE|Shots|Weapon|EMBEZZ|IDENTITY") ~ "Person Crime",
    str_detect(Crime_Description, "Burglary|LARCENY|TRESPASS|VANDAL|MONUMENT|VANDAL") ~ "Property Crime",
    str_detect(Crime_Description, "NARCOTICS|Marijuana") ~ "Narcotics Related",
    str_detect(Crime_Description, "Traffic|INFLUENCE") ~ "Traffic Related",
    str_detect(Crime_Description, "DRUNK|Alcohol|INTOXICATION") ~ "Alcohol Related",
    str_detect(Crime_Description, "Suspicious|Supsicious") ~ "Suspicious Circumstance",
    TRUE ~ "Other"
  )) %>%
  filter(Race != "Unknown") %>%
  group_by(Race, Arrest_Type, Crime) %>%
  filter(Crime == "Alcohol Related") %>%
  summarize(Totals = n())
  
  


UPD_Arrest_ByType <- UPD_Arrests %>%
  group_by(Race, Arrest_Type) %>%
  #filter(Arrest_Type %in% c("On View", "Taken Into Custody")) %>%
  filter(Race %in% c("Black", "White", "Asian or Pacific Islander")) %>%
  summarize(Total = n()) %>%
  mutate(Proportion = Total/sum(Total))







# Traffic Stops ---------

UPD_Traffic_Stops <- read_csv("data/UPD Data from Cav Daily/2019 Traffic Warning Demographics (1).csv") 

names(UPD_Traffic_Stops)[4] <- "Reason"
names(UPD_Traffic_Stops)[6] <- "Search"

UPD_Traffic_Stops_BySearch <- UPD_Traffic_Stops %>%
  group_by(Search) %>%
  filter(Search %in% c("Yes", "No")) %>%
  summarize(Total = n())

UPD_Traffic_Stops_ByReason <- UPD_Traffic_Stops %>%
  group_by(Reason) %>%
  summarize(Total = n())

UPD_Traffic_Stops_ByRace <- UPD_Traffic_Stops %>%
  group_by(Race) %>%
  summarize(Total = n())



