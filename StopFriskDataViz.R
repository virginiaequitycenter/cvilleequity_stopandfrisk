#setup
library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(RColorBrewer)
library(rmdformats)
library(ggplot2)
library(networkD3)
library(dplyr)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(geojsonio)


setwd("/Users/enriqueunruh/Documents/Equity Center/GitHub/cvilleequity_stopandfrisk")

#Data Files
sf <- readRDS("data/sf_combined.Rds")

sf_arrests <- readRDS("data/sf_arrest.Rds")

sflocations <- read_csv("data/sflocations.csv") %>% st_as_sf(
  coords = c("lon", "lat"),
  crs = 4326,
  remove = TRUE
) %>%
  filter(!Year %in% c(2019, 2020))

beats <- readRDS("data/beat_pop_map.Rds")


pop <- data.frame("race" = c("White", "Black", "Asian", "Multiracial", "Remaining"),
                  "value" = c(32565, 8782, 3370, 1443, 327)) %>%
  mutate(race = factor(race, levels = c("Black", "White", "Multiracial", "Asian", "Remaining")),
         group = "pop") %>%
  mutate(race = fct_collapse(race,
                             Remaining = c("Multiracial", "Asian", "Remaining"))) %>%
  group_by(race) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>% mutate(group = "pop") %>%
  filter(race %in% c("Black", "White")) %>%
  droplevels()

pallete <- c("#902D41" ,"#613DC1", "#357266")



#Summary Macro Data--------

sf_summary_stats <- sf %>%
  group_by(race) %>%
  summarize(Totals = n()) %>%
  filter(race %in% c("B", "W"))

sf_summary_stats$race[sf_summary_stats$race == "B"] <- "Black"
sf_summary_stats$race[sf_summary_stats$race == "W"] <- "White"

sf_summary_stats <- sf_summary_stats%>%
  mutate(prop = Totals / sum(sf_summary_stats$Totals),
         ymax = cumsum(prop), ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2) %>%
  left_join(pop) %>%
  mutate(Per1000 = round((Totals/value) *1000),1)


#Total Detentions ---------
ggplot(sf_summary_stats, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=race)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y", direction = 1) +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

ggplot(sf_summary_stats, aes(x = race, y = Totals, fill = race)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip() +
  scale_fill_manual(values = pal2) +
  geom_text(aes(y = Totals - 10, label = Totals), size=7, color = "white", hjust = 1) +
  ggtitle("Detentions by Charlottesville Police Officers from 2012-2017") +
  theme_void() +
  theme(legend.position = "none", 
        plot.title = element_text(size=15, face=4, hjust = .5),
        axis.text.y = element_text(size=15, face=1, hjust = .5))


#Per1000 -------
All <- data_frame(race = "All", Totals = sum(sf_summary_stats$Totals), 
                  value = sum(sf_summary_stats$value), 
                  Per1000 = "")
 
sf_Per1000 <- sf_summary_stats %>%
  select(race, Totals, value, Per1000) %>%
  rbind(All) %>%
  transform(Per1000 = round((Totals/value) *1000),1) %>%
  arrange(Per1000) %>%
  as_tibble()

sf_Per1000 <- sf_Per1000 %>%
  transform(race = fct_reorder(race, desc(Per1000))) %>%
  as_data_frame()

ggplot(sf_Per1000, aes(x = race, y = Per1000, fill = race)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip() +
  scale_fill_manual(values = pallete) +
  geom_text(aes(y = Per1000 - 1, label = Per1000), size=7, color = "white", hjust = 1) +
  ggtitle("Detentions by Charlottesville police officers per 1,000 people") +
  theme_void() +
  theme(legend.position = "none", 
        plot.title = element_text(size=15, face=4, hjust = .5),
        axis.text.y = element_text(size=15, face=1, hjust = .5))



#Detentions Relative to Population --------

sf_race <- sf %>%
  filter(race %in% c("B", "W")) %>%
  mutate(race = factor(race),
         race = fct_recode(race,
                           Black = "B",
                           White = "W"),
         race = fct_relevel(race, "White"),
         type2 = factor(type),
         type2 = fct_recode(type2,
                            "Search/Frisk" = "STOP WITH SEARCH OR FRISK",
                            "No Search/Frisk" = "Search WITHOUT Stop-Frisk"))

sf_race_all <- sf_race %>%
  group_by(race) %>%
  summarize(value = n()) %>%
  ungroup() %>%
  mutate(group = "total")

sf_race_all <- rbind(sf_race_all, pop2) %>%
  mutate(group = factor(group, levels = c("pop", "total")),
         race = fct_relevel(race, "White")) %>%
  arrange(group, desc(race)) %>%
  group_by(group) %>%
  mutate(label_pos = (cumsum(value) - value/2)/sum(value), 
         percent = paste0(round(value/sum(value)*100,1),"%")
  )

labellingdata <- data.frame(label = c("Population", "Detentions"), group = c("pop", "total"), pos = c(.90,.90) )


ggplot(sf_race_all, aes(x = group, y = value, fill = race)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = pal2, name = "Race") +
  coord_polar("y", direction = 1) +
  scale_x_discrete(limits = c(" ", "pop","total")) +
  theme_void() +
  geom_text(data = filter(sf_race_all, race == "Black"), aes(x = group, y = label_pos, label = percent), size = 3, color = "white") +
  geom_text(data = labellingdata,
             aes(x = group, y = pos, label = label) ,inherit.aes = FALSE, size = 3, color = "white") 



#Detentions by Race by Year -------
tots <- sf_race %>% group_by(year, type2) %>% summarize(tots = n())
racetots <- sf_race %>% group_by(year, type2, race) %>% summarize(racetot = n())
racetots <- racetots %>% left_join(tots)
racetots <- racetots %>% mutate(percent = round((racetot/tots)*100,0),
                                percent_lab = paste0(percent, "%"),
                                race = fct_relevel(race, "White")) %>%
  group_by(year, type2) %>%
  arrange(year, type2, race) %>%
  mutate(lab_height =  racetot/2)





#By Year
sf_race_year <- sf_race %>%
  group_by(year, race) %>%
  summarize(value = n()) %>%
  ungroup() %>%
  mutate(group = "")


pop2_p1 <- pop2 %>%
  mutate(period = "2012-2014")
pop2_p2 <- pop2 %>%
  mutate(period = "2016-2017")

pop2_year <- pop2 %>%
  mutate(year = "Total")

sf_race_year <- rbind(sf_race_year, pop2_year) %>%
  mutate(group = c(c(rep("year",12), rep("pop",2))),
         race = fct_relevel(race, "White"))%>%
  arrange(group, year, desc(race)) %>%
  group_by(group, year) %>%
  mutate(label_pos = (cumsum(value) - value/2)/sum(value), 
         percent = paste0(round(value/sum(value)*100,1),"%")
  )

sf_race_period <- rbind(sf_race_period, pop2_p1, pop2_p2) %>%
  mutate(group = factor(group, levels = c("pop", "period")),
         race = fct_relevel(race, "White")) %>%
  arrange(group, period, desc(race)) %>%
  group_by(group, period) %>%
  mutate(label_pos = (cumsum(value) - value/2)/sum(value), 
         percent = paste0(round(value/sum(value)*100,1),"%")
  )

labellingdata <- data.frame (label = c(rep("Population",6), rep("Detentions",6)), 
                             group = c(rep("pop",6), rep("year",6)), pos = rep(.90, 12), year = rep(seq(2012,2017),2))



ggplot(sf_race_year, aes(x = year, y = value, fill = race)) +
  geom_bar(position="fill", stat = "identity") +
  scale_fill_manual(values = pal2, name = "Race") +
  coord_polar("y", direction = 1)  +
  scale_x_discrete(limits = c(" ", "Total",seq(2012,2017))) +
  theme_void() +
  geom_text(data = filter(sf_race_period, race == "Black"), 
            aes(x = year, y = label_pos, label = percent), 
            size = 3) +
  facet_wrap(~ year) +
  geom_text(data = labellingdata,
            aes(x = year, y = pos, label = label),
            inherit.aes = FALSE, size = 3)

sf_race_2012 <- sf_race_year %>%
  filter(year %in% c("Total", "2012"))

ggplot(sf_race_2012, aes(x = group, y = value, fill = race)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = pal2, name = "Race") +
  coord_polar("y", direction = 1) +
  scale_x_discrete(limits = c(" ", "pop","year")) +
  theme_void() +
  geom_text(data = filter(sf_race_2012, race == "Black"), 
            aes(x = group, y = label_pos, label = percent), size = 3, color = "white") +
  geom_text(data = labellingdata,
            aes(x = group, y = pos, label = label) ,inherit.aes = FALSE, size = 3, color = "white") 


#By year extra
scale_x_discrete(limits = c(" ", "Total",seq(2012,2017))) +
  +
  geom_text(data = filter(sf_race_period, race == "Black"), 
            aes(x = year, y = label_pos, label = percent), 
            size = 3) +
  +
  geom_text(data = labellingdata,
            aes(x = year, y = pos, label = label),
            inherit.aes = FALSE, size = 3)





#Stop/Frisk and no Stop/Frisk--------


ggplot(sf_race, aes(x = type2, fill = race)) +
  geom_bar(position = "dodge") + 
  facet_wrap(~year) +
  scale_fill_manual(values = pal2) +
  labs(fill = "Race") +
  theme(axis.title = element_blank()) +
  geom_text(data = arrange(racetots, race), 
            aes(x = type2, y = lab_height, fill = race, label = percent_lab), 
            position = position_dodge(width = .85), size = 3, hjust = .5) +
  geom_text(data = arrange(racetots, race),
            aes(x = type2, y = racetot + 8, fill = race, label = racetot), 
            position = position_dodge(width = .85), size = 4, hjust = .5)


# Old Stop and Frisk Vis--------
sf_race_type <- sf_race %>%
  group_by(type2, race) %>%
  summarize(value = n()) %>%
  ungroup() %>%
  mutate(group = "type") %>%
  rename(type = type2)

pop2_t1 <- pop2 %>%
  mutate(type = "No Search/Frisk")
pop2_t2 <- pop2 %>%
  mutate(type = "Search/Frisk")

sf_race_type <- rbind(sf_race_type, pop2_t1, pop2_t2) %>%
  mutate(group = factor(group, levels = c("pop", "type")),
         race = fct_relevel(race, "White")) %>%
  arrange(group, type, desc(race)) %>%
  group_by(group, type) %>%
  mutate(label_pos = (cumsum(value) - value/2)/sum(value), 
         percent = paste0(round(value/sum(value)*100,1),"%")) 

labellingdata <- data.frame(label = rep(c("Population", "Detentions"),2), group = rep(c("pop", "type"),2), pos = rep(c(.90,.90),2), type = rep(c("No Search/Frisk", "Search/Frisk"), each = 2 ))

ggplot(sf_race_type, aes(x = group, y = value, fill = race)) +
  geom_bar(position="fill", stat = "identity") +
  scale_fill_manual(values = pal2, name = "Race") +
  coord_polar("y", direction = 1) +
  scale_x_discrete(limits = c(" ", "pop","type")) +
  geom_text(  data = filter(sf_race_type, race == "Black"), aes(x = group, y = label_pos, label = percent), size = 3) +
  geom_text( data = labellingdata,
             aes(x = group, y = pos, label = label) ,inherit.aes = FALSE, size = 3) +
  theme_void() +
  facet_wrap(~type) 

  

#Stop and Frisk Sankey Diagram -----


#Total

sf_race_sfs <- sf_race %>%
  group_by(race, type2) %>%
  summarize(values = n())

sf_nodes <- data.frame(
  name=c(as.character(sf_race_sfs$race), 
         as.character(sf_race_sfs$type2)) %>%
    unique()
)

sf_race_sfs <- sf_race_sfs %>%
  mutate(IDrace = match(race, sf_nodes$name)-1,
         IDtype2 = match(type2, sf_nodes$name)-1)


sf_race_sfs$group <- as.factor(c("a", "a", "b", "b"))

sf_nodes$group <- as.factor(c("unique_group"))

sf_sankey_color <- 'd3.scaleOrdinal() .domain(["a", "b", "unique_group"]) .range(["#95D7AE", "#87BFFF", "grey"])'

# Make the Network
sf_sankeyNetwork <- sankeyNetwork(Links = sf_race_sfs, Nodes = sf_nodes,
                                  Source = "IDrace", Target = "IDtype2",
                                  Value = "values", NodeID = "name", 
                                  colourScale=sf_sankey_color, LinkGroup="group", NodeGroup="group",
                                  fontSize = 20, nodeWidth=10, units = "Detentions")


sf_sankeyNetwork


#By Year

sf_race_sfs_year <- sf_race %>%
  group_by(race, year) %>%
  summarize(values = n())


sf_race_for_nodes <- sf_race %>%
  group_by(race, year, type2) %>%
  summarize(values = n())

sf_nodes_year <- data.frame(
  name=c(as.character(sf_race_for_nodes$race),
         as.character(sf_race_for_nodes$year), 
         as.character(sf_race_for_nodes$type2)) %>%
    unique()
)

sf_race_sfs_type <- sf_race %>%
  group_by(race,year, type2) %>%
  summarize(values = n()) %>%
  transform(year = as.factor(year)) %>%
  select(source = year, target = type2, values)


sf_race_sfs_year <- sf_race_sfs_year %>%
  rename(source = race, target = year) %>%
  transform(source = as.factor(source)) %>%
  rbind(sf_race_sfs_type)


sf_race_sfs_year <- sf_race_sfs_year %>%
  mutate(IDsource = match(source, sf_nodes_year$name -1),
         IDtarget = match(target, sf_nodes_year$name)-1) %>%
  transform(IDsource =  c(rep("0", 6), rep("1", 6), rep("2", 2), rep("3", 2),
                          rep("4", 1), rep("5", 2), rep("6", 2), rep("7", 2),
                          rep("2", 2), rep("3", 2), rep("4", 2), rep("5", 2), 
                          rep("6", 2), rep("7", 2))
            ) %>%
  transform(IDsource = as.numeric(IDsource))
  

sf_race_sfs_year$group <- as.factor(c(rep("a", 6),
                                      rep("b", 6),
                                      rep("a", 11),
                                      rep("b", 12)))

sf_nodes_year$group <- as.factor(c("unique_group"))


sf_sankey_color <- 'd3.scaleOrdinal() .domain(["a", "b", "unique_group"]) .range(["#95D7AE", "#87BFFF", "grey"])'

# Make the Network
sf_sankeyNetwork_year <- sankeyNetwork(Links = sf_race_sfs_year, Nodes = sf_nodes_year,
                                  Source = "IDsource", Target = "IDtarget",
                                  Value = "values", NodeID = "name", 
                                  colourScale=sf_sankey_color, LinkGroup="group", NodeGroup="group",
                                  fontSize = 10, nodeWidth=10, units = "Detentions")


sf_sankeyNetwork_year


#Black v. White Stop and Frisk

sfs_race_black <- sf_race_sfs_year %>%
  filter(source != "White", 
         group == "b") %>%
  transform(IDsource = IDsource-1,
            IDtarget = IDtarget-1,
            group = ifelse(source == "Black", "a", 
                           ifelse(target == "No Search/Frisk", "b", "c")))

sf_for_nodes_black <- sf_race %>%
  group_by(race, year, type2) %>%
  filter(race != "White") %>%
  summarize(values = n()) 

sf_nodes_black <- data.frame(
  name=c(as.character(sf_for_nodes_black$race),
         as.character(sf_for_nodes_black$year), 
         as.character(sf_for_nodes_black$type2)) %>%
    unique()
)

sf_nodes_black$group <- as.factor(c("unique_group"))

sf_sankey_color <- 'd3.scaleOrdinal() .domain(["a", "b", "unique_group"]) .range(["#87BFFF", "#E9B44C", "grey", "#9D6381"])'

# Make the Network
sf_sankeyNetwork_black <- sankeyNetwork(Links = sfs_race_black, Nodes = sf_nodes_black,
                                       Source = "IDsource", Target = "IDtarget",
                                       Value = "values", NodeID = "name", 
                                       colourScale=sf_sankey_color, LinkGroup="group", NodeGroup="group",
                                       fontSize = 10, nodeWidth=10, units = "Detentions")


sf_sankeyNetwork_black

#White
sfs_race_white <- sf_race_sfs_year %>%
  filter(source != "Black", 
         group == "a") %>%
  transform(IDsource = as.numeric(IDsource - 1),
            IDtarget = IDtarget-1,
            group = ifelse(source == "White", "a", 
                           ifelse(target == "No Search/Frisk", "b", "c")),
            IDsource = ifelse(IDsource < 0, 0, IDsource))

sf_for_nodes_white <- sf_race %>%
  group_by(race, year, type2) %>%
  filter(race != "Black") %>%
  summarize(values = n()) 

sf_nodes_white <- data.frame(
  name=c(as.character(sf_for_nodes_white$race),
         as.character(sf_for_nodes_white$year), 
         as.character(sf_for_nodes_white$type2)) %>%
    unique()
)

sf_nodes_white$group <- as.factor(c("unique_group"))

sf_sankey_color <- 'd3.scaleOrdinal() .domain(["a", "b", "unique_group"]) .range(["#95D7AE", "#E9B44C", "grey", "#9D6381"])'

# Make the Network
sf_sankeyNetwork_white <- sankeyNetwork(Links = sfs_race_black, Nodes = sf_nodes_black,
                                        Source = "IDsource", Target = "IDtarget",
                                        Value = "values", NodeID = "name", 
                                        colourScale=sf_sankey_color, LinkGroup="group", NodeGroup="group",
                                        fontSize = 10, nodeWidth=10, units = "Detentions")


sf_sankeyNetwork_white


##Arrest Numbers -------
sf_arrests_sankey <- sf_arrests %>%
  filter(!is.na(arrest)) %>%
  filter(race %in% c("B", "W")) %>%
  transform(arrest = str_replace_all(arrest, "no", "No"))  %>%
  transform(arrest = str_replace_all(arrest, "NO", "No")) %>%
  transform(type = ifelse(type == "Search WITHOUT Stop-Frisk", "No Search/Frisk", "Search/Frisk")) %>%
  group_by(race, type) %>%
  summarize(values = n())  %>% 
  transform(values = as.numeric(values)) %>%
  select(source = race, target = type, values)

sf_type_arrests <- sf_arrests %>%
  filter(!is.na(arrest)) %>%
  filter(race %in% c("B", "W")) %>%
  transform(arrest = str_replace_all(arrest, "no", "No"))  %>%
  transform(arrest = str_replace_all(arrest, "NO", "No")) %>%
  transform(arrest = ifelse(arrest == "No", "No", "Arrested"),
            type = ifelse(type == "Search WITHOUT Stop-Frisk", "No Search/Frisk", "Search/Frisk")) %>%
  group_by(race, type, arrest) %>%
  summarize(values = n()) %>%
  transform(type = as.character(type),
            values = as.numeric(values),
            arrest = str_replace_all(arrest, "No", "Not Arrested")) %>%
  select(source = type, target = arrest, values)


sf_arrests_sankey <- sf_arrests_sankey %>%
  rbind(sf_type_arrests) %>%
  transform(source = str_replace_all(source, "B", "Black"))  %>%
  transform(source = str_replace_all(source, "W", "White"))

sf_arrests_nodes <- data.frame(
  name=c(as.character(sf_arrests_sankey$source), 
         as.character(sf_arrests_sankey$target)) %>%
    unique()
)


sf_arrests_sankey <- sf_arrests_sankey %>%
  mutate(IDSource = match(source, sf_arrests_nodes$name)-1,
         IDTarget = match(target, sf_arrests_nodes$name)-1)


sf_arrests_sankey$group <- as.factor(c(rep(c("a", "a", "b", "b")), rep("a", 4), rep("b", 4)))


sf_arrests_nodes$group <- as.factor(c("unique_group"))

sf_sankey_color <- 'd3.scaleOrdinal() .domain(["a", "b", "unique_group"]) .range([ "#87BFFF", "#95D7AE", "grey"])'

# Make the Network
sf_sankeyNetwork_arrests <- sankeyNetwork(Links = sf_arrests_sankey, Nodes = sf_arrests_nodes,
                                  Source = "IDSource", Target = "IDTarget", sinksRight=FALSE,
                                  Value = "values", NodeID = "name", 
                                  colourScale=sf_sankey_color, LinkGroup="group", NodeGroup="group",
                                  fontSize = 20, nodeWidth=10, units = "Detentions")


sf_sankeyNetwork_arrests
  

##Arrest Summaries
sf_arrests_summaries <- sf_arrests %>%
  filter(race %in% c("B", "W")) %>%
  transform(arrest = str_replace_all(arrest, "no", "No"))  %>%
  transform(arrest = str_replace_all(arrest, "NO", "No")) %>%
  transform(arrest = ifelse(arrest == "No", "No", "Arrested"),
            type = ifelse(type == "Search WITHOUT Stop-Frisk", "No Search/Frisk", "Search/Frisk"))%>%
  transform(arrest = ifelse(is.na(arrest), "No", arrest)) %>%
  mutate(offense7 = case_when(
    str_detect(offense, "Assault|Robbery|PURSE|Shots|Weapon") ~ "Person Crime",
    str_detect(offense, "Burglary|Larceny|Trespass|VANDAL") ~ "Property Crime",
    str_detect(offense, "Narcotic") ~ "Narcotics Related",
    str_detect(offense, "Traffic") ~ "Traffic Related",
    str_detect(offense, "Disorder|Drunk|Liquor|INDECENT|DIP") ~ "Public Disorder",
    str_detect(offense, "Suspicious|Supsicious") ~ "Suspicious Circumstance",
    TRUE ~ "Other"
  )) %>%
  group_by(type, arrest) %>%
  filter(arrest == "No") %>%
  summarize(values = n()) %>%
  mutate(PercentArrests = values/451)

sf_arrests_summons <- sf_arrests %>%
  transform(arrest = ifelse(is.na(arrest), "No", arrest)) %>%
  filter(race %in% c("B", "W")) %>%
  mutate(arrests = case_when(
    str_detect(arrest, "NO|no|No") ~ "Not Arrested",
    str_detect(arrest, "Summons|Warning") ~ "Summoned/Warned",
    TRUE ~ "Arrested"
  )) %>%
  transform(type = ifelse(type == "Search WITHOUT Stop-Frisk", "No Search/Frisk", "Search/Frisk"))%>%
  mutate(offense7 = case_when(
    str_detect(offense, "Assault|Robbery|PURSE|Shots|Weapon") ~ "Person Crime",
    str_detect(offense, "Burglary|Larceny|Trespass|VANDAL") ~ "Property Crime",
    str_detect(offense, "Narcotic") ~ "Narcotics Related",
    str_detect(offense, "Traffic") ~ "Traffic Related",
    str_detect(offense, "Disorder|Drunk|Liquor|INDECENT|DIP") ~ "Public Disorder",
    str_detect(offense, "Suspicious|Supsicious") ~ "Suspicious Circumstance",
    TRUE ~ "Other"
  )) %>%
  group_by(race, arrests) %>%
  summarize(values = n()) #%>%
  mutate(PercentArrests = values/451)




#Map ------

beats_data <- 
  beats  %>% 
  mutate(`Percent White` = round(whitepopE/totalpopE*100,2),
         `Percent Black` = round(blackpopE/totalpopE*100,2)
  ) %>%
  mutate(popup = str_c("<strong>", NAME, "</strong>",
                       "<br/>",
                       "Percent White: ", paste0(`Percent White`, "%")) %>%
           map(htmltools::HTML))

colorswhite <- colorRampPalette(brewer.pal(9, "Greens"))(9)[2:6]
whitepal <- colorBin(palette = colorswhite, domain = c(0,100), bins = 5)

colorsblack<- colorRampPalette(brewer.pal(9, "Purples"))(9)[2:6]
blackpal <- colorBin(palette = colorsblack, domain = c(0,100), bins = 5)


bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
pal <- colorBin(colorswhite, domain = beats_data$`Percent White`, bins = bins)

leaflet(data = beats_data) %>%
  addTiles() %>%
  addPolygons(label = ~`popup`,
              fillColor = ~whitepal(`Percent White`),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE)) 



l <-NULL



race.beats <- list(
  `Data Off` = beats_data %>% mutate(color = 0, df = "Data Off"),
  `Percent White` = beats_data %>% mutate(color = `Percent White`, df = "Percent White"),
  `Percent Black` = beats_data %>% mutate(color = `Percent Black`, df = "Percent Black")
)

beatscolorfunction <- function(listname, color){
  case_when(
    listname == names(race.beats)[2] ~   whitepal(color),
    listname == names(race.beats)[3] ~   blackpal(color),
    TRUE ~ "DCF0EF"
  )
}

beatsopacityfunction <- function(listname){
  case_when(
    listname == names(race.beats)[2] ~   .5,
    listname == names(race.beats)[3] ~  .5,
    TRUE ~ 0.01
  )
}

l <- leaflet() %>%
  addProviderTiles("Stamen.Toner",
                   options = providerTileOptions(minZoom = 13))   %>%
  addMapPane("polygons", zIndex = 410) %>%
  addMapPane("circlemarkers", zIndex = 420)  


names(race.beats) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addPolygons(
        data = race.beats[[df]],
        weight = 3,
        fillOpacity = ~beatsopacityfunction(df),
        color = ~beatscolorfunction(df, color),
        group = df   ,
        smoothFactor = 0.5,
        label = ~htmlEscape(paste0(NAME,  ifelse(color == 0, "",
                                                 ifelse(df  == "Percent White", paste0(": ", color, "% White"), paste0(": ", color, "% Black") )  )))
      )
  })

## Add Race Dot Colors ## 

sflocationsmap <- 
  sflocations %>%
  mutate(RACE = ifelse(RACE == "W", "White", "Black"))

race.df <- split(sflocationsmap, sflocationsmap$RACE)
race.colors <- data.frame()

#mycolors <- c("#6969B3", "#00AF98")
mycolors <- c("#87BFFF", "#95D7AE")

racelevels <- names(race.df)
racepal <- colorFactor(palette = mycolors,
                       domain = racelevels)

names(race.df) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addCircleMarkers(
        data = race.df[[df]],
        color = ~racepal(df),
        weight = 0,
        radius = 2.5,
        fillOpacity = 1,
        group = df,
        label = ~htmlEscape(paste0("Offense: ", OFFENSE))
        
      )
    
  })


sf.df <- split(sflocationsmap, sflocationsmap$SFTYPE)
sf.race.df   <- split(sf.df[["STOP WITH SEARCH OR FRISK"]], sf.df[["STOP WITH SEARCH OR FRISK"]]$RACE)

names(sf.race.df) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addCircleMarkers(
        data = sf.race.df[[df]],
        color =  "red",
        weight = 1,
        radius = 6,
        fill = FALSE,
        group = df
      )
  })



html_legend <- "<svg width = '10' height = '10'> <circle cx='5' cy='5' r='5' stroke='red' fill = 'white' stroke-width = '1' /></svg> = Stop & Frisk"

html_slider <- "L.control.slider(function(names(race.b) {console.log(value);}, {id:slider, 'vertical'});"

l %>%
  addLayersControl(
    baseGroups =  names(race.beats),
    overlayGroups =  c(names(race.df)),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
    }") %>%
  htmlwidgets::onRender("<script
  src='https://code.jquery.com/jquery-3.3.1.min.js'
  integrity='sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8='
  crossorigin='anonymous'></script>
  <link rel='stylesheet' href='https://unpkg.com/leaflet@1.4.0/dist/leaflet.css'
  integrity='sha512-puBpdR0798OZvTTbP4A8Ix/l+A4dHDD0DGqYW6RQ+9jxkRFclaxxQb/SJAWZfWAkuyeQUytO7+7N4QKrDh+drA=='
  crossorigin=''/>
  <script src='https://unpkg.com/leaflet@1.4.0/dist/leaflet.js'
  integrity='sha512-QVftwZFqvtRNi0ZyCtsznlKSWOStnDORoefr1enyq5mVL4tmKB3S/EnC3rRJcxCPavG10IcrVGSmPh6Qw5lwrg=='
  crossorigin=''></script>
  <script src='leaflet-timeline-slider.min.js'></script>
  var mymap = this;
  mymap.control.timelineSlider({
 
  timelineItems: ['Day 1', 'The Next Day', 'Amazing Event', '1776', '12/22/63', '1984'],
  extraChangeMapParams: {greeting: 'Hello World!'}, 
  changeMap: changeMapFunction })
  .addTo(mymap);") %>%
  addLegend("bottomleft", pal = racepal, values = names(race.df),
            title = "Race",
            opacity = 1
  ) #%>%
  addControl(html = html_legend, position = "bottomright")

  
  #Build data.frame with 10 obs + 3 cols
  power <- data.frame(
    "Latitude" = c(33.515556, 38.060556, 47.903056, 49.71, 49.041667, 31.934167, 54.140586, 54.140586, 48.494444, 48.494444),
    "Longitude" = c(129.837222, -77.789444, 7.563056, 8.415278, 9.175, -82.343889, 13.664422, 13.664422, 17.681944, 17.681944),
    "start" = do.call(
      "as.Date",
      list(
        x = c("15-Sep-1971", "1-Dec-1971", "1-Feb-1972", "1-Feb-1972", "1-Feb-1972", "1-Feb-1972", "1-Apr-1972", "1-Apr-1972", "24-Apr-1972", "24-Apr-1972"),
        format = "%d-%b-%Y"
      )
    )
  )
  
  # set start same as end
  #  adjust however you would like
  power$end <- power$start
  
  
  # use geojsonio to convert our data.frame
  #  to GeoJSON which timeline expects
  power_geo <- geojson_json(power,lat="Latitude",lon="Longitude")
  
  # create a leaflet map on which we will build
  leaf <- leaflet() %>%
    addTiles()
  
  # add leaflet-timeline as a dependency
  #  to get the js and css
  leaf$dependencies[[length(leaf$dependencies)+1]] <- htmlDependency(
    name = "leaflet-timeline",
    version = "1.0.0",
    src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
    script = "javascripts/leaflet.timeline.js",
    stylesheet = "stylesheets/leaflet.timeline.css"
  )
  
  # use the new onRender in htmlwidgets to run
  #  this code once our leaflet map is rendered
  #  I did not spend time perfecting the leaflet-timeline
  #  options
  leaf %>%
    setView(44.0665,23.74667,2) %>%
    htmlwidgets::onRender(sprintf(
      '
function(el,x){
    var power_data = %s;

    var timeline = L.timeline(power_data, {
      pointToLayer: function(data, latlng){
        var hue_min = 120;
        var hue_max = 0;
        var hue = hue_min;
        return L.circleMarker(latlng, {
          radius: 10,
          color: "hsl("+hue+", 100%%, 50%%)",
          fillColor: "hsl("+hue+", 100%%, 50%%)"
        });
      },
      steps: 1000,
      duration: 10000,
      showTicks: true
    });
    timeline.addTo(this);
}
    ',
power_geo
    ))

leaf


