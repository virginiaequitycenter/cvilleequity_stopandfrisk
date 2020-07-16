#setup
library(tidyverse)
library(leaflet)
library(sf)
library(htmltools)
library(RColorBrewer)
library(rmdformats)
library(ggplot2)

setwd("/Users/enriqueunruh/Documents/Equity Center/GitHub/cvilleequity_stopandfrisk")

#Data Files
sf <- readRDS("data/sf_combined.Rds")

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


library(ggraph)
library(igraph)
library(tidyverse)

sf_race_sfs <-sf_race %>%
  group_by(type2, race) %>%
  summarize(Stops = n())

# Create data
mygraph <- graph_from_data_frame(sf_race_year$value, vertices=sf_race_sfs$Stops)

# Make the plot
ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle() +
  theme_void()