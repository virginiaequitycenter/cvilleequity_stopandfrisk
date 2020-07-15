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



#Summary Macro Data

sf_summary_stats <- sf %>%
  group_by(race) %>%
  summarize(Totals = n()) %>%
  filter(race %in% c("B", "W"))

sf_summary_stats$race[sf_summary_stats$race == "B"] <- "Black"
sf_summary_stats$race[sf_summary_stats$race == "W"] <- "White"

sf_summary_stats <- sf_summary_stats%>%
  mutate(prop = Totals / sum(sf_summary_stats$Totals)) %>%
  mutate(ymax = cumsum(prop), ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2, 
         label = Totals,
         position = c(0,0.1))


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

ggplot(sf_summary_stats, aes(x = position, y = Totals, fill = race)) +
  geom_bar(stat = "identity", width = 0.1) +
  scale_fill_hue(c = 40) +
  coord_flip() +
  scale_fill_manual(values = pal2) +
  geom_text(aes(y = Totals/1.2, label = label), size=7, color = "white") +
  geom_text(aes(y = -50, label = race), size=5, fontface=1) +
  ggtitle("Detentions by Charlottesville Police Officers from 2012-2017") +
  theme_void() +
  theme(legend.position = "none", 
        plot.title = element_text(size=15, face=4, hjust = .5))




#Detentions Relative to Population --------

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

pal2 <- c("#52a2a9" ,"#d2a48b")

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






