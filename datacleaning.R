################
# Stop & Frisk #
################
library(tidyverse)
library(openxlsx)

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/cvilleequity_stopandfrisk")

SF2017_read <- read.xlsx("./data/2017 Stop and Frisk SDP Reviewed.xlsx", sheet = 1)

race_characters<- c("1|\\{|\\[|i|j|\\\\|\\!|\\|" )

SF2017 <-
SF2017_read %>%
  mutate(OFFENSE = str_trim(str_replace_all(OFFENSE, "\\[", "")),
         
         RACE = toupper(
                   str_trim(
                      str_replace_all(RACE, race_characters, "")
                           )
                        )
          ) 



View(SF2017)
