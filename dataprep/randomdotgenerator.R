########################
# Random Dot Generator #
########################

beats <- readRDS("beat_pop_map.Rds")

SF1920 <- read_csv("data/finaldata/SF1920.csv")
SF17 <- read_csv("data/finaldata/SF2017.csv")
SF16 <- read_csv("data/finaldata/SF2016.csv")
SF1214 <- read_csv("data/finaldata/SF2014.csv")
monthlabels <- data.frame(Abbrev = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
           Month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))

### 2019-2020 Locaiton Imputations ###

num1920 <- SF1920 %>%
           select(BEAT_NO = beatnum, Number) %>%
           group_by(BEAT_NO) %>%
           summarize(NumDots = sum(Number)) %>%
           mutate(BEAT_NO = str_replace_all(BEAT_NO, "C", ""))

dots1920 <-     
  map_df( unique(num1920$BEAT_NO),  # Okay, so for every occupation
          
          ~ st_sample(beats$geometry[beats$BEAT_NO == .x],                             # Interpolate this surface 
                      size = num1920$NumDots[num1920$BEAT_NO == .x], # with this occupation's dots
                      type = "random",
                      exact = TRUE) %>%                                              # generate the points in ea
            st_cast("POINT") %>%                                                      # cast the geom set as 'POI
            st_coordinates() %>%                                                      # pull out coordinates into
            as_tibble() %>%                                                           # convert to tibble
            setNames(c("lon","lat")) %>%                                              # set column names
            mutate(BEAT_NO = .x)  # add categorical party var  
  ) %>% arrange(BEAT_NO)          
   
SF1920Locations <-            
SF1920 %>%
  mutate(BEAT_NO = str_replace_all(beatnum, "C", "")) %>%
  filter(Number > 0) %>%
  group_by(BEAT_NO, race, Month, Year ) %>%
  slice(rep(1:n(), each = Number)) %>%
  arrange(BEAT_NO) %>%
  add_column(lat = dots1920$lat, lon = dots1920$lon, beatnumdot = dots1920$BEAT_NO) %>% 
  mutate(SFTYPE = NA, OFFENSE = NA) %>%
  select(SFTYPE, OFFENSE, BEAT_NO, RACE = race, BEAT_NO, lat, lon, beatnumdot, Year, Abbrev = Month ) %>%
  left_join(monthlabels) %>%
  ungroup() %>%
  select(-Abbrev)
  
  



### 2017 Location Imputations ###
num17 <-
  SF17 %>%
  group_by(BEAT_NO) %>%
  summarize(NumDots = n()) 

beats

# Time to interpolate the dots
dots2017 <-     
  map_df( unique(num17$BEAT_NO),  # Okay, so for every occupation
          
          ~ st_sample(beats$geometry[beats$BEAT_NO == .x],                             # Interpolate this surface 
                      size = num17$NumDots[num17$BEAT_NO == .x], # with this occupation's dots
                      type = "random",
                      exact = TRUE) %>%                                              # generate the points in ea
            st_cast("POINT") %>%                                                      # cast the geom set as 'POI
            st_coordinates() %>%                                                      # pull out coordinates into
            as_tibble() %>%                                                           # convert to tibble
            setNames(c("lon","lat")) %>%                                              # set column names
            mutate(BEAT_NO = .x)  # add categorical party var  
  ) %>% arrange(BEAT_NO)

SF2017Locations <-
SF17 %>%
  select(SFTYPE, OFFENSE, RACE, BEAT_NO) %>%
  arrange(BEAT_NO) %>%
  add_column(lat = dots2017$lat, lon = dots2017$lon, beatnumdot = dots2017$BEAT_NO) %>%
  mutate(Year = 2017, Month = NA) %>%
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

### 2016 Location Imputations ### 

num16 <-
  SF16 %>%
  group_by(BEAT) %>%
  summarize(NumDots = n()) %>%
  mutate(BEAT_NO = str_replace_all(BEAT, "C", ""))

# Time to interpolate the dots
dots2016 <-     
  map_df( unique(num16$BEAT_NO),  # Okay, so for every occupation
          
          ~ st_sample(beats$geometry[beats$BEAT_NO == .x],                             # Interpolate this surface 
                      size = num16$NumDots[num16$BEAT_NO == .x], # with this occupation's dots
                      type = "random",
                      exact = TRUE) %>%                                              # generate the points in ea
            st_cast("POINT") %>%                                                      # cast the geom set as 'POI
            st_coordinates() %>%                                                      # pull out coordinates into
            as_tibble() %>%                                                           # convert to tibble
            setNames(c("lon","lat")) %>%                                              # set column names
            mutate(BEAT_NO = .x)  # add categorical party var  
  ) %>% arrange(BEAT_NO)

SF2016Locations <-
  SF16 %>%
  mutate(BEAT_NO = str_replace_all(BEAT, "C", "")) %>%
  select(SFTYPE, OFFENSE, RACE, BEAT_NO, Date) %>%
  arrange(BEAT_NO) %>%
  add_column(lat = dots2016$lat, lon = dots2016$lon, beatnumdot = dots2016$BEAT_NO) %>%
  mutate(Year = 2016, Month = format(Date,"%m")) %>%
  select(-Date) %>%
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order


### 2014 Locaion Imputation ### 

num1214 <-
  SF1214 %>%
  group_by(BEAT) %>%
  summarize(NumDots = n()) %>%
  mutate(BEAT_NO = str_replace_all(BEAT, "C", ""))

# Time to interpolate the dots
dots1214 <-     
  map_df( unique(num1214$BEAT_NO),  # Okay, so for every occupation
          
          ~ st_sample(beats$geometry[beats$BEAT_NO == .x],                             # Interpolate this surface 
                      size = num1214$NumDots[num1214$BEAT_NO == .x], # with this occupation's dots
                      type = "random",
                      exact = TRUE) %>%                                              # generate the points in ea
            st_cast("POINT") %>%                                                      # cast the geom set as 'POI
            st_coordinates() %>%                                                      # pull out coordinates into
            as_tibble() %>%                                                           # convert to tibble
            setNames(c("lon","lat")) %>%                                              # set column names
            mutate(BEAT_NO = .x)  # add categorical party var  
  ) %>% arrange(BEAT_NO)

SF1214Locations <-
  SF1214 %>%
  mutate(BEAT_NO = str_replace_all(BEAT, "C", "")) %>%
  select(SFTYPE, OFFENSE, RACE, BEAT_NO, Year = YEAR, Date) %>%
  arrange(BEAT_NO) %>%
  mutate( Month = format(Date,"%m")) %>%
  add_column(lat = dots1214$lat, lon = dots1214$lon, beatnumdot = dots1214$BEAT_NO) %>%
  select(-Date) %>%
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

####################
SFALLLOCATIONS <-
  bind_rows(
    SF1920Locations,
    SF2017Locations,
    SF2016Locations,
    SF1214Locations
  )

View(SFALLLOCATIONS)

write_csv(SFALLLOCATIONS, path = "data/finaldata/sflocations.csv")

