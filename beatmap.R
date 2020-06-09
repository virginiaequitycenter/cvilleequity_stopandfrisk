# Pull police neighborhood map 
# Add population by race from census


# 0. Setup ----
library(geojsonio) # read in map
library(sf) # work with spatial data
library(tigris) # get blocks/block group files
library(tidycensus) # get census data
library(tidyverse) # the usual


# 1. Read in beat map ----
# open portal api
open_url <- "https://opendata.arcgis.com/datasets/ceaf5bd3321d4ae8a0c1a2b21991e6f8_9.geojson"
pm <- geojson_read(open_url, what = "sp")

# convert to sf
pm_sf <- st_as_sf(pm)
# plot(pm_sf[c(2,4)]) # check


# 2. Pull in census blocks and block groups to join ----
bg_cvl <- block_groups(state = "51", county = "540", cb = TRUE)
bl_cvl <- blocks(state = "51", county = "540")

# convert to sf
bg_cvl_sf <- st_as_sf(bg_cvl)
bl_cvl_sf <- st_as_sf(bl_cvl)

# plot(bg_cvl_sf[4]) # check
# plot(bl_cvl_sf[4])
# plot(pm_sf[2])

# reproject census to match beat (EPSG 4326): 
st_crs(bg_cvl_sf) # GRS 1980(IUGG, 1980)
st_crs(pm_sf) # WGS84

bg_cvl_sf2 <- st_transform(bg_cvl_sf, 4326)
bl_cvl_sf2 <- st_transform(bl_cvl_sf, 4326)

# plot(bg_cvl_sf[4])
# plot(bl_cvl_sf[4])
# plot(pm_sf[2])

# # join blocks/blockgroups to beats
# bl_cvl_beat <- st_intersects(pm_sf, bl_cvl_sf2)
# # list of blocks that intersect with each beat -- to understand multiple intersections


# 3. Pull in census data ----
# requires having a census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# acs_var <- load_variables(2018, "acs5", cache = TRUE)
# dec_var <- load_variables(2010, "sf1", cache = TRUE)

# a. Block groups: 2014-2018 ACS
## Median HH Income (B19013_001)
## Number white (B02001_002), Black (B02001_003), total (B02001_001)

varlist_b = c("B02001_001", # totalpop
              "B02001_002", # whitepop
              "B02001_003", # blackpop
              "B19013_001") # medinc

cvl_acs <- get_acs(geography = "block group",
                        variables = varlist_b,
                        state = "VA", 
                        county = "540", 
                        survey = "acs5",
                        year = 2018, 
                        output = "wide")

names(cvl_acs) = c("GEOID", "NAME",
                        "totalpopE", "totalpopM",
                        "whitepopE", "whitepopM",
                        "blackpopE", "blackpopM",
                        "medincE", "medincM")

# b. Blocks: 2010 Census
## Number white (P003002), Black (P003003), total (P003001)

varlist_d = c("P003001", # totalpop
              "P003002", # whitepop
              "P003003") # blackpop

cvl_acs_block <- get_decennial(geography = "block",
                               variables = varlist_d,
                               state = "51",
                               county = "540",
                               output = "wide")

names(cvl_acs_block) = c("GEOID", "NAME",
                         "totalpopE",
                         "whitepopE",
                         "blackpopE")


# 4. Aggregate to beats ----

# block groups
# join block groups to data
cvl_acs_bg <- geo_join(bg_cvl_sf, cvl_acs, by = "GEOID")
cvl_acs_bg2 <- st_transform(cvl_acs_bg, 4326)

# spatial aggregation: sum for pop (extensive=TRUE), average for income (extensive=FALSE)
cvl_acs_beat_bg <- st_interpolate_aw(cvl_acs_bg2[, c("totalpopE", "whitepopE", "blackpopE")],
                                  pm_sf, 
                                  extensive = TRUE)
cvl_acs_beat_bginc <- st_interpolate_aw(cvl_acs_bg2[, "medincE"],
                                      pm_sf,
                                      extensive = FALSE)
# any block group that's missing a value generates a missing beat value
#  median income is missing in two block groups, which intersect with 7 beats
#  looked into assigning missing block groups the tract level median inc, but looked likely to be misleading
#  drop for now; consider using median personal earnings later (present for all block groups)

# plot(cvl_acs_bg2[18])


# b. blocks
# join blocks to data
bl_cvl_sf <- bl_cvl_sf %>% mutate(GEOID = GEOID10)
cvl_acs_block <- geo_join(bl_cvl_sf, cvl_acs_block, by = "GEOID")
cvl_acs_block2 <- st_transform(cvl_acs_block, 4326)

# spatial aggregation: sum for pop, average for income
cvl_acs_beat_block <- st_interpolate_aw(cvl_acs_block2[, c("totalpopE", "whitepopE", "blackpopE")],
                                  pm_sf, 
                                  extensive = TRUE)

# plot(cvl_acs_beat_block[2]) # check
# plot(pm_sf[4])
# plot(cvl_acs_beat[2])


# 5. Use 2010 block aggregation ----
pm_pop_sf <- cbind(pm_sf, cvl_acs_beat_block)

cor(pm_pop_sf$POPULATION, pm_pop_sf$totalpopE) # .97

save.image("beatmap_work.RData")
saveRDS(pm_pop_sf, "beat_pop_map.Rds")

# # to use
# library(sf)
# library(tidyverse)
# beatmap <- readRDS("beat_pop_map.Rds")
# plot(beatmap[7])
