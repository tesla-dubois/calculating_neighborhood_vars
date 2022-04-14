# Link census data with shapefiles
library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)

# SHAPEFILES
PA_cts <- read_sf("U:/Program Files/SettingUpGit/Shapefiles/PA Census Tracts/tl_2016_42_tract.shp")
PA_zips <- read_sf("U:/Program Files/SettingUpGit/Shapefiles/PA Zip Codes/tl_Pennsylvania5Digit2009.shp")

# DATA
Pit_Data <-read.csv("U:/Projects/Personal/GIS in MPH/CensusNeighborhoodVars/allegheny_data.csv")|>
  mutate(GEOID = as.character(GEOID))
Phila_Data <-read.csv("U:/Projects/Personal/GIS in MPH/CensusNeighborhoodVars/Philly_data.csv")|>
  mutate(GEOID = as.character(GEOID))
PA_zip_data <- read.csv("U:/Projects/Personal/GIS in MPH/CensusNeighborhoodVars/pa_zipcode_data.csv")|>
  mutate(GEOID = as.character(GEOID))

# CHECKING SHAPEFILES
ggplot(PA_cts)+
  geom_sf(aes())

# JOINING
Pit_joined <- left_join(PA_cts, Pit_Data, by = "GEOID")|>
  # na.omit()
  filter(COUNTYFP == "003")
  # st_transform(2272)

Phila_joined <- left_join(PA_cts, Phila_Data, by = "GEOID") |>
  filter(COUNTYFP == 101)|>
  st_transform(2272)

PA_zips_joined <- left_join(PA_zips, PA_zip_data, by = c("ZCTA5CE" = "GEOID"))|>
  st_transform(4326)

colnames(PA_zips_joined)

ggplot(PA_zips_joined)+
  geom_sf(aes(fill = prc_pov))

ggplot(Phila_joined)+
  geom_sf(aes(fill = prc_pov))

ggplot(Pit_joined)+
  geom_sf(aes(fill = prc_pov))


write_sf(Pit_joined, "U:/Projects/Personal/GIS in MPH/CensusNeighborhoodVars/allegheny_data.shp")
write_sf(Phila_joined, "U:/Projects/Personal/GIS in MPH/CensusNeighborhoodVars/philly_data.shp")
write_sf(PA_zips_joined, "U:/Projects/Personal/GIS in MPH/CensusNeighborhoodVars/PA_zips_data.shp")


