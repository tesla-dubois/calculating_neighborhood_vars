library(readr)
library(foreign)
library(tidycensus)
library(dplyr)
library(tidyr)

options(scipen = 999)


####################### KRIEGER EXAMPLE

# From Krieger, 2018: Using the Index of Concentration at the Extremes at multiple geographical levels to monitor health inequities in an era of growing spatial social polarization: Massachusetts, USA (2010–14)
# •	ICE for income: low vs. high US household income (20th vs. 80th percentile) (ACS variable B19001), using the cut-points of < $20,000 and ≥ $125,000;29 
# •	ICE for race/ethnicity: non-Hispanic Black vs non-Hispanic White (ACS variable B03002); and
# •	ICE for race/ethnicity + income (i.e. racialized economic segregation): Black population in low-income households vs the non-Hispanic White population in high-income households (ACS variables B19001H, B19001B).

####################### # CALL REQUIRED ICE VARIABLES FROM CENSUS
vars <- load_variables(2019, "acs5", cache = TRUE)
# View(vars)
ice_vars <- c("B19001_001", "B19001_002", "B19001_003", "B19001_004", "B19001_015", "B19001_016", "B19001_017", "B03002_003", 
              "B03002_004", "B03002_012", "B03002_001","B19001H_015", "B19001H_016", "B19001H_017","B19001B_002", "B19001B_003", 
              "B19001B_004","B19001I_002", "B19001I_003", "B19001I_004")
codebook <- vars[which(vars$name %in% ice_vars),]

# IDENTIFIY NEEDED STATES
f_data <- read_excel("Z:/Zack Frosch Project/Data/geocoding/Frosch Geocoding Complete_11.10.21.xlsx")
f_data <- f_data[which(!f_data$Note == "Unable to geocode"),]

states <- f_data%>%
  group_by(STATE_ABBR)%>%
  count()
states <- unique(states$STATE_ABBR)

# DOWNLOAD DATA, MAKE IT WIDE, EXCLUDE MARGIN OF ERROR
ice_data <- get_acs(
  geography = "tract",
  variables = ice_vars,
  state = states,
  year = 2019
)%>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

ice_data <- ice_data[, which(!grepl("moe", colnames(ice_data)))]


####################### VERSION 3: ETHNICITY/RACE ALONE: HISPANIC (REPORTED BY NCI SOCIAL DETERMINATES)
####### CREATE SUMMARY VARIABLES 
high_inc_vars <- paste0("estimate_", c("B19001_015", "B19001_016", "B19001_017"))
low_inc_vars <- paste0("estimate_", c("B19001_002", "B19001_003", "B19001_004"))

# CALCULATING ICE INCOME
ice_data$H125kmore <- apply(ice_data[high_inc_vars], 1, sum)
ice_data$H20kless <- apply(ice_data[low_inc_vars], 1, sum)
ice_data$totalH <- ice_data$estimate_B19001_001 # Households
ice_data$ICE_inc <- ((ice_data$H125kmore - ice_data$H20kless) / ice_data$totalH)*-1

# CALCULATING ICE_RACE - BLACK
ice_data$totalP <- ice_data$estimate_B03002_001 # PEOPLE
ice_data$ICE_raceB <- (ice_data$estimate_B03002_004 - ice_data$estimate_B03002_003) / ice_data$totalP # we nee
ice_data$ICE_raceB <- round(ice_data$ICE_raceB * -1, 4)
range(ice_data$ICE_raceB, na.rm = TRUE) # THIS LOOKS GOOD, RANGE IS -1 TO 1 AND THE CTS THAT ARE MOSTLY BLACK ARE NEGATIVE

# CALCULATING ICE_RACE - HISPANIC ANY RACE
ice_data$ICE_raceH <- (ice_data$estimate_B03002_012 - ice_data$estimate_B03002_003) / ice_data$totalP
ice_data$ICE_raceH <- round(ice_data$ICE_raceH * -1, 4)
range(ice_data$ICE_raceH, na.rm = TRUE) # THIS LOOKS GOOD, RANGE IS -1 TO 1 AND THE CTS THAT ARE MOSTLY BLACK ARE NEGATIVE

####################### VERSION 1: RACE AND INCOME COMBINED: (USED BY NANCY KRIEGER AS QUINTILES)
####### CREATE AGGREGATES
high_inc_white_vars <- paste0("estimate_", c("B19001H_015", "B19001H_016", "B19001H_017")) # 125k and up white
low_inc__black_vars <- paste0("estimate_",c("B19001B_002", "B19001B_003", "B19001B_004")) # low income Black
low_inc__His_vars <- paste0("estimate_",c("B19001I_002", "B19001I_003", "B19001I_004")) # low income Hispanic

# CALCULATING ICE_INC
ice_data$H125kmore_w <- apply(ice_data[high_inc_white_vars], 1, sum)
ice_data$H20kless_b <- apply(ice_data[low_inc__black_vars], 1, sum)
ice_data$H20kless_his <- apply(ice_data[low_inc__His_vars], 1, sum)

ice_data$ICE_RIB <- round(((ice_data$H125kmore_w - ice_data$H20kless_b) / ice_data$totalH), 4) * -1
ice_data$ICE_RIH <- round(((ice_data$H125kmore_w - ice_data$H20kless_his) / ice_data$totalH), 4) * -1

# SUBSET
myvars <- c("GEOID", "ICE_inc", "ICE_raceB","ICE_raceH", "ICE_RIB", "ICE_RIH")
new_ice <- ice_data[myvars]

#SAVE IT OUT 
getwd()
write.csv(new_ice, "Z:/Daniel/GEOspace & Fox Chase Projects/PANeighborhood-NDC/PA NJ Indices/ICE_2019.csv")

# CLEAN UP
rm(high_inc_vars)
rm(low_inc_vars)
rm(ice_vars)
rm(myvars)