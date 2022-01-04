library(tidycensus)
library(dplyr)
library(tidyr)

# GET VARIABLES
vars <- load_variables(2019, "acs5", cache = TRUE)
# View(vars)

# VARIABLE NAMES
var_names <- c("below_pov", "pop_pov", "NHBlack", "NHAsian", "pop_race", "HispanicAn", "housing_all_units", "housing_occ_units", "renterocc_hh", "hh_income", "Birth_foreign", "pop_born", "Educ_ltHS", "pop_ed", "pop_labor", "employed", "trans_tran", "pop_trans", paste0("samhous_pop_grp_", 1:6), paste0("samhous_pop_grp_", 7:14), paste0("samhous_grp", 1:14))
var_codes <- c("B17020_002", "B17020_001", "B03002_004", "B03002_006", "B03002_001", "B03002_012", "B25002_001", "B25002_002", "B25003_003", "B19013_001", "B05002_013", "B05002_001", "B16010_002", "B16010_001", "B23025_003", "B23025_004", "B08006_008", "B08006_001", paste0("B07001_00", 3:9), paste0("B07001_0", 10:16), paste0("B07001_0", 19:32))
items <- setNames(var_codes, var_names)
items_df <- as.data.frame(items)
items_df <- tibble::rownames_to_column(items_df, "short_desc")
codebook <- as.data.frame(vars[which(vars$name %in% var_codes), ])
codebook <- left_join(codebook, items_df, by = c("name" = "items"))

# CLEAN UP
rm(var_codes, var_names, vars, items_df)

# PULLING 5-YEAR ACS ESTIMATES IN ENDING IN YEARS DEFINED BELOW.
data <- NULL
year <- c(2019)
states <- c("PA", "NJ")

for (i in year) {
  temp <- get_acs(
    geography = "tract", survey = "acs5", variables = items, # CHANGE VAR NAMES DEPENDING ON WHAT YOU WANT, CAN ALSO ASSIGN IN FOR LOOP
    state = states, year = i, key = census_key
  )
  temp <- temp[, 1:4] %>%
    pivot_wider(names_from = variable, values_from = estimate) %>% # MAKE IT WIDE
    mutate(
      prc_pov = round(below_pov / pop_pov * 100, 2),
      prc_NHBlack = round(NHBlack / pop_race * 100, 2),
      prc_NHAsian = round(NHAsian / pop_race * 100, 2),
      prc_HispanicAn = round(HispanicAn / pop_race * 100, 2),
      prc_renterocc_hh = round(renterocc_hh / housing_occ_units * 100, 2),
      prc_birth_foreign = round(Birth_foreign / pop_born * 100, 2),
      prc_educ_ltHS = round(Educ_ltHS / pop_ed * 100, 2),
      prc_employed = round(employed / pop_labor * 100, 2),
      prc_trans_tran = round(trans_tran / pop_trans * 100, 2),
      same_house_pop = rowSums(select(., starts_with("samhous_pop_grp_"))),
      same_house = rowSums(select(., starts_with("samhous_grp"))),
      prc_same_house = round(same_house / same_house_pop * 100, 2)
    )
  data <- rbind(data, temp) # ADD DATA FROM EACH ITERATION THROUGH THE FOR LOOP TO THE DATA FROM PREVIOUS ITERATIONS
  rm(temp) # CLEAN UP
}

# REDUCE ACS VARS TO THE PERCENTAGES
myvars <- c("GEOID", "prc_pov", "prc_NHBlack", "prc_NHAsian", "prc_HispanicAn", "prc_renterocc_hh", "prc_birth_foreign", "prc_educ_ltHS", "prc_employed", "prc_trans_tran", "prc_same_house", "hh_income")
acs_data <- data[myvars]

# CLEAN UP
rm(i, items, myvars, states, year, data)

# TO DO:
# - Generate a new codebook for my output data.
