library(tidycensus)
library(dplyr)
library(tidyr)
library(purrr)

# GET VARIABLES
vars <- load_variables(2019, "acs5", cache = TRUE)
# View(vars)


# VARIABLE NAMES
var_names <- c("total_pop","female","below_pov", "pop_pov", "NHBlack", "NHAsian", "pop_race", "HispanicAn", "housing_all_units", "housing_occ_units", "renterocc_hh", "hh_income", "Birth_foreign", "pop_born", "Educ_ltHS", "pop_ed", "pop_labor", "employed", "trans_tran", "pop_trans", paste0("samhous_pop_grp_", 1:2), paste0("samhous_grp", 1:2))

var_codes <- c("B01001_001","B01001_026","B17020_002", "B17020_001", "B03002_004", "B03002_006", "B03002_001", "B03002_012", "B25002_001", "B25002_002", "B25003_003", "B19013_001", "B05002_013", "B05002_001", "B16010_002", "B16010_001", "B23025_003", "B23025_004", "B08006_008", "B08006_001", paste0("B07001_00", 1:2), paste0("B07001_0", 17:18))

codebook <-
  tibble(
    name = var_codes,
    short_desc = var_names
  ) |>
  inner_join(vars) |>
  print()


# PULLING 5-YEAR ACS ESTIMATES IN ENDING IN YEARS DEFINED BELOW.
acs_data <- NULL

year <- c(2015, 2019)
# states <- c("PA")
# county <- "Philadelphia"
items <- set_names(codebook$name, codebook$short_desc)

for (i in year) {
  res <- get_acs(
    geography = "zcta", survey = "acs5", variables = items,
    # state = states, couny = county, year = i, key = census_key
    year = i, key = census_key
  )

  temp <-
    res |>
    select(-moe) |>
    mutate(year = i) |>
    pivot_wider(names_from = variable, values_from = estimate)

  acs_data <-
    bind_rows(acs_data, temp)
}


acs_data <- acs_data |>
  transmute(
    GEOID,
    NAME,
    year,
    total_pop = total_pop,
    prc_female = female / total_pop,
    prc_pov = below_pov / pop_pov,
    across(
      .cols = c(NHBlack, NHAsian, HispanicAn, Birth_foreign),
      .fns = ~(.x / pop_race), # pop_race is the same as pop_total
      .names = "prc_{.col}"
    ),
    prc_educ_ltHS = Educ_ltHS / pop_ed,
    prc_employed = employed / pop_labor,
    prc_trans_tran = trans_tran / pop_trans,
    prc_renterocc_hh = renterocc_hh / housing_occ_units,
    prc_same_house =                        #       total - ages 1-4
      (samhous_grp1 - samhous_grp2) / ( samhous_pop_grp_1 - samhous_pop_grp_2),
    hh_income
   ) |>
  mutate(
    across(
      .cols = starts_with("prc"),
      .fns = ~round(.x * 100, 2))
  ) |>
  arrange(GEOID, year)

