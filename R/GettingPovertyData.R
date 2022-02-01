library(tidycensus)
library(dplyr)
library(tidyr)
library(purrr)

# GET VARIABLES
vars <- load_variables(2019, "acs5", cache = TRUE)
# View(vars)

# VARIABLE NAMES
var_names <- c("below_pov_indv", "below_pov_indv_pop", "deep_pov_house", "deep_pov_denom")
var_codes <- c("B17020_002EA", "B17020_001EA", "B17026_002EA", "B17026_001EA")


# var_names <- c("below_pov_indv", "below_pov_indv_pop")
# var_codes <- c("B17020_002", "B17020_001")


codebook <-
  tibble(
    name = var_codes,
    short_desc = var_names
  ) |>
  inner_join(vars) |>
  print()

# PULLING 5-YEAR ACS ESTIMATES IN ENDING IN YEARS DEFINED BELOW.
acs_data <- NULL

year <- c(2009, 2014, 2019)
states <- c("PA")
items <- set_names(var_codes, var_names)
items
 for (i in year) {
# i <- 2020
  res <- get_acs(
    geography = "tract", survey = "acs5", variables = items,
    state = states, county = "Philadelphia", year = i, key = census_key
  )

  temp <-
    res |>
    select(-moe) |>
    mutate(year = i) |>
    pivot_wider(names_from = variable, values_from = estimate)

  acs_data <-
    bind_rows(acs_data, temp)
}


acs_data |>
  transmute(
    GEOID,
    NAME,
    year,
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
