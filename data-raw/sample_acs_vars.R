library(tidycensus)

acs_vars <- load_variables(2019, "acs5", cache = TRUE)

set.seed(1212)
sample_acs_vars <-
  acs_vars |>
  slice_sample(n = 10) |>
  print()

save(sample_acs_vars, file = "data/sample_acs_vars.rda")
# once a pkg can use  usethis::use_data(sample_acs_vars, overwrite = TRUE)
