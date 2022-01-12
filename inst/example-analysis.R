# this is an example script of how you could use the functions in the [R] folder
# and the built-in data from the [data] folder
library(tidycensus)
library(tidyverse)

source("R/utils-codebook.R")
source("R/pivot-census.R")
load("data/codebook_demographics.rda")

# would eventually drop ^^^ and use:
# devtools::load_all()

acs_vars <- load_variables(2019, "acs5", cache = TRUE)

code_list <-
  codebook_demographics |>
  bind_rows(
    build_lookup("household", "B09005", 1, 5)
  )

codebook <-
  prep_codebook(acs_vars, code_list$variable) |>
  left_join(code_list) |>
  mutate(
    demo =
      case_when(
        variable == "B16010_002" ~ "lt_hs",
        variable == "B9005_005" ~ "single_mom",
        TRUE ~ raw_demo
      ),
    var_alias = paste(table_alias, demo, sep = "_")
  ) |>
  select(variable, table, subgroup_id, table_alias, demo, var_alias) |>
  print()


# pass years to pivot_census for DE
acs_hx <-
  map_dfr(
    .x = 2018:2019,
    .f =
      ~pivot_census(
        year = .x,
        state = "DE",
        geography = "county",
        variables = set_names(codebook$variable, codebook$var_alias)
      )
  ) |>
  print()



# another example where you can pass a list or table as arguments to pmap_dfr()
var_table <-
  tibble(year = 2018:2019) |>
  expand( # makes a table with all combinations
    year,
    geography = "state",
    state = c("PA", "NJ")
  )


all_states <-
  pmap_dfr(
    .l = var_table, # list or table (different for each iteration)
    .f = pivot_census,
    # static arguments, ex: variables list is the same each time
    variables = set_names(codebook$variable, codebook$var_alias)
  ) |>
  print()

