## code to prepare `codebook_demographics` dataset goes here

# built using usethis::use_data_raw("codebook_demographics")
source("R/utils-codebook.R") # will use devtools::load_all() in the future

acs_vars <- load_variables(2019, "acs5", cache = TRUE)

codebook_demographics <- #code_df <-
  bind_rows(
    build_lookup("pop", "B03002", 1),
    build_lookup("race", "B03002", 4, 6, 12),
    build_lookup("nat", "B05002", 1, 13),
    build_lookup("age", "B07001", 1:2),
    build_lookup("age_same_hh", "B07001", 17:18),
    # build_lookup("age", "B07001", 3:16),
    # build_lookup("age_same_hh", "B07001", 19:32),
    build_lookup("transit",  "B08006", 1, 8),
    build_lookup("ed", "B16010", 1:2),
    build_lookup("poverty", "B17020", 1:2),
    build_lookup("hh_income", "B19013", 1),
    build_lookup("labor",  "B23025", 3:4),
    build_lookup("housing", "B25002", 1:2),
    build_lookup("housing", "B25003", 3)
  ) |>
  print()


# codebook_demographics <-
#   prep_codebook(acs_vars, code_df$variable) |>
#   left_join(code_df) |>
#   mutate(
#     demo =
#       case_when(
#         variable == "B16010_002" ~ "lt_hs",
#         TRUE ~ raw_demo
#       ),
#     var_name = paste(alias, demo, sep = "_")
#   ) |>
#   select(variable, table, subgroup_id, alias, demo, var_name) |>
#   print(n = Inf)


save(codebook_demographics, file = "data/codebook_demographics.rda")
# once a pkg can use  usethis::use_data(codebook_demographics, overwrite = TRUE)
