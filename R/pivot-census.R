#' @examples
#' pivot_census(geography = "state", variables = "B19013_001", state = "DE", year = 2019)
pivot_census <- function(geography, variables, state, year) {
  res <-
    get_acs(
      geography = geography,
      survey = "acs5",
      variables = variables,
      state = state,
      year = year
    )

  res |>
    mutate(
      geography = geography,
      state = state,
      year = year
    ) |>
    relocate(year, state, geography, .before = everything()) |>
    select(-moe) |>
    pivot_wider(
      names_from = variable,
      values_from = estimate
    )
}
