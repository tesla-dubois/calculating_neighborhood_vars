library(tidyverse)

#' @examples
#' build_lookup(alias = "age", table = "B07001", 1:5)
build_lookup <- function(alias, table, ...) {
  vars <-
    paste(
      table,
      str_pad(c(...), 3, "left", "0"),
      sep = "_"
    )

  tibble(
    table_alias = alias,
    table = table,
    subgroup_id = c(...),
    variable = vars
  )
}


#' @examples
#' prep_codebook(acs = sample_acs_vars, codes = "B24115_335")
prep_codebook <- function(acs, codes) {
  # codes |>
  #   left_join(select(acs, name, label)) |>
  acs |>
    filter(name %in% codes) |>
    transmute( # same as a mutate() then select(-label)
      variable = name,
      last_statement =
        tolower(label) |>
        str_extract("[^!]+$") |> # keep everything after the last !!
        str_remove(" \\(.+"), # remove " (..."
      raw_demo =
        last_statement |>
        str_extract(
          # 1st word | digit maybe followed by [word digit]
          "(^[a-z]+)|(^\\d+( \\w+ \\d+)?)"
        ) |>
        str_replace_all(" \\w+ ", "_") #  [" and "] & [" to "] -> ["_"]
    )
}
