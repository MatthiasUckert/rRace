## code to prepare `name_table` dataset goes here
library(tidyverse); library(stringi)

set.seed(123)
int <- sample(200)

name_table <- read_csv("data-raw/name_table_raw.csv", col_types = cols(.default = "c")) %>%
  mutate(
    gender = if_else(sex == "Male", "male", "female"),
    TRACTCE10 = stri_pad_left(TRACTCE10, 11, "0"),
    CD = stri_sub(TRACTCE10, 1, 2),
    county = stri_sub(TRACTCE10, 3, 5),
    tract = stri_sub(TRACTCE10, 6, 11),
  ) %>%
  left_join(tibble(state = state.name, code = state.abb), by = "state") %>%
  rename(state_name = state, state = code) %>%
  mutate(id = row_number(), birth_year = 2022 - as.integer(age)) %>%
  select(id, first_name, last_name = surname, gender, birth_year, state, county, tract) %>%
  slice(1:200) %>%
  mutate(
    first_name = .$first_name[int],
    gender = .$gender[int]
    )



usethis::use_data(name_table, overwrite = TRUE)
