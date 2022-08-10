#' Download Census Data
#'
#' @param .key
#' A required character object containing a valid Census API key, which can be requested at
#' \href{https://api.census.gov/data/key_signup.html}{api.census.gov}
#' @param .geo
#' Either "county", "tract", "block", or "place"
#' @param .use_age
#' Should the age be included in the census download?
#' @param .use_gen
#' Should the gender be included in the census download?
#' @param .dir
#' Folder to store data
#' @param .workers
#' Number of paralell workers for downloading data
#' @param .retry
#' The number of retries at the census website if network interruption occurs.
#' @param .progress
#' Progress Bar
#' @param .states
#' Two Letter state codes to download
#'
#' @return A Dataframe
#' @export
download_census <- function(.key = "", .geo, .use_age = TRUE, .use_gen = TRUE, .dir, .workers = 1,
                            .retry = 10, .progress = FALSE, .states = NULL) {
  # Debug -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # source("_debug_vars/debug-download_census.R")

  # Assign NULL to Global Vars -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  state <- age <- gen <- id <- name <- value <- NULL

  # Create Directory -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  dir_ <- file.path(.dir, .geo)
  dir.create(dir_, FALSE, TRUE)

  # Generate Combination -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  combs_ <- tidyr::expand_grid(
    state = get_states(),
    age = unique(c(FALSE, .use_age)),
    gen = unique(c(FALSE, .use_gen)),
  ) %>% dplyr::mutate(
    id = paste(state, age, gen, sep = "-"),
    dplyr::across(dplyr::everything(), ~ purrr::set_names(., id)
    ))

  if (!is.null(.states)) combs_ <- dplyr::filter(combs_, state %in% .states)

  # Check for non-downloaded files -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  fils_ <- lft(dir_)
  new_ <- dplyr::filter(combs_, !id %in% fils_$doc_id)

  # Download Files -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (nrow(new_) > 0) {
    message(paste0("Downloading ", nrow(new_), " State-Age-Gender Census Data"))

    future::plan("multisession", workers = .workers)
    furrr::future_pwalk(
      .l = list(combs_$state, combs_$age, combs_$gen, combs_$id),
      .f = ~ {
        path_ <- file.path(dir_, paste0(..4, ".rds"))
        if (!file.exists(path_)) {
          obj_ <- quiet(wru::get_census_data(
            key = .key, states = ..1, age = ..2, sex = ..3, census.geo = .geo, retry = .retry
          ))
          readr::write_rds(obj_, path_, "gz")
        }
      },
      .options = furrr::furrr_options(seed = TRUE),
      .progress = .progress
    )
    future::plan("default", workers = .workers)
  } else {
    message(paste0("All State-Age-Gender Census Data already Downloaded"))
  }

}



#' Wrapper around predictrace::predict_race()
#'
#' @param .tab
#' Input Table (see details)
#' @param .use
#' Which name variables to use?\cr
#' First Name (first_name) or Last Name (last_name)
#'
#' @return
#' The original data frame (.tab in long format) appended with the following columns:\cr
#' - method: Used methods (see details for more information)\cr
#' - pasian: Probability of the person being asian\cr
#' - pblack: Probability of the person being black\cr
#' - phispa: Probability of the person being hispanic\cr
#' - pwhite: Probability of the person being white\cr
#' - pother: Probability of the person being non of the above races\cr
#' - race: The most likely race as predicted by the respective method
#'
#' @export
#'
#' @details
#' The column 'method' encodes several variables at one and is encoded in the following way:\cr
#' {package}-{first name used}-{last name used}-{geo loaction used}-{age used}-{gender used}-{geo location}\cr
#' For example: The method WRU-0-1-1-1-1-C means that the package WRU is used, prediction is not based on the first name,
#' predictio is based on the last name, geo location is used, age is used, gender is used, and the geo location is a COunty (C)
#'
#' @examples
#' library(rRace)
#'
#' tab_names <- name_table
#'
#' race_prr(tab_names) # Using First/Last Name
#'
#' race_prr(tab_names, "first_name") # Using First Name
#'
#' race_prr(tab_names, "last_name") # Using Last Name
#'
#' # Using Middle Name (Throws an error, because there's no prediction on middle names)
#' # If a middle name is present in your dataset, consider pasting it together with the first name
#' \dontrun{
#' race_prr(tab_names, "middle_name")
#' }
#'
race_prr <- function(.tab, .use = c("first_name", "last_name")) {

  # Debug -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # source("_debug_vars/debug-race_prr.R")

  # Assign NULL to Global Vars -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  probability_asian <- probability_black <- probability_hispanic <-
    probability_white <- probability_american_indian <- prob_asian <-
    method <- use_last <- use_first <- use_geo <- use_age <- use_sex <-
    geo <- pother <- pwhite <- pblack <- pasian <- phispa <- id <- NULL


  # Checks -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  check_id(.tab)

  check_cols(.tab, .require = .use)

  # Predict Race: First Name -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  if ("first_name" %in% .use) {
    tab_fn_ <- dplyr::bind_cols(
      .tab, predictrace::predict_race(name = .tab$first_name, surname = FALSE)
    ) %>% dplyr::mutate(method = "PRR-1-0-0-0-0-N")
  } else {
    tab_fn_ <- tibble::tibble()
  }

  # Predict Race: Last Name -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if ("last_name" %in% .use) {
    tab_ln_ <- dplyr::bind_cols(
      .tab, predictrace::predict_race(name = .tab$last_name)
    ) %>% dplyr::mutate(method = "PRR-0-1-0-0-0-N")
  } else {
    tab_ln_ <- tibble::tibble()
  }

  # Prepare Output -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  out_ <- dplyr::bind_rows(tab_fn_, tab_ln_) %>%
    dplyr::rename(
      pasian = probability_asian,
      pblack = probability_black,
      phispa = probability_hispanic,
      pwhite = probability_white,
      pother = probability_american_indian
    ) %>%
    dplyr::select(!!!dplyr::syms(colnames(.tab)), method, pasian, pblack, phispa, pwhite, pother) %>%
    dplyr::arrange(id, method) %>%
    dplyr::mutate(
      tmp = pmax(pother, pwhite, pblack, pasian, phispa, na.rm = TRUE),
      race = dplyr::case_when(
        pasian == tmp ~ "asian",
        pother == tmp ~ "other",
        pblack == tmp ~ "black",
        phispa == tmp ~ "hispa",
        pwhite == tmp ~ "white"
      ),
      tmp = NULL
    )

  return(out_)
}

#' Wrapper around wru::predict_race()
#'
#' @param .tab
#' Input Table, must contain at least 2 columns:\cr
#' id: Unique Identifier of the Data\cr
#' first_name/last_name: At least a first name OR last name column (both columns are possible)\cr
#'
#' @param .use_geo
#' Should race be inferred from geo location (Logical: TRUE/FALSE, Default: FALSE)?\cr
#' If TRUE, .tab must contain a column "state" (2 Letter State Codes) and one column of
#' "county", "tract", "block", or "place". See: XXX for more information
#' @param .use_age
#' Should race be inferred from information about a persons birth year (Logical: TRUE/FALSE, Default: FALSE)?\cr
#' If TRUE .tab must contain a column "age" (age of the person in years)
#' @param .use_gen
#' Should race be inferred from information about a persons gender (Logical: TRUE/FALSE, Default: FALSE)?\cr
#' If TRUE .tab must contain a column "gen" (coding: male/female)
#' @param .census_dir
#' Directory to save Census Data (can be reused)
#'
#' @param .census_geo
#' Either "county", "tract", "block", or "place"
#'
#' @return
#' The original data frame (.tab in long format) appended with the following columns:\cr
#' - method: Used methods (see details for more information)\cr
#' - pasian: Probability of the person being asian\cr
#' - pblack: Probability of the person being black\cr
#' - phispa: Probability of the person being hispanic\cr
#' - pwhite: Probability of the person being white\cr
#' - pother: Probability of the person being non of the above races\cr
#' - race: The most likely race as predicted by the respective method
#'
#' @details
#' The column 'method' encodes several variables at one and is encoded in the following way:\cr
#' {package}-{first name used}-{last name used}-{geo loaction used}-{age used}-{gender used}-{geo location}\cr
#' For example: The method WRU-0-1-1-1-1-C means that the package WRU is used, prediction is not based on the first name,
#' predictio is based on the last name, geo location is used, age is used, gender is used, and the geo location is a COunty (C)
#'
#' @export
race_wru <- function(.tab, .use_geo = FALSE, .use_age = FALSE, .use_gen = FALSE, .census_dir = NULL, .census_geo = "") {
  # Debug -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # source("_debug_vars/debug-race_wru.R")

  # Assign NULL to Global Vars -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  last_name <- gender <- birth_year <- pred.asi <- pred.bla <- pred.his <- pred.whi <-
    pred.oth <- prob_asian <- age <- gen <- id <- use_geo <- use_age <- use_sex <-
    group <- surname <- tmp_id <- method <- use_last <- use_first <- geo <-
    pother <- pwhite <- pblack <- pasian <- phispa <- sex <- NULL


  # Checks -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  check_id(.tab)

  check_census_dir(.census_geo, .census_dir)

  check_geo_columns(.tab, .census_geo)
  check_cols(.tab, "last_name")

  if (.use_age) check_cols(.tab, "age")

  if (.use_gen) check_cols(.tab, "gen")

  # Rename Variables for WRU Package -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  tab_in_ <- dplyr::mutate(.tab, surname = trimws(gsub("[^[:alnum:] ]", "", last_name))) # last_name = surname
  tab_in_ <- dplyr::mutate(tab_in_, gen = as.integer(gen == "female"))

  # Make Predictions -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  combs_  <- wru_get_combinations(.use_age, .use_gen)
  if (.use_geo) {
    census_ <- wru_read_check_census(.tab, .census_dir, .census_geo, .use_age, .use_gen)
  } else {
    census_ <- tibble::tibble(age = 1L, gen = 1L, value = list())
  }


  tab_ <- purrr::pmap_dfr(
    .l = list(combs_$use_geo, combs_$use_age, combs_$use_gen),
    .f = ~ {
      tab_use_ <- tab_in_
      tab_use_ <- if (..2) dplyr::filter(tab_use_, !is.na(age)) else tab_use_
      tab_use_ <- if (..3) dplyr::filter(tab_use_, !is.na(gen)) else tab_use_
      wru::predict_race(
        voter.file = tab_use_,
        surname.only = ..1,
        census.geo = .census_geo,
        census.data = purrr::flatten(dplyr::filter(census_, age == ..2, gen == ..3)$value),
        age = ..2,
        sex = ..3
      ) %>% tibble::as_tibble() %>% quiet() %>% suppressWarnings()
    }, .id = "method"
  )

  geo_ <- ifelse(is.null(.census_geo), "N", toupper(stringi::stri_sub(.census_geo, 1, 1)))
  out_ <- tab_ %>%
    dplyr::mutate(
      method = gsub("TRUE", "1", method),
      method = gsub("FALSE", "0", method),
      method = paste0("WRU-0-1-", method, "-", geo_),
      gen = dplyr::if_else(gen == 1, "female", "male")
    ) %>%
    dplyr::select(-surname) %>%
    dplyr::rename(id = tmp_id)


  miss_ <- tidyr::expand_grid(dplyr::distinct(out_, method), id = .tab$id) %>%
    dplyr::anti_join(
      y = dplyr::select(out_, id, method),
      by = c("method", "id")
    ) %>%
    dplyr::left_join(.tab, by = "id")

  out_ <- dplyr::bind_rows(out_, miss_) %>%
    dplyr::rename(
      pasian = pred.asi,
      pblack = pred.bla,
      phispa = pred.his,
      pwhite = pred.whi,
      pother = pred.oth
    ) %>%
    dplyr::select(!!!dplyr::syms(colnames(.tab)), method, pasian, pblack, phispa, pwhite, pother) %>%
    dplyr::arrange(id, method) %>%
    dplyr::mutate(
      tmp = pmax(pother, pwhite, pblack, pasian, phispa, na.rm = TRUE),
      race = dplyr::case_when(
        pasian == tmp ~ "asian",
        pother == tmp ~ "other",
        pblack == tmp ~ "black",
        phispa == tmp ~ "hispa",
        pwhite == tmp ~ "white"
      ),
      tmp = NULL
    )

  return(out_)

}

#' Predict Race
#'
#' @param ... Tables generated by race_prr(), race_wru(), or the race_eth python function
#' @return
#' The dataframes in ... are appended and the following columns are added:\cr
#' - hp (Highest Probability): Shows the highest probability of the race prediction for each row
#' - gd (Guess Difference): Shows the difference between the highest and second highest probability
#' - rank_hp: Prediction rank based on the highest probability (hp)
#' - rank_gd: Prediction rank based on the guess difference (gd)
#'
#' @details
#' The column 'method' encodes several variables at one and is encoded in the following way:\cr
#' {package}-{first name used}-{last name used}-{geo loaction used}-{age used}-{gender used}-{geo location}\cr
#' For example: The method WRU-0-1-1-1-1-C means that the package WRU is used, prediction is not based on the first name,
#' predictio is based on the last name, geo location is used, age is used, gender is used, and the geo location is a COunty (C)
#'
#' @export
race_predict <- function(...) {

  # Debug -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # source("_debug_vars/debug-race_predict.R")

  # Assign NULL to Global Vars -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  pother <- pwhite <- pblack <- pasian <- phispa <- hp <- gd <- race <- id <- rank_hp <-
    rank_gd <- method <- NULL

  # Prepare Final Putput -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  dplyr::bind_rows(...) %>%
    dplyr::mutate(
      hp = pmax(pother, pwhite, pblack, pasian, phispa, na.rm = TRUE),
      dplyr::across(c(pother, pwhite, pblack, pasian, phispa), ~ dplyr::if_else(. == hp, NA_real_, .)),
      gd = pmax(pother, pwhite, pblack, pasian, phispa, na.rm = TRUE),
      gd = hp - gd,
      dplyr::across(c(pother, pwhite, pblack, pasian, phispa), ~ dplyr::if_else(is.na(.), hp, .)),
    ) %>%
    dplyr::relocate(race, .after = gd) %>%
    dplyr::arrange(id, method) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      rank_hp = dplyr::dense_rank(-hp),
      rank_gd = dplyr::dense_rank(-gd)
    ) %>%
    dplyr::arrange(rank_hp, .by_group = TRUE) %>%
    dplyr::ungroup()

}



#' Predict Gender
#'
#' @param .tab
#' Input Table (see details)
#' @param .use_age
#' Should race be inferred from information about a persons birth year?
#' @param .methods
#' Any combination of "ssa", "ipums", "napp"
#'
#' @return
#' The original data frame (.tab in long format) appended with the following columns:\cr
#' - method: Used methods (see details for more information)\cr
#' - pmale: Probability of the person being male\cr
#' - gender: The most likely gender as predicted by the respective method\cr
#' - rank: Prediction Rank\cr
#'
#' @details
#' The column 'method' encodes several variables at one and is encoded in the following way:\cr
#' {package}-{first name used}-{last name used}-{geo loaction used}-{age used}-{gender used}-{geo location}\cr
#' For example: The method WRU-0-1-1-1-1-C means that the package WRU is used, prediction is not based on the first name,
#' predictio is based on the last name, geo location is used, age is used, gender is used, and the geo location is a COunty (C)
#' @export
gender_predict <- function(.tab, .use_age = FALSE, .methods = c("ssa", "ipums", "napp")) {

  # Debug -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # source("_debug_vars/debug-gender_predict.R")


  # Assign NULL to Global Vars -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  first_name <- birth_year <- gender <- name <- method <- use_birth <- year_min <-
    year_max <- proportion_male <- proportion_female <- prob_male <- prob_female <-
    guess_diff <- highest_prob <- age <- name_ <- id <- pmale <- NULL

  # Checks  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  check_id(.tab)
  check_cols(.tab, "first_name")

  if (!all(.methods %in% c("ssa", "ipums", "napp"))) {
    stop("allowed methods are 'ssa', 'ipums' and 'napp'", call. = FALSE)
  }

  # Standardize Names  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  tab_ <- dplyr::mutate(.tab, name_ = trimws(toupper(gsub("[^[:alnum:] ]", "", first_name)))) %>%
    dplyr::mutate(birth_year = as.integer(format(Sys.time(), "%Y")) - age)

  # Checks  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  if (any(grepl(" ", tab_$name_, fixed = TRUE))) {
    msg_ <- paste0(
      "Column first_name contains blanks (maybe a double first name), ensure that the ",
      "column first_name only contains single names"
    )
    stop(msg_, call. = FALSE)
  }


  # Predict w/o age  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  tab1_ <- purrr::map_dfr(
    .x = purrr::set_names(.methods, .methods),
    .f = ~ gender::gender(unique(tab_[["name_"]]), method = .x),
    .id = "method"
  ) %>% dplyr::mutate(method = paste0(toupper(stringi::stri_sub(method, 1, 3)), "-0"))  %>%
    dplyr::left_join(tab_, by = c("name" = "name_"))


  # Predict with age  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (.use_age) {
    check_cols(.tab, "age")
    tab_in_ <- dplyr::distinct(tab_, name_, birth_year)
    lst_in_ <- split(tab_in_, tab_in_$birth_year)
    tab2_ <- purrr::map_dfr(
      .x = purrr::set_names(.methods, .methods),
      .f = ~ purrr::map_dfr(
        .x = lst_in_,
        .f = ~ gender::gender(.x[["name_"]], .x[["birth_year"]][1], .method),
        .id = "birth_year"
      ) %>% quiet() %>% suppressWarnings() %>%
        dplyr::mutate(gender = as.character(gender), birth_year = as.integer(birth_year)),
      .id = "method"
    ) %>%
      dplyr::mutate(method = paste0(toupper(stringi::stri_sub(method, 1, 3)), "-1")) %>%
      dplyr::left_join(tab_, by = c("name" = "name_", "birth_year"))

  } else {
    tab2_ <- tibble::tibble()
  }
  out_ <- dplyr::bind_rows(tab1_, tab2_) %>%
    dplyr::select(!!!dplyr::syms(colnames(.tab)), method, pmale = proportion_male, gender)

  miss_ <- tidyr::expand_grid(dplyr::distinct(out_, method), id = .tab$id) %>%
    dplyr::anti_join(
      y = dplyr::select(out_, id, method),
      by = c("method", "id")
    ) %>%
    dplyr::left_join(.tab, by = "id")

  out_ <- dplyr::bind_rows(out_, miss_) %>%
    dplyr::arrange(id, method) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(rank = dplyr::dense_rank(-pmale)) %>%
    dplyr::arrange(rank, .by_group = TRUE) %>%
    dplyr::ungroup()

  return(out_)



}
