#' Wrapper around predictrace::predict_race()
#'
#' @param .tab Input Table (see details)
#' @param .use Which name variables to use? First Name (first_name) or Last Name (last_name)
#'
#' @return
#' The original data frame (.tab in long format) appended with the following columns:\cr
#' - method: Used methods (here: prr for predictrace)\cr
#' - use_name: which name variable has been used (here: first/last)\cr
#' - use_geo: Which geo variable has been used (here: none)\cr
#' - use_age: Is a persons age used for prediction (here: FALSE)\cr
#' - use_sex: Is a persons sex used for prediction (here: FALSE)\cr
#' - prob_asian: Probability of the person being asian\cr
#' - prob_black: Probability of the person being black\cr
#' - prob_hispanic: Probability of the person being hispanic\cr
#' - prob_white: Probability of the person being white\cr
#' - prob_other: Probability of the person being non of the above races\cr
#'
#' IMPORTANT: Non-Predictions are omited from the output
#' @export
#'
#' @examples
#' library(Rrace)
#'
#' tab_names <- name_table
#' # Using First/Last Name
#' race_prr(tab_names)
#'
#' # Using First Name
#' race_prr(tab_names, "first_name")
#'
#' # Using Last Name
#' race_prr(tab_names, "last_name")
#'
#' # Using Middle Name (Throws an error, because there's no prediction on middle names)
#' # If a middle name is present in your dataset, consider pasting it together iwth the first name
#' \dontrun{
#' race_prr(tab_names, "middle_name")
#' }
#'

# DEBUG
# .tab <- name_table
# .use = c("first_name", "last_name")
race_prr <- function(.tab, .use = c("first_name", "last_name")) {
  probability_asian <- probability_black <- probability_hispanic <-
    probability_white <- probability_american_indian <- prob_asian <- NULL


  # Check Columns -----------------------------------------------------------
  check_cols(.tab, .require = .use)

  # Checks ------------------------------------------------------------------
  if (!"id" %in% colnames(.tab)) {
    stop("Name Table (.tab) must have a unique column ID")
  }

  if (any(duplicated(.tab[["id"]]))) {
    stop("Name Table (.tab) must have a unique column ID")
  }

  # Predict Race: First Name ------------------------------------------------
  if ("first_name" %in% .use) {
    tab_fn_ <- dplyr::bind_cols(
      .tab, predictrace::predict_race(name = .tab$first_name, surname = FALSE)
    ) %>% dplyr::mutate(use_name = "first")
  } else {
    tab_fn_ <- tibble::tibble()
  }

  # Predict Race: Last Name -------------------------------------------------
  if ("last_name" %in% .use) {
    tab_ln_ <- dplyr::bind_cols(
      .tab, predictrace::predict_race(name = .tab$last_name)
    ) %>% dplyr::mutate(use_name = "last")
  } else {
    tab_ln_ <- tibble::tibble()
  }

  # Prepare Output ----------------------------------------------------------
  tab_ <- dplyr::bind_rows(tab_fn_, tab_ln_) %>%
    dplyr::rename(
      prob_asian = probability_asian,
      prob_black = probability_black,
      prob_hispanic = probability_hispanic,
      prob_white = probability_white,
      prob_other = probability_american_indian
    ) %>%
    dplyr::mutate(use_geo = "none", use_age = FALSE, use_sex = FALSE, method = "prr") %>%
    dplyr::filter(!is.na(prob_asian))

  # Select and Reorder Columns ----------------------------------------------
  cn_ <- colnames(tab_)
  tab_ <- tab_[, c(colnames(.tab), "method", cn_[grepl("use_", cn_)], sort(cn_[grepl("prob_", cn_)]))]
  tibble::as_tibble(tab_)
}


#' Wrapper around wru::predict_race()
#'
#' @param .tab Input Table (see details)
#' @param .use_geo
#' Which geo variable has been used (Either "county", "tract", "block", or "place").
#' If used must be the same as in download_census()
#' @param .use_age Should race be inferred from information about a persons age?
#' @param .use_sex Should race be inferred from information about a persons sex?
#' @param .census Only needed if .use_geo is not NULL
#'
#' @return
#' The original data frame (.tab in long format) appended with the following columns:\cr
#' - method: Used methods (here: prr for predictrace)\cr
#' - use_name: which name variable has been used (here: first/last)\cr
#' - use_geo: Which geo variable has been used (here: none)\cr
#' - use_age: Is a persons age used for prediction (here: FALSE)\cr
#' - use_sex: Is a persons sex used for prediction (here: FALSE)\cr
#' - prob_asian: Probability of the person being asian\cr
#' - prob_black: Probability of the person being black\cr
#' - prob_hispanic: Probability of the person being hispanic\cr
#' - prob_white: Probability of the person being white\cr
#' - prob_other: Probability of the person being non of the above races\cr
#'
#' IMPORTANT: Non-Predictions are omited from the output
#' @export
# DEBUG
# .tab = name_table
# .use_geo = "county"
# .use_age = TRUE
# .use_sex = TRUE
# .census = download_census(.dir = "cache_census_data/", .geo = "county")
race_wru <- function(.tab, .use_geo = NULL, .use_age = FALSE, .use_sex = FALSE, .census = NULL) {

  last_name <- sex <- age <- pred.asi <- pred.bla <- pred.his <- pred.whi <-
    pred.oth <- prob_asian <- NULL


  # Checks ------------------------------------------------------------------
  if (!"id" %in% colnames(.tab)) {
    stop("Name Table (.tab) must have a unique column ID")
  }

  if (any(duplicated(.tab[["id"]]))) {
    stop("Name Table (.tab) must have a unique column ID")
  }


  # Rename First Name for WRU Package ---------------------------------------
  tab_in_ <- dplyr::mutate(.tab, surname = gsub("[^[:alnum:] ]", "", last_name))

  # Surname Only ------------------------------------------------------------
  if (is.null(.use_geo)) {
    tab_ <- wru::predict_race(
      voter.file = tab_in_,
      surname.only = TRUE
    ) %>% dplyr::mutate(use_geo = "none", use_age = FALSE, use_sex = FALSE) %>%
      quiet()
  }

  # Surname & Geo -----------------------------------------------------------
  if (!is.null(.use_geo)) {
    tab_ <- wru::predict_race(
      voter.file = tab_in_,
      census.geo = .use_geo,
      census.data = purrr::flatten(dplyr::filter(.census, !age, !sex)$value)
    ) %>% dplyr::mutate(use_geo = .use_geo, use_age = FALSE, use_sex = FALSE) %>%
      quiet()
  }

  # Surname & Geo & Age -----------------------------------------------------
  if (!is.null(.use_geo) & .use_age) {
    tab_ <- wru::predict_race(
      voter.file = dplyr::filter(tab_in_, !is.na(age)),
      census.geo = .use_geo,
      census.data = purrr::flatten(dplyr::filter(.census, age, !sex)$value),
      age = TRUE
    ) %>% dplyr::mutate(use_geo = .use_geo, use_age = TRUE, use_sex = FALSE) %>%
      quiet()
  }


  # Surname & Geo & Sex ------------------------------------------------------
  if (!is.null(.use_geo) & .use_sex) {
    tab_ <- wru::predict_race(
      voter.file = dplyr::filter(tab_in_, !is.na(sex)),
      census.geo = .use_geo,
      census.data = purrr::flatten(dplyr::filter(.census, !age, sex)$value),
      sex = TRUE
    ) %>% dplyr::mutate(use_geo = .use_geo, use_age = FALSE, use_sex = TRUE) %>%
      quiet()
  }


  # Surname & Geo & Age & Sex -----------------------------------------------
  if (!is.null(.use_geo) & .use_age & .use_sex) {
    tab_ <- wru::predict_race(
      voter.file = dplyr::filter(tab_in_, !is.na(age), !is.na(sex)),
      census.geo = .use_geo,
      census.data = purrr::flatten(dplyr::filter(.census, age, sex)$value),
      age = TRUE,
      sex = TRUE
    ) %>% dplyr::mutate(use_geo = .use_geo, use_age = TRUE, use_sex = TRUE) %>%
      quiet()
  }

  # Prepare Output ----------------------------------------------------------
  tab_ <- dplyr::mutate(tab_, method = "wru", use_name = "last_name") %>%
    dplyr::rename(
      prob_asian = pred.asi,
      prob_black = pred.bla,
      prob_hispanic = pred.his,
      prob_white = pred.whi,
      prob_other = pred.oth
    ) %>%
    dplyr::filter(!is.na(prob_asian))

  # Select and Reorder Columns ----------------------------------------------
  cn_ <- colnames(tab_)
  tab_ <- tab_[, c(colnames(.tab), "method", cn_[grepl("use_", cn_)], sort(cn_[grepl("prob_", cn_)]))]
  tibble::as_tibble(tab_)
}

#' Download Census Data
#'
#' @param .key
#' A required character object containing a valid Census API key, which can be requested at
#' \href{https://api.census.gov/data/key_signup.html}{api.census.gov}
#' @param .geo Either "county", "tract", "block", or "place"
#' @param .dir Folder to store data
#' @param .workers Number of paralell workers for downloading data
#' @param .retry The number of retries at the census website if network interruption occurs.
#' @param .progress Progress Bar
#'
#' @return A Dataframe
#' @export

# DEBUG
# .key <- "8d569f4a70e1abf84cd23350522b479216d008c4"
# .geo <- "county"
# .dir <- "cache_census_data"
# .workers <- 20
# .retry <- 20
# .progress <- TRUE
download_census <- function(.key = "", .geo, .dir, .workers = 1, .retry = 10, .progress = FALSE) {
  state <- age <- sex <- id <- name <- value <- NULL

  dir_ <- file.path(.dir, .geo)
  dir.create(dir_, FALSE, TRUE)

  combs_ <- tidyr::expand_grid(
    state = get_states(),
    age = c(TRUE, FALSE),
    sex = c(TRUE, FALSE)
  ) %>% dplyr::mutate(
    id = paste(state, age, sex, sep = "-"),
    dplyr::across(dplyr::everything(), ~ purrr::set_names(., id)
    ))

  fils_ <- lft(dir_)
  new_ <- dplyr::filter(combs_, !id %in% fils_$doc_id)

  if (nrow(new_) > 0) {
    message(paste0("Downloading ", nrow(new_), " State-Age-Gender Census Data"))

    future::plan("multisession", workers = .workers)
    furrr::future_pwalk(
      .l = list(combs_$state, combs_$age, combs_$sex, combs_$id),
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

  fils_ <- lft(dir_)
  purrr::map(fils_$path, readr::read_rds) %>%
    tibble::enframe() %>%
    tidyr::separate(name, c("state", "age", "sex"), convert = TRUE)  %>%
    dplyr::group_by(age, sex) %>%
    dplyr::summarise(value = list(value), .groups = "drop") %>%
    dplyr::mutate(value = purrr::map(value, ~ purrr::flatten(.x)))
}


#' Predict Race
#'
#' @param .tab Input Table (see details)
#' @param .packages Which package to use, either prr (predictrace) or wru (WRU)
#' @param .prr_use Which name variables to use in prr? First Name (first_name) or Last Name (last_name)
#' @param .wru_use_geo
#' Which geo variable has been used in wru (Either "county", "tract", "block", or "place").
#' If used must be the same as in download_census()
#' @param .wru_use_age Should race be inferred from information about a persons age (wru)?
#' @param .wru_use_sex Should race be inferred from information about a persons sex (wru)?
#' @param .wru_cenus Only needed if .use_geo is not NULL (wru)
#'
#' @return
#' The original data frame (.tab in long format) appended with the following columns:\cr
#' - method: Used methods (here: prr for predictrace)\cr
#' - use_name: which name variable has been used (here: first/last)\cr
#' - use_geo: Which geo variable has been used (here: none)\cr
#' - use_age: Is a persons age used for prediction (here: FALSE)\cr
#' - use_sex: Is a persons sex used for prediction (here: FALSE)\cr
#' - prob_asian: Probability of the person being asian\cr
#' - prob_black: Probability of the person being black\cr
#' - prob_hispanic: Probability of the person being hispanic\cr
#' - prob_white: Probability of the person being white\cr
#' - prob_other: Probability of the person being non of the above races\cr
#' - highest_prob: Highest probability of the race predictions
#' - guess_diff: difference between the highest and second highest prediction
#' - race: the race of the highest prediction
#'
#' IMPORTANT: Non-Predictions are omited from the output
#' @export
.tab = name_table
.packages = c("prr", "wru")
.prr_use = c("first_name", "last_name")
.wru_use_geo = NULL
.wru_use_age = FALSE
.wru_use_sex = FALSE
.wru_census = NULL
race_predict <- function(.tab, .packages = c("prr", "wru"),
                         .prr_use = c("first_name", "last_name"),
                         .wru_use_geo = NULL, .wru_use_age = FALSE,
                         .wru_use_sex = FALSE, .wru_cenus = NULL) {

  prob_white <- prob_other <- prob_black <- prob_hispanic <- prob_asian <-
    highest_prob <- guess_diff <- race <- id <- NULL
  lst_ <- list()

  # Checks ------------------------------------------------------------------
  if (!"id" %in% colnames(.tab)) {
    stop("Name Table (.tab) must have a unique column ID")
  }

  if (any(duplicated(.tab[["id"]]))) {
    stop("Name Table (.tab) must have a unique column ID")
  }

  if ("prr" %in% .packages) {
    lst_[[1]] <- race_prr(.tab, .prr_use)
  }

  if ("wru" %in% .packages) {
    lst_[[2]] <- race_wru(.tab, NULL, FALSE, FALSE, NULL)

    if (!is.null(.wru_use_geo)) {
      lst_[[3]] <- race_wru(.tab, .wru_use_geo, FALSE, FALSE, .wru_cenus)


      if (.wru_use_age) {
        lst_[[4]] <- race_wru(.tab, .wru_use_geo, TRUE, FALSE, .wru_cenus)
      }

      if (.wru_use_sex) {
        lst_[[5]] <- race_wru(.tab, .wru_use_geo, FALSE, TRUE, .wru_cenus)
      }

      if (.wru_use_age & .wru_use_sex) {
        lst_[[6]] <- race_wru(.tab, .wru_use_geo, TRUE, TRUE, .wru_cenus)
      }
    }
  }

  dplyr::bind_rows(lst_) %>%
    dplyr::filter(!is.na(prob_white)) %>%
    dplyr::mutate(
      highest_prob = pmax(prob_other, prob_white, prob_black, prob_asian, prob_hispanic, na.rm = TRUE),
      race = dplyr::case_when(
        prob_asian == highest_prob ~ "asian",
        prob_other == highest_prob ~ "other",
        prob_black == highest_prob ~ "black",
        prob_hispanic == highest_prob ~ "hispanic",
        prob_white == highest_prob ~ "white"
      ),
      dplyr::across(dplyr::starts_with("prob_"), ~ dplyr::if_else(. == highest_prob, NA_real_, .)),
      guess_diff = pmax(prob_other, prob_white, prob_black, prob_asian, prob_hispanic, na.rm = TRUE),
      guess_diff = highest_prob - guess_diff,
      dplyr::across(dplyr::starts_with("prob_"), ~ dplyr::if_else(is.na(.), highest_prob, .)),
    ) %>%
    dplyr::relocate(race, .after = guess_diff) %>%
    dplyr::arrange(id)

}
