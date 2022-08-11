#' Helper Function: Check if ID is present in datafram
#'
#' @param .tab same as in race_...()
#'
#' @return Nothing or Error
check_id <- function(.tab) {
  if (!"id" %in% colnames(.tab)) {
    tab_name_ <- deparse(substitute(.tab))
    msg_ <- "Name Table: `{tab_name_}` must have a unique column ID (column name: `id`)"
    stop(glue::glue(msg_), call. = FALSE)
  }

  if (any(duplicated(.tab[["id"]]))) {
    tab_name_ <- deparse(substitute(.tab))
    msg_ <- "Name Table: `{tab_name_}` must have a unique column ID (column name: `id`)"
    stop(glue::glue(msg_), call. = FALSE)
  }
}

#' Helper Function: Check census dir
#'
#' @param .census_geo same as in download_census()
#' @param .census_dir same as in download_census()
#'
#' @return Nothing or Error
check_census_dir <- function(.census_geo, .census_dir) {
  if (!is.null(.census_geo) & is.null(.census_dir)) {
    msg_ <- "To use geo variables you need to specify the '.census_dir' directory"
    stop(msg_, call. = FALSE)
  }
}

#' Helper Function: Check if geo columns are present
#'
#' @param .tab same as in race_...()
#' @param .census_geo same as in race_...()
#'
#' @return Nothing or Error
check_geo_columns <- function(.tab, .census_geo) {
  geos_ <- c("county", "tract", "block", "place")

  if (!is.null(.census_geo)) {
    if (!.census_geo %in% geos_) {
      msg_ <- '.census_geo must be one of c("county", "tract", "block", "place")'
      stop(glue::glue(msg_), call. = FALSE)
    }
  }

  for (g in c("county", "tract", "block", "place")) {
    if (.census_geo == g) {
      check_ <- all(c(g, "state") %in% colnames(.tab))
      tab_name_ <- deparse(substitute(.tab))
      if (!check_) {
        msg_ <- "If .census_geo == {g}, Name Table: `{tab_name_}` must have columns 'state' and {g}"
        stop(glue::glue(msg_), call. = FALSE)
      }
    }
  }
}

#' Helper Function: get combinations for race_wru()
#'
#' @param .use_age same as in race_wru()
#' @param .use_gen same as in race_wru()
#'
#' @return A datafram
wru_get_combinations <- function(.use_geo, .use_age, .use_gen) {

  # Assign NULL to Global Vars -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  use_geo <- use_age <- use_gen <- NULL

  tidyr::expand_grid(
    use_geo = unique(c(FALSE, .use_geo)),
    use_age = unique(c(FALSE, .use_age)),
    use_gen = unique(c(FALSE, .use_gen))
  ) %>%
    dplyr::filter(!(!use_geo & (use_age | use_gen))) %>% # Gender and Age can only be used with Geo Variables
    dplyr::mutate(
      id = paste(use_geo, use_age, use_gen, sep = "-"),
      dplyr::across(c(use_geo, use_age, use_gen), ~ purrr::set_names(., id))
    )
}

#' Helper Function: read and check census files for race_wru()
#'
#' @param .tab same as in race_wru()
#' @param .census_dir same as in race_wru()
#' @param .census_geo same as in race_wru()
#' @param .use_age same as in race_wru()
#' @param .use_gen same as in race_wru()
#'
#' @return A datafram
wru_read_check_census <- function(.tab, .census_dir, .census_geo, .use_age, .use_gen) {

  # Assign NULL to Global Vars -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  doc_id <- name <- age <- gen <- value <- NULL

  # Read Files -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  fils_ <- lft(file.path(.census_dir, .census_geo)) %>%
    tidyr::separate(doc_id, c("state", "age", "gen"), convert = TRUE)

  # Select only relevant Files -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  expect_ <- tidyr::expand_grid(
    state = unique(.tab$state),
    age = unique(c(FALSE, .use_age)),
    gen = unique(c(FALSE, .use_gen))
  ) %>% dplyr::left_join(fils_, by = c("state", "age", "gen"))

  # Check for missing Files -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (any(is.na(expect_$path))) {
    stop("Not all Census Data downloaded, please download with download_census()", call. = FALSE)
  }

  # Read selected Files -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  purrr::map(expect_$path, readr::read_rds) %>%
    tibble::enframe() %>%
    tidyr::separate(name, c("state", "age", "gen"), convert = TRUE)  %>%
    dplyr::group_by(age, gen) %>%
    dplyr::summarise(value = list(value), .groups = "drop") %>%
    dplyr::mutate(value = purrr::map(value, ~ purrr::flatten(.x)))
}

#' Check for Column Names
#'
#' @param .tab The input table
#' @param .require Required column names
#'
#' @return Throws an error if required column is not present in the dataset
#.require <- c("first_name", "last_name", "middle_name")
check_cols <- function(.tab, .require) {
  cols_ <- colnames(.tab)

  for (i in .require) {
    if (!i %in% cols_) {
      stop(
        paste0("Column '", i, "' must be present in the dataset."),
        call. = FALSE
      )
    }
  }
}

#' Suppress Messages and Warnings
#'
#' @param ... Any expression
#' @param messages ...
#' @param cat ...
#'
#' @return Any object
quiet <- function(..., messages = FALSE, cat = FALSE) {
  if (!cat) {
    sink(tempfile())
    on.exit(sink())
  }
  out <- if (messages) eval(...) else suppressMessages(eval(...))
  out
}


#' List Files in Dataframe
#'
#' @param .dirs Full paths to folders
#' @param .reg Regulare Expression for file names
#' @param .rec Recursive search
#'
#' @return A Dataframe
lft <- function(.dirs, .reg = "*", .rec = FALSE) {
  path <- file_ext <- doc_id <- NULL

  purrr::map_dfr(
    .x = .dirs,
    .f = ~ tibble::tibble(path = list.files(.x, .reg, F, T, .rec))
  ) %>%
    dplyr::mutate(
      file_ext = paste0(".", tools::file_ext(path)),
      doc_id = stringi::stri_replace_all_fixed(basename(path), file_ext, ""),
      path = purrr::set_names(path, doc_id)
    ) %>%
    dplyr::select(doc_id, file_ext, path)
}

#' Retrieve US state Abbreviations
#'
#' @return A character vector
get_states <- function() {
  c(
    "ALABAMA" = "AL",
    "ALASKA" = "AK",
    "ARIZONA" = "AZ",
    "ARKANSAS" = "AR",
    "CALIFORNIA" = "CA",
    "COLORADO" = "CO",
    "CONNECTICUT" = "CT",
    "DELAWARE" = "DE",
    "DISTRICT OF COLUMBIA" = "DC",
    "FLORIDA" = "FL",
    "GEORGIA" = "GA",
    "HAWAII" = "HI",
    "IDAHO" = "ID",
    "ILLINOIS" = "IL",
    "INDIANA" = "IN",
    "IOWA" = "IA",
    "KANSAS" = "KS",
    "KENTUCKY" = "KY",
    "LOUISIANA" = "LA",
    "MAINE" = "ME",
    "MARYLAND" = "MD",
    "MASSACHUSETTS" = "MA",
    "MICHIGAN" = "MI",
    "MINNESOTA" = "MN",
    "MISSISSIPPI" = "MS",
    "MISSOURI" = "MO",
    "MONTANA" = "MT",
    "NEBRASKA" = "NE",
    "NEVADA" = "NV",
    "NEW HAMPSHIRE" = "NH",
    "NEW JERSEY" = "NJ",
    "NEW MEXICO" = "NM",
    "NEW YORK" = "NY",
    "NORTH CAROLINA" = "NC",
    "NORTH DAKOTA" = "ND",
    "OHIO" = "OH",
    "OKLAHOMA" = "OK",
    "OREGON" = "OR",
    "PENNSYLVANIA" = "PA",
    "PUERTO RICO" = "PR",
    "RHODE ISLAND" = "RI",
    "SOUTH CAROLINA" = "SC",
    "SOUTH DAKOTA" = "SD",
    "TENNESSEE" = "TN",
    "TEXAS" = "TX",
    "UTAH" = "UT",
    "VERMONT" = "VT",
    "VIRGINIA" = "VA",
    "WASHINGTON" = "WA",
    "WEST VIRGINIA" = "WV",
    "WISCONSIN" = "WI",
    "WYOMING" = "WY"
  )

}
