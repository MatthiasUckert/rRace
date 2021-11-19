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
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
    "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
    "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
    "UT", "VT", "VA", "WA", "WV", "WI", "WY"
  )
}

# merge_surnames <- wru::merge_surnames
