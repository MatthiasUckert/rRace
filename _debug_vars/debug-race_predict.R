tab_wru <- race_wru(
  .tab = name_table,
  .use_geo = FALSE,
  .use_age = FALSE,
  .use_gen = FALSE,
  .census_dir = "_debug_data/census/",
  .census_geo = "county"
    )
tab_ppr <- race_prr(name_table)
dots_ <- dplyr::quos(tab_prr, tab_wru)
