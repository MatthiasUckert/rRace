library(reticulate); library(tidyverse)

.py_path <- filter(conda_list(), name == "env-ethnicolr")[[2]]
Sys.setenv(RETICULATE_PYTHON = .py_path)
Sys.getenv("RETICULATE_PYTHON")
reticulate::py_discover_config()
reticulate::source_python(system.file("extdata/ethnicolr.py", package = "rRace"))

tab_ <- name_table
suppressWarnings(tibble::as_tibble(race_eth2(tab_)))
