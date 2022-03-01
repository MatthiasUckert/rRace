library(reticulate)
# reticulate::conda_create(envname = "ethn", python_version = "3.7")
reticulate::use_condaenv(condaenv = "ethn")

os <- import("os")
pd <- import("pandas")
et <- import("ethnicolr")



et$


conda_python()
