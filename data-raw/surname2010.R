## code to prepare `surnames2010` dataset goes here
library(tidyverse)
surnames2010 <- wru::surnames2010
usethis::use_data(surnames2010, overwrite = TRUE)
