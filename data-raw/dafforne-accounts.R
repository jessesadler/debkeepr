## Clean and import dafforne accounts to data/ ##

library(readr)
library(dplyr)

dafforne_accounts <- readr::read_csv("data-raw/dafforne-accounts.csv") %>%
  dplyr::filter(id != 47)

usethis::use_data(dafforne_accounts, overwrite = TRUE)
