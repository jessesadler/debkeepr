## Clean and import dafforne transactions to data/ ##

library(readr)
library(dplyr)

dafforne_transactions <- readr::read_csv("data-raw/dafforne-transactions.csv",
                                col_types =  list(
                                  date = col_date(format = "%Y%m%d"),
                                  l = col_double(),
                                  s = col_double(),
                                  d = col_double())) %>%
  dplyr::filter(id <= 175)

usethis::use_data(dafforne_transactions, overwrite = TRUE)
