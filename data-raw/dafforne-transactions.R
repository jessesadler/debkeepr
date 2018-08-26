## Clean and import dafforne transactions to data/ ##

library(readr)
library(dplyr)
library(debkeepr)

dafforne_transactions <- readr::read_csv("data-raw/dafforne-transactions.csv",
                                col_types =  list(
                                  date = col_date(format = "%Y%m%d"),
                                  l = col_double(),
                                  s = col_double(),
                                  d = col_double())) %>%
  dplyr::filter(id <= 175)

dafforne_transactions <- deb_lsd_gather(dafforne_transactions,
                                        lsd_column = lsd,
                                        replace = TRUE) %>%
  dplyr::select(id, credit, debit, date, lsd, journal, ledger, description)

usethis::use_data(dafforne_transactions, overwrite = TRUE)
