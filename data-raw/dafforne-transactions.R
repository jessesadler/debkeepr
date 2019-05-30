## Clean and import dafforne transactions to data ##

library(readr)
library(dplyr)


dafforne_transactions <- readr::read_csv("data-raw/dafforne-transactions.csv",
                                col_types =  list(
                                  date = col_date(format = "%Y%m%d"),
                                  l = col_double(),
                                  s = col_double(),
                                  d = col_double())) %>%
  dplyr::filter(id <= 175)

# Temporary change to create new lsd class when mutate does not work with vctrs
dafforne_transactions[["lsd"]] <- debkeepr::deb_lsd(dafforne_transactions$l,
                                                    dafforne_transactions$s,
                                                    dafforne_transactions$d)
dafforne_transactions <- select(dafforne_transactions,
                                id, credit, debit, date, lsd, journal, ledger, description)

usethis::use_data(dafforne_transactions, overwrite = TRUE)
