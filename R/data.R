## Documentation for data ##

#' Accounts from the practice journal and ledger of Richard Dafforne
#'
#' A data set of the accounts from the first practice journal and ledger in
#' Richard Dafforne's accounting manual from 1660 called *The Merchant's*
#' *Mirrour*. By 1660 *The Merchant's Mirrour* was in its third edition, and
#' its contents had been printed in the well-known merchant manual of Gerard
#' Malynes, *Consuetudo Vel Lex Mercatoira* since the 1636 edition, making it
#' one of the most popular bookkeeping manuals in 17th-century England. The
#' data set is meant to be used in conjunction with `dafforne_transactions`.
#' It contains information on the accounts found in the practice journal and
#' ledger that Dafforne used to teach double-entry bookkeeping practices.
#'
#' The data set does not include the Balance account that Dafforne uses to
#' close the books. The transactions from this account can be recreated using
#' the lsd account functions in `debkeepr`.
#'
#' @format A data frame with 46 rows and 5 variables.
#'
#' @section Variables:
#'
#'   - `id`: Numeric id for each account. The ids correspond to the ids in the
#'     `credit` and `debit` variables in `dafforne_transactions`.
#'   - `account`: Name of the account.
#'   - `ledger`: Page on which the account appears in the ledger.
#'   - `investor`: The investor or the person's whose capital is involved in
#'     the account. Accounts that only deal with the bookkeeper's capital are
#'     listed as "ego".
#'   - `description`: Short description of each account.
#'
#' @source Richard Dafforne, *The Merchant's Mirrour, Or Directions for the*
#'   *Perfect Ordering and Keeping of His Accounts*, Third Edition, (London,
#'   1660)
"dafforne_accounts"

#' Transactions from the practice journal and ledger of Richard Dafforne
#'
#' A data set of the transactions from the first practice journal and ledger in
#' Richard Dafforne's accounting manual from 1660 called *The Merchant's*
#' *Mirrour*. By 1660 *The Merchant's Mirrour* was in its third edition, and
#' its contents had been printed in the well-known merchant manual of Gerard
#' Malynes, *Consuetudo Vel Lex Mercatoira* since the 1636 edition, making it
#' one of the most popular bookkeeping manuals in 17th-century England. The
#' data set is meant to be used in conjunction with `dafforne_accounts`. It
#' contains the transactions in the practice journal and ledger that Dafforne
#' used to teach double-entry bookkeeping practices.
#'
#' The data set does not include the last 16 transactions recorded in the
#' journal, which deal with the balancing of the book. These transactions can
#' be recreated using the lsd account functions in `debkeepr`.
#'
#' @format A data frame with 177 rows and 8 variables.
#'
#' @section Variables:
#'
#'   - `id`: Numeric id for each transaction.
#'   - `credit`: Account id for the credit account in the transactions. The
#'     accounts that discharges the transactional value or from which the
#'     value derives. The account ids correspond to the `id` variable in
#'     `dafforne_accounts`.
#'   - `debit`: Account id for the debit account in the transactions. The
#'     accounts that receive the transactional value. The account ids
#'     correspond to the `id` variable in `dafforne_accounts`.
#'   - `date`: Date on which the transaction was entered into the journal. Date
#'     conforms to the Anglican calendar that used the old Julian calendar with
#'     the new year on 25 March. Encoded as a date vector.
#'   - `lsd`: Column of class `deb_lsd` with pounds, shillings, and pence
#'     values. Bases for shillings and pence are 20 and 12 respectively.
#'   - `journal`: Page on which the transaction is recorded in the journal.
#'   - `ledger`: The pages on which the transaction is recorded in the ledger.
#'     The number before the slash is the page on which the debit is recorded.
#'     The number after the slash is the page on which the credit is recorded.
#'   - `description`: Description of the transaction as recorded in the
#'     journal.
#'
#' @source Richard Dafforne, *The Merchant's Mirrour, Or Directions for the*
#'   *Perfect Ordering and Keeping of His Accounts*, Third Edition, (London,
#'   1660)
"dafforne_transactions"
