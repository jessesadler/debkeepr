## Utility functions ##

# To help deal with floating point problems in normalize
# and transaction functions
should_be_int <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# Used in deb_account_summary, deb_credit, and deb_debit if statement
# Whether all accounts have both a credit and debit transaction
all_present <- function(pos, neg) {
  all(pos[[1]] %in% neg[[1]], neg[[1]] %in% pos[[1]])
}

# For coercion of different units in deb_decimal
# Hierarchy: d -> s -> l
unit_hierarchy <- function(x, y) {
  if (identical(deb_unit(x), deb_unit(y))) {
    deb_unit(x)
  } else if (any(c(deb_unit(x), deb_unit(y)) == "l")) {
    "l"
  } else {
    "s"
  }
}
