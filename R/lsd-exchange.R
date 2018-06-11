## lsd conversions ##

# Exchange rate by shillings
# This is set up to go from sterling to vlaams
deb_exchange <- function(l, s, d,
                         rate_per_solidi,
                         round = 3,
                         vector = FALSE) {
  deb_multiply(x = rate_per_solidi/20,
               l, s, d,
               round = round,
               vector = vector)
}


deb_exchange_mutate <- function(df,
                                l = l,
                                s = s,
                                d = d,
                                rate_per_solidi,
                                suffix = ".exchange",
                                round = 3) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  lsd_column_check(df, l, s, d)
  # Column names: avoid overwriting l, s, and d columns
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  x <- rate_per_solidi/20

  lsd_mutate_columns(df,
                     !! l * x,
                     !! s * x,
                     !! d * x,
                     lsd_names,
                     round = round)
}
