## lsd-mutate-columns ##

# Helper function that does the heavy lifting for other functions
# that take l, s, and d columns, use mutate to modify the values
# and create new l, s, and d columns.
#
# The calculations are done by changing to denarii
#
# This must be used with lsd_column_names function to create lsd_names

lsd_mutate_columns <- function(df,
                               l, s, d,
                               lsd_names,
                               round) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  dplyr::mutate(df,
                !! lsd_names[1] := deb_librae(!! l, !! s, !! d),
                !! lsd_names[2] := deb_solidi(!! l, !! s, !! d),
                !! lsd_names[3] := deb_denarii(!! l, !! s, !! d, round))
}
