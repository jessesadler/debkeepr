## lsd-mutate-columns ##

# Helper function that does the heavy lifting for other functions
# that take l, s, and d columns, use mutate to modify the values
# and create new l, s, and d columns.
#
# The calculations are done by changing to denarii
#
# This should be used with lsd_column_names function to create lsd_names

lsd_mutate_columns <- function(df,
                               l, s, d,
                               lsd_names,
                               round = 3) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  dplyr::mutate(df,
                temp_denarii = deb_lsd_d(!! l, !! s, !! d),
                !! lsd_names[1] := deb_denarii_l(temp_denarii),
                !! lsd_names[2] := deb_denarii_s(temp_denarii),
                !! lsd_names[3] := deb_denarii_d(temp_denarii, round)) %>%
    dplyr::select(-temp_denarii)
}
