## lsd-mutate-columns ##

# Basis for deb_normalize_df
# Helper function that does the heavy lifting for other functions
# that take l, s, and d columns, use mutate to modify the values
# and create new l, s, and d columns.

## Helper functions to normalize separate l, s, and d ##

## Librae ##
deb_librae <- function(l, s, d) {
  if (length(l) > 1) {
    return(purrr::pmap_dbl(list(l, s, d), deb_librae))
  }

  lsd <- lsd_decimal_check(c(l, s, d))

  librae <- lsd[1] + ((lsd[2] + lsd[3] %/% 12) %/% 20)
  dplyr::if_else(l + s/20 + d/240 > 0, librae, -librae)
}

## Solidi ##
deb_solidi <- function(l, s, d) {
  if (length(l) > 1) {
    return(purrr::pmap_dbl(list(l, s, d), deb_solidi))
  }

  lsd <- lsd_decimal_check(c(l, s, d))

  solidi <- (lsd[2] + lsd[3] %/% 12) %% 20
  dplyr::if_else(l + s/20 + d/240 > 0, solidi, -solidi)
}

## Denarii ##
deb_denarii <- function(l, s, d, round) {
  if (length(l) > 1) {
    return(purrr::pmap_dbl(list(l, s, d), ~ deb_denarii(..1, ..2, ..3, round)))
  }

  lsd <- lsd_decimal_check(c(l, s, d))

  denarii <- round(lsd[3] %% 12, round)
  dplyr::if_else(l + s/20 + d/240 > 0, denarii, -denarii)
}

## Mutate columns to l, s, and d ##
# This must be used with lsd_column_names function to create lsd_names
# Should be the last step in a data frame function

lsd_mutate_columns <- function(df,
                               l, s, d,
                               lsd_names,
                               replace,
                               round) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  if (replace == FALSE) {
    dplyr::mutate(df,
                  !! lsd_names[1] := deb_librae(!! l, !! s, !! d),
                  !! lsd_names[2] := deb_solidi(!! l, !! s, !! d),
                  !! lsd_names[3] := deb_denarii(!! l, !! s, !! d, round))
  } else {
    dplyr::mutate(df,
                  temp_librae_col = deb_librae(!! l, !! s, !! d),
                  temp_solidi_col = deb_solidi(!! l, !! s, !! d),
                  temp_denarii_col = deb_denarii(!! l, !! s, !! d, round)) %>%
      # Get rid of original columns,
      # because they do not get overwritten with tidyeval
      dplyr::select(-!! lsd_names[1], -!! lsd_names[2], -!! lsd_names[3]) %>%
      dplyr::rename(!! lsd_names[1] := temp_librae_col,
                    !! lsd_names[2] := temp_solidi_col,
                    !! lsd_names[3] := temp_denarii_col)
  }

}
