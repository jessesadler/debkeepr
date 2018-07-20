## lsd-mutate-columns ##

# Basis for deb_normalize_df
# Helper function that does the heavy lifting for other functions
# that take l, s, and d columns, use mutate to modify the values
# and create new l, s, and d columns.

## Helper functions to normalize separate l, s, and d ##

## Librae ##
deb_librae <- function(l, s, d, lsd_bases = c(20, 12)) {
  if (length(l) > 1) {
    return(purrr::pmap_dbl(list(l, s, d), ~ deb_librae(..1, ..2, ..3, lsd_bases)))
  }

  lsd <- lsd_decimal_check(c(l, s, d), lsd_bases)

  librae <- lsd[1] + ((lsd[2] + lsd[3] %/% lsd_bases[2]) %/% lsd_bases[1])

  # Case when denarii rounds up to its base and makes solidi round up to its base
  if (!is.na(librae) &&
      dplyr::near(round(lsd[3] %% lsd_bases[2], 5), lsd_bases[2]) &&
      dplyr::near((lsd[2] + 1 + lsd[3] %/% lsd_bases[2]) %% lsd_bases[1], 0)) {
    librae <- librae + 1
  }

  dplyr::if_else(l + s / lsd_bases[1] + d / prod(lsd_bases) > 0, librae, -librae)
}

## Solidi ##
deb_solidi <- function(l, s, d, lsd_bases = c(20, 12)) {
  if (length(l) > 1) {
    return(purrr::pmap_dbl(list(l, s, d), ~ deb_solidi(..1, ..2, ..3, lsd_bases)))
  }

  lsd <- lsd_decimal_check(c(l, s, d), lsd_bases)

  solidi <- (lsd[2] + lsd[3] %/% lsd_bases[2]) %% lsd_bases[1]

  # Case when denarii rounds up to its base and if solidi goes up to is base
  if (!is.na(solidi) && dplyr::near(round(lsd[3] %% lsd_bases[2], 5), lsd_bases[2])) {
    solidi <- solidi + 1
    if (dplyr::near(solidi, lsd_bases[1])) {
      solidi <- 0
    }
  }

  dplyr::if_else(l + s / lsd_bases[1] + d / prod(lsd_bases) > 0, solidi, -solidi)
}

## Denarii ##
deb_denarii <- function(l, s, d, lsd_bases = c(20, 12)) {
  if (length(l) > 1) {
    return(purrr::pmap_dbl(list(l, s, d), ~ deb_denarii(..1, ..2, ..3, lsd_bases)))
  }

  lsd <- lsd_decimal_check(c(l, s, d), lsd_bases)

  denarii <- round(lsd[3] %% lsd_bases[2], 5)

  if (!is.na(denarii) && dplyr::near(denarii, lsd_bases[2])) {
    denarii <- 0
  }

  dplyr::if_else(l + s / lsd_bases[1] + d / prod(lsd_bases) > 0, denarii, -denarii)
}

## Mutate columns to l, s, and d ##
# This must be used with lsd_column_names function to create lsd_names
# Should be the last step in a data frame function

lsd_mutate_columns <- function(df,
                               l, s, d,
                               lsd_names,
                               replace,
                               lsd_bases) {
  bases_check(lsd_bases)

  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  if (replace == FALSE) {
    dplyr::mutate(df,
                  !! lsd_names[1] := deb_librae(!! l, !! s, !! d, lsd_bases),
                  !! lsd_names[2] := deb_solidi(!! l, !! s, !! d, lsd_bases),
                  !! lsd_names[3] := deb_denarii(!! l, !! s, !! d, lsd_bases))
  } else {
    ret <- dplyr::mutate(df,
                         temp_librae_col = deb_librae(!! l, !! s, !! d, lsd_bases),
                         temp_solidi_col = deb_solidi(!! l, !! s, !! d, lsd_bases),
                         temp_denarii_col = deb_denarii(!! l, !! s, !! d, lsd_bases)) %>%
      # Get rid of original columns,
      # because they do not get overwritten with tidyeval
      dplyr::select(-!! lsd_names[1], -!! lsd_names[2], -!! lsd_names[3]) %>%
      dplyr::rename(!! lsd_names[1] := temp_librae_col,
                    !! lsd_names[2] := temp_solidi_col,
                    !! lsd_names[3] := temp_denarii_col)

    # Get rid of no visible binding for global variable from CMD check
    temp_librae_col <- NULL
    temp_solidi_col <- NULL
    temp_denarii_col <- NULL

    ret
  }

}
