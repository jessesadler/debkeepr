## Create and separate a deb_lsd column ##

#' Helpers to create and separate a `deb_lsd` column in a data frame
#'
#' @description
#' - `deb_gather_lsd()` creates a `deb_lsd` column from separate variables
#'   representing pounds, shillings, and pence values.
#' - `deb_spread_lsd()` creates separate variables for pounds, shillings,
#'   and pence from a `deb_lsd` column.
#'
#' If transcribing historical accounting data by hand, entering the pounds,
#' shillings, and pence values (lsd) into separate columns is probably the
#' easiest and least error prone method. `deb_gather_lsd()` creates a
#' `deb_lsd` column from the three separate `l`, `s`, and `d` columns.
#' `deb_spread_lsd()` does the opposite. It takes a `deb_lsd` column and
#' spreads it into three separate pounds, shillings, and pence columns.
#'
#' Values for column names (`lsd_col`, `l_col`, `s_col`, and `d_col`) must
#' be valid column names. They can be quoted or unquoted, but they cannot be
#' vectors or bare numbers. This follows the rules of [dplyr::rename()].
#'
#' @param df A data frame.
#' @param l Pounds column: Unquoted name of a numeric variable corresponding
#'   to the pounds or libra unit. Default is `l`.
#' @param s Shillings column: Unquoted name of numeric variable corresponding
#'   to the shillings or solidus unit. Default is `s`.
#' @param d Pence column: Unquoted name of numeric variable corresponding to
#'   the pence or denarius unit. Default is `d`.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#' @param lsd_col Unquoted name of the `deb_lsd` column create. Default
#'   is `lsd`.
#' @param replace Logical (default `FALSE`). When `TRUE`, the newly created
#'   column(s) will replace the one(s) used to create it/them.
#' @param lsd `deb_lsd` column: Unquoted name of a `deb_lsd` column.
#'   Default is `lsd`.
#' @param l_col An unquoted name for the pounds column created by the
#'   function. Default is `l`.
#' @param s_col An unquoted name for the shillings column created by
#'   the function. Default is `s`.
#' @param d_col An unquoted name for the pence column created by the
#'   function. Default is `d`.
#'
#' @examples
#'
#' libra <- c(3, 5, 6, 2)
#' solidus <- c(10, 18, 11, 16)
#' denarius <- c(9, 11, 10, 5)
#'
#' # data frame with separate l, s, and d variables and default names
#' x <- data.frame(accounts = c(1, 2, 3, 4),
#'                 l = libra,
#'                 s = solidus,
#'                 d = denarius)
#'
#' # data frame with deb_lsd variable and default names
#' y <- data.frame(accounts = c(1, 2, 3, 4),
#'                 lsd = deb_lsd(l = libra,
#'                               s = solidus,
#'                               d = denarius))
#'
#' # Gather l, s, and d variables into deb_lsd column
#' deb_gather_lsd(x, l = l, s = s, d = d)
#'
#' # Spread deb_lsd column into separate l, s, and d columns
#' deb_spread_lsd(y, lsd = lsd)
#'
#' # Replace original columns with replace = TRUE
#' deb_gather_lsd(x, replace = TRUE)
#' deb_spread_lsd(y, replace = TRUE)
#'
#' # Choose non-default column names
#' deb_gather_lsd(x, lsd_col = data, replace = TRUE)
#' deb_spread_lsd(y,
#'                l_col = libra,
#'                s_col = solidus,
#'                d_col = denarius,
#'                replace = TRUE)
#'
#' # The two functions are opposites
#' z <- x %>%
#'   deb_gather_lsd(replace = TRUE) %>%
#'   deb_spread_lsd(replace = TRUE)
#' all.equal(x, z)
#'
#' @name lsd-column
NULL


#' @rdname lsd-column
#' @export
deb_gather_lsd <- function(df,
                           l = l, s = s, d = d,
                           bases = c(20, 12),
                           lsd_col = lsd,
                           replace = FALSE) {

  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)
  cn <- rlang::as_name(rlang::enquo(lsd_col))

  df[[cn]] <- deb_lsd(l = rlang::eval_tidy(l, df),
                      s = rlang::eval_tidy(s, df),
                      d = rlang::eval_tidy(d, df),
                      bases = bases)
  if (replace == TRUE) {
    df <- dplyr::select(df, -(!! l), -(!! s), -(!! d))
  }
  df
}


#' @rdname lsd-column
#' @export
deb_spread_lsd <- function(df,
                           lsd = lsd,
                           l_col = l,
                           s_col = s,
                           d_col = d,
                           replace = FALSE) {

  lsd <- rlang::enquo(lsd)
  ln <- rlang::enquo(l_col)
  sn <- rlang::enquo(s_col)
  dn <- rlang::enquo(d_col)

  if (!deb_is_lsd(rlang::eval_tidy(lsd, df))) {
    rlang::abort("`lsd` must be of type <deb_lsd>.")
  }

  ret <- dplyr::mutate(df,
                       !! ln := field(!! lsd, "l"),
                       !! sn := field(!! lsd, "s"),
                       !! dn := field(!! lsd, "d"))
  if (replace == TRUE) {
    ret <- dplyr::select(ret, -(!! lsd))
  }
  ret

}
