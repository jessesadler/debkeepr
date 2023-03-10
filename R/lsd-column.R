## Create and separate a deb_lsd column ##

#' Helpers to create and separate a `deb_lsd` column in a data frame
#'
#' @description
#' - `deb_gather_lsd()` creates a `deb_lsd` column from separate variables
#'   representing pounds, shillings, and pence values.
#' - `deb_spread_lsd()` creates separate variables for pounds, shillings,
#'   and pence from a `deb_lsd` column.
#'
#' @details
#' When transcribing historical accounting data by hand, entering the pounds,
#' shillings, and pence values (lsd) into separate columns is probably the
#' easiest and least error prone method. The `deb_gather_()` and
#' `deb_spread_()` set of functions provide helpers to go back and forth
#' between this mode of data entry and the use of `deb_lsd` and `deb_tetra`
#' vectors within data frames in R. `deb_gather_lsd()` creates a `deb_lsd`
#' column from `l`, `s`, and `d` columns representing the three units of
#' this type of value. `deb_spread_lsd()` does the opposite. It takes a
#' `deb_lsd` column and spreads it into three separate pounds, shillings,
#' and pence columns.
#'
#' Values for column names (`lsd_col`, `l_col`, `s_col`, and `d_col`) must
#' be valid column names. They can be quoted or unquoted, but they cannot be
#' vectors or bare numbers. This follows the rules of [dplyr::rename()].
#'
#' @seealso [`deb_gather_tetra()`] and [`deb_spread_tetra()`] provide the
#' same functionality for the less common tetrapartite values of pounds,
#' shillings, pence, and farthings.
#'
#' @param df A data frame.
#' @param l Pounds column: Unquoted name of a numeric variable corresponding
#'   to the pounds or libra unit. Default is `l`.
#' @param s Shillings column: Unquoted name of a numeric variable
#'   corresponding to the shillings or solidus unit. Default is `s`.
#' @param d Pence column: Unquoted name of a numeric variable corresponding to
#'   the pence or denarius unit. Default is `d`.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#' @param lsd_col Unquoted name of the `deb_lsd` column created by the
#'   function. Default is `lsd`.
#' @param replace Logical (default `FALSE`). When `TRUE`, the newly created
#'   column(s) will replace the one(s) used to create it/them.
#' @param lsd `deb_lsd` column: Unquoted name of a `deb_lsd` column.
#'   Default is `lsd`.
#' @param l_col Unquoted name for the pounds column created by the function.
#'   Default is `l`.
#' @param s_col Unquoted name for the shillings column created by the function.
#'   Default is `s`.
#' @param d_col Unquoted name for the pence column created by the function.
#'   Default is `d`.
#'
#' @returns A data frame with a new `deb_lsd` column for `deb_gather_lsd()`
#'   or new pounds, shillings, and pence columns for `deb_spread_lsd()`.
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
  df <- dplyr::mutate(df, "{{lsd_col}}" := deb_lsd(l = {{ l }},
                                                   s = {{ s }},
                                                   d = {{ d }},
                                                   bases = bases))
  if (replace == TRUE) {
    df <- dplyr::select(df, -{{ l }}, -{{ s }}, -{{ d }})
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

  if (!deb_is_lsd(rlang::eval_tidy(rlang::enquo(lsd), df))) {
    cls <- class(rlang::eval_tidy(rlang::enquo(lsd), df))[[1]]
    cli::cli_abort(c("{.arg lsd} must be of type <deb_lsd>.",
                     "x" = "You've supplied a column of type {.cls {cls}}."))
  }

  df <- dplyr::mutate(df,
                      "{{l_col}}" := field({{ lsd }}, "l"),
                      "{{s_col}}" := field({{ lsd }}, "s"),
                      "{{d_col}}" := field({{ lsd }}, "d"))
  if (replace == TRUE) {
    df <- dplyr::select(df, -{{ lsd }})
  }
  df
}
