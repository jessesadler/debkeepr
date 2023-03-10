## Create and separate a deb_tetra column ##

#' Helpers to create and separate a `deb_tetra` column in a data frame
#'
#' @description
#' - `deb_gather_tetra()` creates a `deb_tetra` column from separate
#'   variables representing pounds, shillings, pence, and farthing values.
#' - `deb_spread_tetra()` creates separate variables for pounds, shillings,
#'   pence, and farthings from a `deb_tetra` column.
#'
#' @details
#' When transcribing historical accounting data by hand, entering the pounds,
#' shillings, pence, and optionally farthing values (lsd(f)) into separate
#' columns is probably the easiest and least error prone method. The
#' `deb_gather_()` and `deb_spread_()` set of functions provide helpers to
#' go back and forth between this mode of data entry and the use of `deb_lsd`
#' and `deb_tetra` vectors within data frames in R. `deb_gather_tetra()`
#' creates a `deb_tetra` column from four separate columns representing the
#' four units in this type of value. `deb_spread_tetra()` does the opposite.
#' It takes a `deb_tetra` column and spreads it into four separate columns
#' representing the four units.
#'
#' Values for column names (`tetra_col`, `l_col`, `s_col`, `d_col`, and
#' `f_col`) must be valid column names. They can be quoted or unquoted, but
#' they cannot be vectors or bare numbers. This follows the rules of
#' [dplyr::rename()].
#'
#' @seealso [`deb_gather_lsd()`] and [`deb_spread_lsd()`] provide the same
#' functionality for the more common tripartite values of pounds, shillings,
#' and pence.
#'
#' @param df A data frame.
#' @param l Pounds column: Unquoted name of a numeric variable corresponding
#'   to the pounds or libra unit. Default is `l`.
#' @param s Shillings column: Unquoted name of numeric variable corresponding
#'   to the shillings or solidus unit. Default is `s`.
#' @param d Pence column: Unquoted name of numeric variable corresponding to
#'   the pence or denarius unit. Default is `d`.
#' @param f Farthing column: Unquoted name of numeric variable corresponding
#'   to the farthing or f unit. Default is `f`.
#' @param bases Numeric vector of length 3 used to specify the bases for the
#'   solidus or s, denarius or d, and farthing or f units. Default is
#'   `c(20, 12, 4)`, which conforms to the English system of 1 pound =
#'   20 shillings, 1 shilling = 12 pence, and 1 pence = 4 farthing.
#' @param tetra_col Unquoted name of the `deb_tetra` column created by the
#'   function. Default is `tetra`.
#' @param replace Logical (default `FALSE`). When `TRUE`, the newly created
#'   column(s) will replace the one(s) used to create it/them.
#' @param tetra `deb_tetra` column: Unquoted name of a `deb_tetra` column.
#'   Default is `tetra`.
#' @param l_col An unquoted name for the pounds column created by the
#'   function. Default is `l`.
#' @param s_col An unquoted name for the shillings column created by the
#'   function. Default is `s`.
#' @param d_col An unquoted name for the pence column created by the
#'   function. Default is `d`.
#' @param f_col An unquoted name for the farthings column created by the
#'   function. Default is `f`.
#'
#' @returns A data frame with a new `deb_tetra` column for `deb_gather_tetra()`
#'   or new pounds, shillings, pence, and farthing columns for
#'   `deb_spread_tetra()`.
#'
#' @examples
#'
#' libra <- c(3, 5, 6, 2)
#' solidus <- c(10, 18, 11, 16)
#' denarius <- c(9, 11, 10, 5)
#' farthing <- c(2, 3, 1, 0)
#'
#' # data frame with separate l, s, and d variables and default names
#' x <- data.frame(accounts = c(1, 2, 3, 4),
#'                 l = libra,
#'                 s = solidus,
#'                 d = denarius,
#'                 f = farthing)
#'
#' # data frame with deb_tetra variable and default names
#' y <- data.frame(accounts = c(1, 2, 3, 4),
#'                 tetra = deb_tetra(l = libra,
#'                                   s = solidus,
#'                                   d = denarius,
#'                                   f = farthing))
#'
#' # Gather l, s, d, and f variables into a deb_tetra column
#' deb_gather_tetra(x, l = l, s = s, d = d, f = f)
#'
#' # Spread deb_tetra column into separate l, s, d, and f columns
#' deb_spread_tetra(y, tetra = tetra)
#'
#' # Replace original columns with replace = TRUE
#' deb_gather_tetra(x, replace = TRUE)
#' deb_spread_tetra(y, replace = TRUE)
#'
#' # Choose non-default column names
#' deb_gather_tetra(x, tetra_col = data, replace = TRUE)
#' deb_spread_tetra(y,
#'                  l_col = libra,
#'                  s_col = solidus,
#'                  d_col = denarius,
#'                  f_col = farthing,
#'                  replace = TRUE)
#'
#' # The two functions are opposites
#' z <- x %>%
#'   deb_gather_tetra(replace = TRUE) %>%
#'   deb_spread_tetra(replace = TRUE)
#' all.equal(x, z)
#'
#' @name tetra-column
NULL


#' @rdname tetra-column
#' @export
deb_gather_tetra <- function(df,
                             l = l, s = s, d = d, f = f,
                             bases = c(20, 12, 4),
                             tetra_col = tetra,
                             replace = FALSE) {
  df <- dplyr::mutate(df, "{{tetra_col}}" := deb_tetra(l = {{ l }},
                                                       s = {{ s }},
                                                       d = {{ d }},
                                                       f = {{ f }},
                                                       bases = bases))
  if (replace == TRUE) {
    df <- dplyr::select(df, -{{ l }}, -{{ s }}, -{{ d }}, -{{ f }})
  }
  df
}

#' @rdname tetra-column
#' @export

deb_spread_tetra <- function(df,
                             tetra = tetra,
                             l_col = l,
                             s_col = s,
                             d_col = d,
                             f_col = f,
                             replace = FALSE) {

  if (!deb_is_tetra(rlang::eval_tidy(rlang::enquo(tetra), df))) {
    cls <- class(rlang::eval_tidy(rlang::enquo(tetra), df))[[1]]
    cli::cli_abort(c("{.arg tetra} must be of type <deb_tetra>.",
                     "x" = "You've supplied a column of type {.cls {cls}}."))
  }

  df <- dplyr::mutate(df,
                      "{{l_col}}" := field({{ tetra }}, "l"),
                      "{{s_col}}" := field({{ tetra }}, "s"),
                      "{{d_col}}" := field({{ tetra }}, "d"),
                      "{{f_col}}" := field({{ tetra }}, "f"))
  if (replace == TRUE) {
    df <- dplyr::select(df, -{{ tetra }})
  }
  df
}
