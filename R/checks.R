## Checks ##

#' Check l, s, and d values
#' @param l Pounds: numeric value
#' @param s Shillings: numeric value
#' @param d Pence: numeric value
lsd_check <- function(l, s, d) {

  if (!is.numeric(l)) {
    stop(call. = FALSE, "l must be numeric")
  }

  if (!is.numeric(s)) {
    stop(call. = FALSE, "s must be numeric")
  }

  if (!is.numeric(d)) {
    stop(call. = FALSE, "d must be numeric")
  }
}

#' Check l, s, and d values
#' @param l Pounds column: Unquoted name of numeric variable.
#' @param s Shillings column: Unquoted name of numeric variable.
#' @param d Pence column: Unquoted name of numeric variable.
lsd_column_check <- function(df, l, s, d) {
  l <- rlang::eval_tidy(l, df)
  s <- rlang::eval_tidy(s, df)
  d <- rlang::eval_tidy(d, df)

  if (!is.numeric(l)) {
    stop(call. = FALSE, "l must be numeric")
  }

  if (!is.numeric(s)) {
    stop(call. = FALSE, "s must be numeric")
  }

  if (!is.numeric(d)) {
    stop(call. = FALSE, "d must be numeric")
  }

}
