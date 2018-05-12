## Checks ##

#' Check that l, s, and d values are numeric
#' @param l Pounds: numeric value
#' @param s Shillings: numeric value
#' @param d Pence: numeric value
lsd_check <- function(l, s, d) {
  if (is.null(l) | is.null(s) | is.environment(d)) {
    stop(call. = FALSE, "Values for l, s, and d must be provided. Maybe you need a 0.")
  }

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
#' @param l Pounds column: Unquoted name of a numeric variable corresponding to pounds.
#' @param s Shillings column: Unquoted name of numeric variable corresponding to shillings.
#' @param d Pence column: Unquoted name of numeric variable corresponding to pence.
lsd_column_check <- function(df, l, s, d) {
  l <- rlang::eval_tidy(l, df)
  s <- rlang::eval_tidy(s, df)
  d <- rlang::eval_tidy(d, df)

  if (!is.numeric(l)) {
    stop(call. = FALSE, "l must be a numeric variable")
  }

  if (!is.numeric(s)) {
    stop(call. = FALSE, "s must be a numeric variable")
  }

  if (!is.numeric(d)) {
    stop(call. = FALSE, "d must be a numeric variable")
  }

}
