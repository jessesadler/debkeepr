## Checks ##

# Check that l, s, and d values are numeric in deb_refactor
lsd_check <- function(l, s, d, round = 3, vector = FALSE) {
  if (!is.numeric(round)) {
    stop(call. = FALSE, "round must be numeric")
  }

  if (!is.logical(vector)) {
    stop(call. = FALSE, "vector must be logical, either TRUE or FALSE")
  }

  if (is.null(l) | is.null(s) | is.null(d)) {
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

  if (length(l) != length(s) | length(l) != length(d)) {
    stop(call. = FALSE, "l, s, and d must be numeric vectors of the same length")
  }
}

# Check l, s, and d values for deb_sum
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
