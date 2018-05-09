## Checks ##

#' Check l, s, and d values
#' @param l pounds
#' @param s shillings
#' @param d pence
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
