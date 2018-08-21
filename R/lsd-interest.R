## lsd interest calculations ##

#' Calculation of the interest of pounds, shillings, and pence
#'
#' Calculate the interest of pounds, shillings, and pence given an interest
#' rate and a duration. The function does not calculate compound interest.
#'
#' @inheritParams deb_normalize
#' @param interest Interest rate in decimal form. A numeric vector of length 1.
#'   Default is 0.0625, or 6.25 percent, which was default interest rate in the
#'   Low Countries at the end of the sixteenth century.
#' @param duration Duration over which the interest is calculated, or the
#'   number of times that the interest should be added. A numeric vector of
#'   length 1. Default is 1.
#' @param with_principal Logical (default `TRUE`): when `TRUE` the resulting
#'   pounds, shillings and pence value(s) will include the principal as well as
#'   the interest. When `FALSE` only the interest will be returned.
#'
#' @return Returns an object of class lsd with a bases attribute.
#'
#' @examples
#' # Calculate the interest with the principal over a certain duration
#' # If £10.14.5 were lent over a period of 5 years at 6.25%
#' deb_interest(lsd = c(10, 14, 5), duration = 5)
#'
#' # If £10.14.5 were lent over a period of 5 years at 8%
#' deb_interest(lsd = c(10, 14, 5), interest = 0.08, duration = 5)
#'
#' # Or you can calculate only the interest
#' deb_interest(lsd = c(10, 14, 5), duration = 5, with_principal = FALSE)
#'
#' # Interest for an lsd value with alternative bases
#' deb_interest(lsd = c(10, 14, 5), duration = 5, bases = c(20, 16))
#'
#' # Interest of an object of class lsd will use the bases attribute
#' lsd <- deb_as_lsd(lsd = c(10, 14, 5), bases = c(20, 16))
#' deb_interest(lsd = lsd, duration = 5)
#'
#' # Interest of a list of lsd values at a single rate
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_interest(lsd = lsd_list, interest = 0.08, duration = 5)
#'
#' # Or an lsd object with alternative bases
#' lsd_list2 <- deb_as_lsd(lsd = lsd_list, bases = c(20, 16))
#' deb_interest(lsd = lsd_list2, interest = 0.08, duration = 5)
#'
#' @export

deb_interest <- function(lsd,
                         interest = 0.0625,
                         duration = 1,
                         with_principal = TRUE,
                         bases = c(20, 12),
                         round = 5) {
  interest_check(interest, duration, with_principal)
  bases <- validate_bases(lsd, bases)

  if (is.list(lsd) == TRUE) {
    # lists
    if (with_principal == TRUE) {
      lsd <- purrr::map(lsd, ~ . + . * interest * duration)
    } else {
      lsd <- purrr::map(lsd, ~ . * interest * duration)
      }
    } else {
      # vectors
      if (with_principal == TRUE) {
        lsd <- lsd + lsd * interest * duration
      } else {
        lsd <- lsd * interest * duration
      }
  }
  deb_normalize(lsd = lsd, bases = bases, round = round)
}
