#' Replace NULL elements in an lsd object
#'
#' Replace NULL elements in an lsd object with a numeric vector.
#'
#' Note that `NULL` elements in an lsd object print as NA due to the printing
#' method for lsd objects. `NULL` elements can be confirmed with `str()`. If
#' `c(NA, NA, NA)` is used for `replace` argument, the `NA` vector will be
#' coerced to numeric.
#'
#' @param lsd An object of class lsd or a list of numeric vectors of length 3
#'   that can be coerced to an lsd object.
#' @param replace A numeric vector of length 3. Default is `c(0, 0, 0)`, which
#'   replaces NULL with a value of 0.
#'
#' @return Returns an object of class lsd with `NULL` elements replaced.
#'
#' @examples
#' # Create lsd object with NULL elements
#' lsd <- deb_as_lsd(list(c(15, 4, 6),
#'                        NULL,
#'                        c(9, 12, 3),
#'                        NULL))
#' str(lsd)
#'
#' # Replace with default lsd value of 0
#' deb_replace_null(lsd = lsd)
#'
#' # Replace NULL with NA
#' str(deb_replace_null(lsd = lsd, replace = c(NA, NA, NA)))
#'
#' @export

deb_replace_null <- function(lsd, replace = c(0, 0, 0)) {
  replace <- list(replace)

  if (identical(replace, list(c(NA, NA, NA)))) {
    replace <- list(as.numeric(c(NA, NA, NA)))
  }

  # checks
  lsd_check(lsd)

  if (!purrr::map_lgl(replace, is.numeric)) {
    stop(call. = FALSE, "replace must be a numeric vector")
  }
  if (!identical(purrr::map_dbl(unname(replace), length), 3)) {
    stop(call. = FALSE, "replace must be a numeric vector of length 3")
  }

  if (any(purrr::map_lgl(lsd, rlang::is_null))) {
    nulls <- purrr::map_lgl(lsd, rlang::is_null)
    lsd[which(nulls == TRUE)] <- replace
  }
  lsd
}
