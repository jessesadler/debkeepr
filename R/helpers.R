## Helpers ##

# list of lsd vectors to data frame and
# data frame of l, s, d values to list of lsd vectors

#' List of lsd values to data frame of pounds, shillings, and pence
#'
#' Transform an object of class lsd or a list of lsd values into a data frame
#' that has pounds, shillings, and pence variables.
#'
#' `lsd_list_to_df` is a helper function to make it easier to convert between
#' the two primary types of input objects of multiple lsd values used by
#' `debkeepr`. See [deb_df_to_list()] to convert a data frame with pounds,
#' shillings, and pence variable to an lsd object.
#'
#' @family helper functions
#' @param lsd An lsd object or a list of numeric vectors of length 3.
#'
#' @return Returns a tibble or tbl_df object with pounds, shillings, and
#'   pence variables named l, s, and d respectively. The number of rows in
#'   the tibble will be equal to the length of `lsd`.
#'
#' @examples
#' # Convert a list of lsd values into a tibble with
#' # pounds, shillings, and pence variables
#' example <- list(c(4, 14, 9), c(-9, -5, -1), c(15, 15, 6))
#' deb_list_to_df(example)
#'
#' @export

deb_list_to_df <- function(lsd) {
  # checks
  if (is.list(lsd) == FALSE | is.data.frame(lsd) == TRUE) {
    stop(call. = FALSE, "lsd must be a list of numeric vectors")
  }
  lsd_check(lsd)

  purrr::map(lsd, ~ rlang::set_names(., c("l", "s", "d"))) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    tibble::as_tibble()
}

#' Data frame of pounds, shillings, and pence to an lsd object
#'
#' Transform a data frame that has pounds, shillings, and pence variables into
#' an lsd object.
#'
#' `df_to_lsd_list` is a helper function to make it easier to convert between
#' the two primary types of input objects of multiple lsd values used by
#' `debkeepr`. See [deb_list_to_df()] to convert a list of lsd values to a
#' data frame with pounds, shillings, and pence variables.
#'
#' @family helper functions
#' @inheritParams deb_as_lsd_mutate
#'
#' @return Returns an lsd object with a bases attribute. All variables in `df`
#'   aside from `l`, `s`, and `d` will be dropped.
#'
#' @examples
#' # Convert a data frame with pounds, shillings, and pence variables
#' # into a list an lsd object
#' example <- data.frame(l = c(4, -9, 25),
#'                       s = c(14, -5, 15),
#'                       d = c(9, -1, 6))
#' deb_df_to_list(example, l = l, s = s, d = d)
#'
#' # All variables aside from l, s, and d will be dropped
#' example2 <- data.frame(credit = c("a", "b", "a", "c"),
#'                        debit = c("b", "a", "c", "a"),
#'                        l = c(10, 10, 7, 9),
#'                        s = c(15, 15, 11, 2),
#'                        d = c(6, 6, 8, 11))
#' deb_df_to_list(example2, l = l, s = s, d = d)
#'
#' @export

deb_df_to_list <- function(df, l = l, s = s, d = d, bases = c(20, 12)) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  if (is.data.frame(df) == FALSE) {
    stop(call. = FALSE, "df must be a data frame")
  }
  bases_check(bases)
  lsd_column_check(df, l, s, d)

  lsd_df <- dplyr::select(df, !! l, !! s, !! d)

  if (ncol(df) != ncol(lsd_df))
    message("non-lsd variables were dropped")

  as.list(lsd_df) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::map(unname) %>%
    to_lsd(bases)
}
