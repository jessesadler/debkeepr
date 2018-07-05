## Helpers ##

# list of lsd vectors to data frame and
# data frame of l, s, d values to list of lsd vectors

#' List of lsd vectors to data frame of pounds, shillings, and pence
#'
#' Transform a list of lsd vectors into a data frame that has pounds,
#' shillings, and pence variables.
#'
#' `lsd_list_to_df` is a helper function to make it easier to convert between
#' the two primary types of input objects of multiple lsd values used by
#' `debkeepr`. See [df_to_lsd_list()] to convert a data frame with pounds,
#' shillings, and pence variable to a list with lsd vectors.
#'
#' @family helper functions
#' @param lsd_list A list of numeric vectors of length 3. The first position
#'   of the vector represents the pounds value or l. The second position
#'   represents the shillings value or s. And the third position represents
#'   the pence value or d.
#'
#' @return Returns a tibble or `tbl_df` object with pounds, shillings, and
#'   pence variables named l, s, and d respectively. The number of rows in
#'   the tibble will be equal to the length of `lsd_list`.
#'
#' @examples
#' # Convert a list of lsd vectors to a tibble with
#' # pounds, shillings, and pence variables
#' example <- list(c(4, 34, 89), c(-9, -75, -19), c(15.85, 36.15, 56))
#' lsd_list_to_df(example)
#'
#' @importFrom stats setNames
#'
#' @export

lsd_list_to_df <- function(lsd_list) {
  # checks
  if (is.list(lsd_list) == FALSE | is.data.frame(lsd_list) == TRUE) {
    stop(call. = FALSE, "lsd_list must be a list of numeric vectors")
  }
  lsd_check(lsd_list)

  purrr::map(lsd_list, ~ stats::setNames(., c("l", "s", "d"))) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    tibble::as_tibble()
}

#' Data frame of pounds, shillings, and pence to list of lsd vectors
#'
#' Transform a data frame that has pounds, shillings, and pence variables into
#' a list of lsd vectors.
#'
#' `df_to_lsd_list` is a helper function to make it easier to convert between
#' the two primary types of input objects of multiple lsd values used by
#' `debkeepr`. See [lsd_list_to_df()] to convert a list of lsd vectors to a
#' data frame with pounds, shillings, and pence variables.
#'
#' @family helper functions
#' @inheritParams deb_normalize_df
#'
#' @return Returns a list of named numeric vectors representing the values of
#'   pounds, shillings, and pence. The length of the list will be equal to the
#'   number of rows in `df`. All variables in `df` aside from `l`, `s`, and `d`
#'   will be dropped.
#'
#' @examples
#' # Convert a data frame with pounds, shillings, and pence variables
#' # to a list of lsd vectors
#' example <- data.frame(l = c(35, -10, 26.725, 12),
#'                       s = c(50, -48, 311.85, 76),
#'                       d = c(89, -181, 70, 205))
#' df_to_lsd_list(example, l, s, d)
#'
#' # All variables aside from l, s, and d will be dropped
#' example2 <- data.frame(credit = c("a", "b", "a", "c"),
#'                        debit = c("b", "a", "c", "a"),
#'                        l = c(10, 10, 7, 9),
#'                        s = c(15, 15, 11, 2),
#'                        d = c(6, 6, 8, 11))
#' df_to_lsd_list(example, l, s, d)
#'
#' @export

df_to_lsd_list <- function(df, l = l, s = s, d = d) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  if (is.data.frame(df) == FALSE) {
    stop(call. = FALSE, "df must be a data frame")
  }

  lsd_column_check(df, l, s, d)

  df <- dplyr::select(df, !! l, !! s, !! d)

  as.list(df) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::map(~ stats::setNames(., c("l", "s", "d")))
}
