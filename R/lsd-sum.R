## Sum of lsd objects ##

## Simplify deb_sum for single lsd object ##
deb_sum_simple <- function(lsd, round = 5, na.rm = FALSE) {
  # Error message for deb_summarise
  if (!deb_is_lsd(lsd)) {
    stop(call. = FALSE, "Variables to summarise must be list columns of class lsd.")
  }

  bases <- attributes(lsd)$bases
  lsd <- null_check(lsd)

  # Remove lsd vectors that have NA if na.rm = TRUE
  if (na.rm == TRUE) {
    lsd <- purrr::modify_if(lsd, ~ any(is.na(.)), as.null) %>%
      purrr::compact()
  }

  lsd <- purrr::reduce(lsd, `+`)

  deb_normalize(lsd, bases = bases, round = round)
}

#' Sum of pounds, shillings, and pence values
#'
#' Sum multiple pounds, shillings, and pence values, reducing the values to a
#' single pounds, shillings, and pence value in the form of an lsd object.
#'
#' See [deb_add()] to add an lsd value or list of lsd values to the
#' elements of a list of lsd values.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize
#' @param ... Objects of class lsd or objects that can be coerced to class lsd:
#'   numeric vectors of length 3 or lists of such vectors. All lsd objects must
#'   have the same bases.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   shillings or s and pence or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence. If any of the lsd values are lsd objects, the bases
#'   attribute will be used in the place of this argument. All lsd objects must
#'   have the same bases.
#' @param na.rm Logical. Passed on to `na.rm` argument in [sum()]. Whether
#'   missing values (NA) should be removed. Default is `FALSE`.
#'
#' @return Returns an lsd object with a bases attribute.
#'
#' @examples
#' # Sum of multiple lsd values
#' deb_sum(c(12, 7, 9), c(5, 8, 11), c(3, 18, 5))
#'
#' # Sum of multiple lsd values with alternative bases
#' deb_sum(c(12, 7, 9), c(5, 8, 11), c(3, 18, 5), bases = c(20, 16))
#'
#' # If one value is an lsd object, the bases attribute will be used
#' lsd <- deb_as_lsd(lsd = c(12, 7, 9), bases = c(20, 16))
#' deb_sum(lsd, c(5, 8, 11), c(3, 18, 5))
#'
#' # Sum of a lsd object of length > 1
#' lsd_list <- deb_as_lsd(lsd = list(c(12, 7, 9), c(5, 8, 11), c(3, 18, 5)))
#' deb_sum(lsd_list)
#'
#' # Sum of a mixture of lsd vectors and list of lsd vectors
#' deb_sum(lsd_list, c(8, 4, 9), c(6, 19, 10))
#'
#' # Cannot find sum of lsd objects that have different bases
#' \dontrun{
#' deb_sum(lsd, lsd_list)
#' }
#'
#' @export

deb_sum <- function(..., bases = c(20, 12), round = 5, na.rm = FALSE) {
  lsd_list <- list(...)
  # If input is a single lsd object use deb_sum_simple
  if (length(lsd_list) == 1 & deb_is_lsd(lsd_list[[1]])) {
    deb_sum_simple(lsd_list[[1]], round, na.rm)
  } else {
    purrr::map(lsd_list, lsd_check)
    bases <- validate_bases_p(lsd_list, bases)

    # Remove lsd vectors that have NA if na.rm = TRUE
    if (na.rm == TRUE) {
      lsd_list <- purrr::map(lsd_list, ~ purrr::modify_if(., ~ any(is.na(.)), as.null)) %>%
        purrr::map(purrr::compact)
    }

    lsd <- purrr::map_if(lsd_list, is.list, ~ purrr::reduce(., `+`)) %>%
      purrr::reduce(`+`)

    deb_normalize(lsd, bases = bases, round = round)
  }
}


#' Sum of pounds, shillings, and pence in a list column of a data frame
#'
#' Uses [dplyr::summarise_at()] to add pounds, shillings, and pence in one or
#' more lsd list columns in a data frame.
#'
#' When used on a data frame without any grouping, the result will be a data
#' frame with a single row consisting of the lsd list columns chosen to be
#' summarised. When used in conjunction with [dplyr::group_by()],
#' [deb_summarise()] will return a sum of the lsd list columns for each group.
#'
#' @inheritParams deb_lsd_gather
#' @param ... lsd list column or columns to be summed.
#' @inheritParams deb_sum
#'
#' @return Returns a data frame with one level of grouping dropped and lsd list
#'   columns.
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' # Tibble with an lsd list column
#' lsd_list <- deb_as_lsd(list(c(3, 10, 9),
#'                             c(5, 18, 11),
#'                             c(6, 11, 10),
#'                             c(2, 16, 5)),
#'                        bases = c(20, 12))
#' example <- tibble(group = c(1, 2, 1, 2),
#'                   lsd = lsd_list)
#'
#' # Sum of the lsd list column
#' deb_summarise(df = example, lsd)
#'
#' # Sum of groups of the lsd list column
#' example %>%
#'   group_by(group) %>%
#'   deb_summarise(lsd)
#'
#' @export

deb_summarise <- function(df, ..., round = 5, na.rm = FALSE) {
  lsd <- rlang::enquos(...)
  if (length(lsd) == 0) {
    stop(call. = FALSE, "Names for lsd list columns must be provided.")
  }

  dplyr::summarise_at(df, dplyr::vars(!!! lsd),
                      deb_sum_simple, round = round, na.rm = na.rm)
}
