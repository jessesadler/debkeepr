## Sum of lsd columns ##

# Helper functions: They check whether sum is positive or negative
# If negative l, s, and d are all made negative

deb_l_sum <- function(l, s, d) {
  if (sum(l) + sum(s)/20 + sum(d)/240 > 0) {
    sum(l) + ((sum(s) + (sum(d) %/% 12)) %/% 20)
  } else {
    -(sum(-l) + ((sum(-s) + (sum(-d) %/% 12)) %/% 20))
  }
}

deb_s_sum <- function(l, s, d) {
  if (sum(l) + sum(s)/20 + sum(d)/240 > 0) {
    (sum(s) + (sum(d) %/% 12)) %% 20
  } else {
    -((sum(-s) + (sum(-d) %/% 12)) %% 20)
  }
}

deb_d_sum <- function(l, s, d) {
  if (sum(l) + sum(s)/20 + sum(d)/240 > 0) {
    round(sum(d) %% 12, 3)
  } else {
    -(round(sum(-d) %% 12, 3))
  }
}


#' Sum of pounds, shillings, and pence columns in a data frame
#'
#' Uses \code{summarise()} from \code{dplyr} to add pounds, shillings, and pence
#' columns in a data frame and refactor the values so that the result has
#' properly formatted values.
#'
#' When used on a data frame without any grouping, the result will be a data
#' frame with a single row consisting of columns for pounds, shillings, and
#' pence. When used in conjunction with \code{group_by()}, \code{deb_sum()}
#' will summarize the pounds, shillings, and pence columns for each group.
#'
#' @param df A data frame that contains columns with pounds, shillings,
#'   and pence variables.
#' @inheritParams lsd_column_check
#'
#' @return Returns a data frame with one level of grouping dropped.
#'   Any variables other than l, s, and d that are not grouped will
#'   be dropped. If the sum of any group is a negative value, the l,
#'   s, and d values for that group will all be returned as negative.
#'
#' @examples
#' # Use on an ungrouped data frame adds all values of pounds, shillings,
#' # and pence, resulting in a data frame with one row.
#' example1 <- tibble::tibble(group = c(1, 2, 1, 2),
#'                            l = c(3, 5, 6, 2),
#'                            s = c(10, 18, 11, 16),
#'                            d = c(9, 11, 10, 5))
#' deb_sum(example1, l, s, d)
#'
#' # Use with group_by() summarizes the values of pounds, shillings,
#' # and pence for each group.
#' example1 %>%
#'   group_by(group) %>%
#'   deb_sum(l, s, d)
#'
#' # The default for the function is to have pounds, shillings, and pence
#' # columns labeled as l, s, d. If this is true, you do not need to
#' # include these arguments.
#' example1 %>%
#'   group_by(group) %>%
#'   deb_sum()
#'
#' # The function can also take into account negative values
#' example2 <- tibble::tibble(group = c(1, 2, 1, 2),
#'                            l = c(-3, 5, -6, 2),
#'                            s = c(-10, 18, -11, 16),
#'                            d = c(-9, 11, -10, 5))
#' example2 %>%
#'   group_by(group) %>%
#'   deb_sum(l, s, d)
#'
#' @export

deb_sum <- function(df, l = l, s = s, d = d) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df = df,
                   l = l,
                   s = s,
                   d = d)

  dplyr::summarise(df,
                   l = deb_l_sum(!!l, !!s, !!d),
                   s = deb_s_sum(!!l, !!s, !!d),
                   d = deb_d_sum(!!l, !!s, !!d))
}
