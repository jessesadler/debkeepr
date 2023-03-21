#' @keywords internal
#' @import vctrs
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

#' Unpacking assignment
#'
#' See \code{\link[zeallot]{\%<-\%}} for more details.
#'
#' @name %<-%
#' @rdname multi-assign
#' @keywords internal
#' @returns Invisibly returns `value`.
#' @export
#' @importFrom zeallot %<-%
#' @usage x \%<-\% value
NULL


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Tidy eval helpers
#'
#' @name tidyeval
#' @keywords internal
#' @importFrom rlang :=
#' @importFrom rlang .data
NULL

#' Methods
#' @name methods
#' @keywords internal
#' @importFrom methods setOldClass
NULL
