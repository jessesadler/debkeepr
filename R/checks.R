## Checks ##

# Check that l, s, and d values are numeric in deb_normalize
lsd_check <- function(l, s, d, round = 3, vector = FALSE) {
  if (!is.numeric(round)) {
    stop(call. = FALSE, "round must be numeric")
  }

  if (!is.logical(vector)) {
    stop(call. = FALSE, "vector must be logical, either TRUE or FALSE")
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

# Check l, s, and d values and column names
lsd_column_check <- function(df, l, s, d) {

  column_names <- c(rlang::quo_name(l),
                    rlang::quo_name(s),
                    rlang::quo_name(d))

  # Ensure that l, s, and d columns exist in the data frame
  if (all(column_names %in% names(df)) == FALSE) {
    stop(call. = FALSE, "Column names for l, s, and d must be provided if the
         default names of l, s, and d are not present in the data frame")
  }

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

# Avoid overwriting l, s, and d columns in mutate functions,
# and check that column names and suffix are character vectors of length 1
lsd_column_names <- function(df, l, s, d, suffix) {

  # Turn l, s, and d column name arguments into character vector of length 3
  lsd_names <- c(rlang::quo_name(l), rlang::quo_name(s), rlang::quo_name(d))

  if (!is.character(suffix)) {
    stop(call. = FALSE, "suffix must be a character vector")
  }

  if (length(suffix) != 1) {
    stop(call. = FALSE, "suffix must be a character vector of length 1")
  }

  if (any(lsd_names %in% names(df)) == TRUE) {
    lsd_names[1] <- paste0(lsd_names[1], suffix)
    lsd_names[2] <- paste0(lsd_names[2], suffix)
    lsd_names[3] <- paste0(lsd_names[3], suffix)
  }
  lsd_names
}

# Check credit and debit columns
credit_check <- function(df, credit = NULL, debit = NULL, edge_columns, account_id = NULL) {

  if (all(edge_columns %in% names(df)) == FALSE) {
    stop(call. = FALSE, "Column names for credit and/or debit must be provided if
         the default names of credit and/or debit are not present in the data frame")
  }

  if (!is.null(credit) & !is.null(debit)) {
    credit <- rlang::eval_tidy(credit, df)
    debit <- rlang::eval_tidy(debit, df)
    if (class(credit) != class(debit)) {
      stop(call. = FALSE, "credit and debit variables must be of the same class")
      }
    }

  if (!is.null(account_id)) {
    id_present <- account_id %in% credit | account_id %in% debit
    if (id_present == FALSE) {
      stop(call. = FALSE, "account_id must be a value present in the credit and/or debit variables")
    }
  }
}

# Check interest parameters
interest_check <- function(interest, duration, with_principal) {
  if (!is.numeric(interest)) {
    stop(call. = FALSE, "interest must be numeric")
  }
  if (length(interest) != 1) {
    stop(call. = FALSE, "interest must be a numeric vector of length 1")
  }

  if (!is.numeric(duration)) {
    stop(call. = FALSE, "duration must be numeric")
  }
  if (length(duration) != 1) {
    stop(call. = FALSE, "duration must be a numeric vector of length 1")
  }

  if (!is.logical(with_principal)) {
    stop(call. = FALSE, "with_principal must be logical, either TRUE or FALSE")
  }
}

arithmetic_check <- function(x) {
  if (!is.numeric(x)) {
    stop(call. = FALSE, "x must be a numeric vector")
  }
  if (length(x) != 1) {
    stop(call. = FALSE, "x must be a numeric vector of length 1")
  }
}
