## Checks ##

# Check that lsd is numeric vector of length 3 or
# list of numeric vectors of length 3
lsd_check <- function(lsd) {
  if (is.vector(lsd) == FALSE) {
    stop(call. = FALSE, "lsd must be either a numeric vector or list of numeric vectors")
  }

  # check lsd vector
  if (is.list(lsd) == FALSE & is.vector(lsd) == TRUE) {
    if (!is.numeric(lsd)) {
      stop(call. = FALSE, "lsd must be a numeric vector")
    }
    if (length(lsd) != 3) {
      stop(call. = FALSE, paste("lsd must be a vector of length of 3.",
                                "There must be a value for pounds, shillings, and pence.",
                                sep = "\n"))
    }
  }

  # check lsd list
  if (is.list(lsd) == TRUE) {
    if (!all(purrr::map_lgl(lsd, is.numeric))) {
      stop(call. = FALSE, "lsd must be a list of numeric vectors")
    }
    if (identical(purrr::map_dbl(lsd, length), rep(3, length(lsd))) == FALSE) {
      stop(call. = FALSE, paste("lsd must be a list of numeric vectors of length 3.",
                                "There must be a value for pounds, shillings, and pence.",
                                sep = "\n"))
    }
  }
}

# Check lsd_bases
bases_check <- function(lsd_bases) {
  # check lsd_bases
  if (!is.numeric(lsd_bases)) {
    stop(call. = FALSE, "lsd_bases must be a numeric vector")
  }
  if (length(lsd_bases) != 2) {
    stop(call. = FALSE, "lsd_bases must be a numeric vector of length of 2")
  }
  if (any(lsd_bases == 0)) {
    stop(call. = FALSE, "Neither of the values in lsd_bases can be 0")
  }
  if (any(lsd_bases < 0)) {
    stop(call. = FALSE, "The values in lsd_bases must both be positive")
  }
}

# Check l, s, and d values and column names
lsd_column_check <- function(df, l, s, d) {

  if (!is.data.frame(df)) {
    stop(call. = FALSE, "df must be a data frame or data-frame like object")
  }

  column_names <- c(rlang::quo_name(l),
                    rlang::quo_name(s),
                    rlang::quo_name(d))

  # Ensure that l, s, and d columns exist in the data frame
  if (all(column_names %in% names(df)) == FALSE) {
    stop(call. = FALSE, paste("Column names for l, s, and d must be provided if the",
                              "default names of l, s, and d are not present in the data frame",
                              sep = "\n"))
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

  if (any(lsd_names %in% names(df)) == TRUE) {
    lsd_names[1] <- paste0(lsd_names[1], suffix)
    lsd_names[2] <- paste0(lsd_names[2], suffix)
    lsd_names[3] <- paste0(lsd_names[3], suffix)
  }
  lsd_names
}

suffix_check <- function(suffix, replace = FALSE) {
  if (!is.character(suffix)) {
    stop(call. = FALSE, "suffix must be a character vector")
  }

  if (length(suffix) != 1) {
    stop(call. = FALSE, "suffix must be a character vector of length 1")
  }

  # Make sure suffix has a value so that variables are not overwritten during
  # the function, resulting in wrong results if l or s have decimals.
  if (suffix == "") {
    stop(call. = FALSE, paste("suffix cannot be an empty character vector.",
         "To keep the same variable names and replace the original variables use replace = TRUE",
         sep = "\n"))
  }
  # Use same variable names if replace is TRUE
  if (replace == TRUE) {
    suffix <- ""
  }
  suffix
}

# Check credit and debit columns
credit_check <- function(df, credit = NULL, debit = NULL, edge_columns, account_id = NULL) {

  if (all(edge_columns %in% names(df)) == FALSE) {
    stop(call. = FALSE, paste("Column names for credit and/or debit must be provided if",
                              "the default names of credit and/or debit are not present in the data frame",
                              sep = "\n"))
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

exchange_rate_check <- function(x) {
  if (!is.numeric(x)) {
    stop(call. = FALSE, "rate_per_shillings must be numeric")
  }

  if (length(x) != 1) {
    stop(call. = FALSE, "rate_per_shillings must be a numeric vector of length 1")
  }
}
