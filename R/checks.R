## Checks ##

# Check that lsd is numeric vector of length 3 or
# list of numeric vectors of length 3
lsd_check <- function(lsd) {
  if (rlang::is_bare_vector(lsd) == FALSE && deb_is_lsd(lsd) == FALSE) {
    stop(call. = FALSE, paste("lsd must be a list of class lsd, or an object that can be coerced to this class,",
                              "       namely a numeric vector of length 3 or a list of such vectors.",
                              sep = "\n"))
  }

  # check lsd vector
  if (rlang::is_list(lsd) == FALSE & rlang::is_vector(lsd) == TRUE) {
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
  if (rlang::is_list(lsd) == TRUE) {
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

null_check <- function(lsd) {
  if (any(purrr::map_lgl(lsd, rlang::is_null))) {
    nulls <- purrr::map_lgl(lsd, rlang::is_null)
    lsd[which(nulls == TRUE)] <- list(c(as.numeric(NA), as.numeric(NA), as.numeric(NA)))
  }
  lsd
}

separate_lsd_check <- function(lsd) {
  # numeric
  if (!all(purrr::map_lgl(lsd, is.numeric))) {
    stop(call. = FALSE, "l, s, and d must be numeric")
  }
  # length
  lengths <- purrr::map_int(lsd, length)
  if (length(unique(lengths)) > 1L) {
    stop(call. = FALSE, "l, s, and d must be vectors of equal length")
  }
}

# Check bases
bases_check <- function(bases) {
  # check bases
  if (!is.numeric(bases)) {
    stop(call. = FALSE, "bases must be a numeric vector")
  }
  if (length(bases) != 2) {
    stop(call. = FALSE, "bases must be a numeric vector of length of 2")
  }
  if (any(bases == 0)) {
    stop(call. = FALSE, "Neither of the values in bases can be 0")
  }
  if (any(bases < 0)) {
    stop(call. = FALSE, "The values in bases must both be positive")
  }
}

# Check round
round_check <- function(round) {
  if (!is.numeric(round)) {
    stop(call. = FALSE, "round must be numeric")
  }
  if (length(round) > 1) {
    stop(call. = FALSE, "round must be a numeric vector of length 1")
  }
}

lsd_list_column_check <- function(df, lsd) {
  if (!is.data.frame(df)) {
    stop(call. = FALSE, "df must be a data frame")
  }
  if (!rlang::quo_name(lsd) %in% names(df)) {
    stop(call. = FALSE, paste("Column name for lsd list column must be provided,",
                              "if the default name of lsd is not present in df.",
                              sep = " "))
  }
  if (!inherits(rlang::eval_tidy(lsd, df), "lsd")) {
    stop(call. = FALSE, "lsd must be an lsd list column")
  }
}

# Check l, s, and d values and column names
lsd_column_check <- function(df, l, s, d) {

  if (!is.data.frame(df)) {
    stop(call. = FALSE, "df must be a data frame")
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

suffix_check <- function(suffix, replace) {
  if (!is.logical(replace)) {
    stop(call. = FALSE, "replace must be either TRUE or FALSE")
  }
  if (length(replace) != 1) {
    stop(call. = FALSE, "replace must be a logical vector of length 1")
  }

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
         "Use replace = TRUE to replace the original variables where this is an option in the function",
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

arithmetic_check2 <- function(lsd1, lsd2) {
  if (is.list(lsd1) & is.list(lsd2) == TRUE) {
    if (length(lsd1) > 1 & length(lsd2) > 1) {
      if (identical(length(lsd1), length(lsd2)) == FALSE) {
        stop(call. = FALSE,
             "If lsd1 and lsd2 are both lists, they must be the same length, or one must be of length 1.")
      }
    }
  }
}

shillings_check <- function(x) {
  if (!is.numeric(x)) {
    stop(call. = FALSE, "shillings_rate must be numeric")
  }

  if (length(x) != 1) {
    stop(call. = FALSE, "shillings_rate must be a numeric vector of length 1")
  }
}

exchange_rate_check <- function(exchange_rate) {
  if (rlang::is_bare_vector(exchange_rate) == FALSE && deb_is_lsd(exchange_rate) == FALSE) {
    stop(call. = FALSE, paste("exchange_rate must be a list of class lsd, or an object that can be coerced to this",
                              "       class, namely a numeric vector of length 3 or a list of such vectors.",
                              sep = "\n"))
  }
  # check rate vector
  if (rlang::is_list(exchange_rate) == FALSE & rlang::is_vector(exchange_rate) == TRUE) {
    if (!is.numeric(exchange_rate)) {
      stop(call. = FALSE, "exchange_rate must be a numeric vector")
    }
    if (length(exchange_rate) != 3) {
      stop(call. = FALSE, paste("exchange_rate must be a vector of length of 3.",
                                "There must be a value for pounds, shillings, and pence.",
                                sep = "\n"))
    }
  }
  # check rate list
  if (rlang::is_list(exchange_rate) == TRUE) {
    if (!all(purrr::map_lgl(exchange_rate, is.numeric))) {
      stop(call. = FALSE, "exchange_rate must be a list of numeric vectors")
    }
    if (identical(purrr::map_dbl(exchange_rate, length), rep(3, length(exchange_rate))) == FALSE) {
      stop(call. = FALSE, paste("exchange_rate must be a list of numeric vectors of length 3.",
                                "There must be a value for pounds, shillings, and pence.",
                                sep = "\n"))
    }
  }
}

ratio_check <- function(ratio) {
  # check ratio
  if (!is.numeric(ratio)) {
    stop(call. = FALSE, "ratio must be a numeric vector")
  }
  if (length(ratio) != 1) {
    stop(call. = FALSE, "ratio must be a numeric vector of length 1")
  }
}
