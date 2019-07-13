## Transaction functions ##


deb_account <- function(df, account_id,
                        credit = credit, debit = debit,
                        lsd = lsd,
                        na.rm = FALSE) {

  # First two for transaction_check
  # Distinguish from non-quoted; not necessary but why not
  credit_quo <- rlang::enquo(credit)
  debit_quo <- rlang::enquo(debit)
  lsd_quo <- rlang::enquo(lsd)
  # need cn for base subsetting to access lsd column
  cn <- rlang::as_name(lsd_quo)

  transaction_check(df, cn = cn,
                    credit = credit_quo, debit = debit_quo,
                    edge_columns = c(rlang::as_name(credit_quo),
                                     rlang::as_name(debit_quo)),
                    account_id = account_id)

  # Access data for bases and class
  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  bases <- deb_bases(lsd_vctr)

  deb_ptype_check(lsd_vctr)

  # deb_lsd
  if (deb_is_lsd(lsd_vctr)) {
    pos <- dplyr::filter(df, {{ credit }} == account_id)
    if (vec_size(pos) < 1) {
      cred <- deb_lsd(0, 0, 0, bases = bases)
    } else {
      cred <- sum(pos[[cn]], na.rm = na.rm)
    }

    neg <- dplyr::filter(df, {{ debit }} == account_id)
    if (vec_size(neg) < 1) {
      deb <- deb_lsd(0, 0, 0, bases = bases)
    } else {
      deb <- sum(neg[[cn]], na.rm = na.rm)
    }

    current <- cred - deb
    # deb_decimal
  } else {
    pos <- dplyr::filter(df, {{ credit }} == account_id)
    if (vec_size(pos) < 1) {
      cred <- deb_decimal(0, bases = bases)
    } else {
      cred <- sum(pos[[cn]], na.rm = na.rm)
    }

    neg <- dplyr::filter(df, {{ debit }}== account_id)
    if (vec_size(neg) < 1) {
      deb <- deb_decimal(0, bases = bases)
    } else {
      deb <- sum(neg[[cn]], na.rm = na.rm)
    }

    current <- cred - deb
    # Deal with floating point issues
    if (should_be_int(current) == TRUE) {
      current <- round(current)
    }
  }

  tibble::tibble(relation = c("credit", "debit", "current"),
                 !! cn := c(cred, deb, current))
}


deb_account_summary <- function(df,
                                credit = credit, debit = debit,
                                lsd = lsd,
                                na.rm = FALSE) {

  credit_quo <- rlang::enquo(credit)
  debit_quo <- rlang::enquo(debit)
  lsd_quo <- rlang::enquo(lsd)
  cn <- rlang::as_name(lsd_quo)

  transaction_check(df, cn = cn,
                    credit = credit_quo, debit = debit_quo,
                    edge_columns = c(rlang::as_name(credit_quo),
                                     rlang::as_name(debit_quo)))

  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  deb_ptype_check(lsd_vctr)

  # Turn deb_lsd to deb_decimal until dplyr works with rcrd
  if (deb_is_lsd(lsd_vctr)) {
    df <- dplyr::mutate(df, !! cn := deb_as_decimal({{ lsd }}))
  }

  pos <- dplyr::group_by(df, {{ credit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ credit }}, credit = {{ lsd }})
  neg <- dplyr::group_by(df, {{ debit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ debit }}, debit = {{ lsd }})

  ret <- dplyr::full_join(pos, neg, by = "account_id") %>%
    dplyr::mutate(credit = dplyr::coalesce(credit, 0), # replace NA with 0
                  debit = dplyr::coalesce(debit, 0),
                  current = credit - debit,
                  current = dplyr::if_else(should_be_int(current),
                                           round(current),
                                           current)) %>%
    dplyr::arrange(.data$account_id)

  # Return deb_decimal back to deb_lsd
  if (deb_is_lsd(lsd_vctr)) {
    ret[["credit"]] <- deb_as_lsd(ret[["credit"]])
    ret[["debit"]] <- deb_as_lsd(ret[["debit"]])
    ret[["current"]] <- deb_as_lsd(ret[["current"]])
  }

  ret
}

deb_credit <- function(df,
                       credit = credit, debit = debit,
                       lsd = lsd,
                       na.rm = FALSE) {

  credit_quo <- rlang::enquo(credit)
  debit_quo <- rlang::enquo(debit)
  lsd_quo <- rlang::enquo(lsd)
  cn <- rlang::as_name(lsd_quo)

  transaction_check(df, cn = cn,
                    credit = credit_quo, debit = debit_quo,
                    edge_columns = c(rlang::as_name(credit_quo),
                                     rlang::as_name(debit_quo)))

  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  deb_ptype_check(lsd_vctr)

  # Turn deb_lsd to deb_decimal until dplyr works with rcrd
  if (deb_is_lsd(lsd_vctr)) {
    df <- dplyr::mutate(df, !! cn := deb_as_decimal({{ lsd }}))
  }

  ret <- dplyr::group_by(df, {{ credit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ credit }}) %>%
    # Add any accounts that do not have a credit
    dplyr::full_join(dplyr::distinct(df, {{ debit }}),
                     by = c("account_id" = rlang::as_name(rlang::enquo(debit)))) %>%
    dplyr::mutate(!! cn := dplyr::coalesce({{ lsd }}, 0)) %>%
    dplyr::arrange(.data$account_id)

  if (deb_is_lsd(lsd_vctr)) {
    ret[[cn]] <- deb_as_lsd(ret[[cn]])
  }

  ret
}

deb_debit <- function(df,
                       credit = credit, debit = debit,
                       lsd = lsd,
                       na.rm = FALSE) {

  credit_quo <- rlang::enquo(credit)
  debit_quo <- rlang::enquo(debit)
  lsd_quo <- rlang::enquo(lsd)
  cn <- rlang::as_name(lsd_quo)

  transaction_check(df, cn = cn,
                    credit = credit_quo, debit = debit_quo,
                    edge_columns = c(rlang::as_name(credit_quo),
                                     rlang::as_name(debit_quo)))

  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  deb_ptype_check(lsd_vctr)

  # Turn deb_lsd to deb_decimal until dplyr works with rcrd
  if (deb_is_lsd(lsd_vctr)) {
    df <- dplyr::mutate(df, !! cn := deb_as_decimal({{ lsd }}))
  }

  ret <- dplyr::group_by(df, {{ debit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ debit }}) %>%
    # Add any accounts that do not have a debit
    dplyr::full_join(dplyr::distinct(df, {{ credit }}),
                     by = c("account_id" = rlang::as_name(rlang::enquo(credit)))) %>%
    dplyr::mutate(!! cn := dplyr::coalesce({{ lsd }}, 0)) %>%
    dplyr::arrange(.data$account_id)

  if (deb_is_lsd(lsd_vctr)) {
    ret[[cn]] <- deb_as_lsd(ret[[cn]])
  }

  ret
}

deb_current <- function(df,
                        credit = credit, debit = debit,
                        lsd = lsd,
                        na.rm = FALSE) {

  deb_account_summary(df,
                      credit = {{ credit }},
                      debit = {{ debit }},
                      lsd = {{ lsd }},
                      na.rm = na.rm) %>%
    dplyr::select(.data$account_id, {{ lsd }} := current)
}

deb_open <- function(df,
                     credit = credit, debit = debit,
                     lsd = lsd,
                     na.rm = FALSE) {

  deb_account_summary(df,
                      credit = {{ credit }},
                      debit = {{ debit }},
                      lsd = {{ lsd }},
                      na.rm = na.rm) %>%
    dplyr::select(.data$account_id, {{ lsd }} := current) %>%
    dplyr::filter({{ lsd }} != 0)
}

deb_balance <- function(df,
                        credit = credit, debit = debit,
                        lsd = lsd,
                        na.rm = FALSE) {

  lsd_quo <- rlang::enquo(lsd)
  cn <- rlang::as_name(lsd_quo)
  # Access data for bases and class
  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  bases <- deb_bases(lsd_vctr)

  current <- deb_current(df,
                         credit = {{ credit }},
                         debit = {{ debit }},
                         lsd = {{ lsd }},
                         na.rm = na.rm)
  # If completely balanced
  if (all(current[[cn]] == 0)) {
    if (deb_is_lsd(lsd_vctr)) {
      return(tibble::tibble(relation = c("credit", "debit"),
                            !! cn := deb_lsd(c(0, 0),
                                             c(0, 0),
                                             c(0, 0),
                                             bases = bases)))
    } else {
      return(tibble::tibble(relation = c("credit", "debit"),
                            !! cn := deb_decimal(c(0, 0), bases = bases)))
    }
  }

  vals <- current[[cn]]
  pos <- sum(vals[vals > 0], na.rm = na.rm)
  neg <- sum(vals[vals < 0], na.rm = na.rm)

  tibble::tibble(relation = c("credit", "debit"),
                 !! cn := c(pos, neg))
}
