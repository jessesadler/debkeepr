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
    if (!is.na(current)){
      if (should_be_int(current)) {
        current <- round(current)
        }
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

  # If statements to ensure NAs from above not turned into 0s.
  # If no NAs, then any NAs from join should be 0s
  if (!anyNA(c(pos$credit, neg$debit))) {
    ret <- dplyr::full_join(pos, neg, by = "account_id") %>%
      dplyr::mutate(credit = dplyr::coalesce(credit, 0), # replace NA with 0
                    debit = dplyr::coalesce(debit, 0),
                    current = credit - debit,
                    current = dplyr::if_else(should_be_int(current),
                                             round(current),
                                             current)) %>%
      dplyr::arrange(.data$account_id)
  # If all accounts present in pos and neg, no NAs will be introduced
  } else if (all_present(pos, neg)) {
    ret <- dplyr::left_join(pos, neg, by = "account_id") %>%
      dplyr::mutate(current = credit - debit,
                    current = dplyr::if_else(should_be_int(current),
                                             round(current),
                                             current)) %>%
      dplyr::arrange(.data$account_id)
  # If there are NAs and NAs will be introduced by join, need to distingush
  # between the two types. Add 0s from missing accounts then join.
  } else {
    pos_acc <- pos[[1]]
    neg_acc <- neg[[1]]

    pos_missing <- neg_acc[!(neg_acc %in% pos_acc)]
    neg_missing <- pos_acc[!(pos_acc %in% neg_acc)]

    pos <- tibble::tibble(account_id = c(pos_acc, pos_missing),
                          credit = c(pos$credit,
                                     rep(0, vctrs::vec_size(pos_missing))))
    neg <- tibble::tibble(account_id = c(neg_acc, neg_missing),
                          debit = c(neg$debit,
                                     rep(0, vctrs::vec_size(neg_missing))))

    ret <- dplyr::left_join(pos, neg, by = "account_id") %>%
      dplyr::mutate(current = credit - debit,
                    current = dplyr::if_else(should_be_int(current),
                                             round(current),
                                             current)) %>%
      dplyr::arrange(.data$account_id)
  }

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

  pos <- dplyr::group_by(df, {{ credit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ credit }})
  neg <- dplyr::distinct(df, {{ debit }})

  if (!anyNA(c(pos[[2]]))) {
    ret <- dplyr::full_join(pos, neg, by = c("account_id" = names(neg))) %>%
      dplyr::mutate(!! cn := dplyr::coalesce({{ lsd }}, 0)) %>%
      dplyr::arrange(.data$account_id)
  } else if (all_present(pos, neg)) {
    ret <- dplyr::left_join(pos, neg, by = c("account_id" = names(neg))) %>%
      dplyr::arrange(.data$account_id)
  } else {
    pos_acc <- pos[[1]]
    neg_acc <- neg[[1]]

    pos_missing <- neg_acc[!(neg_acc %in% pos_acc)]

    ret <- tibble::tibble(account_id = c(pos_acc, pos_missing),
                          !! cn := c(pos[[2]],
                                     rep(0, vctrs::vec_size(pos_missing)))) %>%
      dplyr::arrange(.data$account_id)
  }

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

  neg <- dplyr::group_by(df, {{ debit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ debit }})
  pos <- dplyr::distinct(df, {{ credit }})

  if (!anyNA(c(neg[[2]]))) {
    ret <- dplyr::full_join(neg, pos, c("account_id" = names(pos))) %>%
      dplyr::mutate(!! cn := dplyr::coalesce({{ lsd }}, 0)) %>%
      dplyr::arrange(.data$account_id)
  } else if (all_present(pos, neg)) {
    ret <- dplyr::left_join(neg, pos, by = c("account_id" = names(pos))) %>%
      dplyr::arrange(.data$account_id)
  } else {
    neg_acc <- neg[[1]]
    pos_acc <- pos[[1]]

    neg_missing <- pos_acc[!(pos_acc %in% neg_acc)]

    ret <- tibble::tibble(account_id = c(neg_acc, neg_missing),
                          !! cn := c(neg[[2]],
                                     rep(0, vctrs::vec_size(neg_missing)))) %>%
      dplyr::arrange(.data$account_id)
  }

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
    # Closed accounts = 0; NA accounts still open
    dplyr::filter({{ lsd }} != 0 | is.na({{ lsd }}))
}

deb_balance <- function(df,
                        credit = credit, debit = debit,
                        lsd = lsd,
                        na.rm = FALSE) {

  current <- deb_current(df,
                         credit = {{ credit }},
                         debit = {{ debit }},
                         lsd = {{ lsd }},
                         na.rm = na.rm)

  # Putting this here ensures checks run first
  lsd_quo <- rlang::enquo(lsd)
  cn <- rlang::as_name(lsd_quo)
  # Access data for bases and class
  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  bases <- deb_bases(lsd_vctr)

  vals <- current[[2]]

  # If there are NAs
  if(anyNA(vals)) {
    return(tibble::tibble(relation = c("credit", "debit"),
             !! cn := vctrs::vec_init(deb_lsd(bases = bases), 2L)))
  }

  # If completely balanced
  if (all(vals == 0)) {
    if (deb_is_lsd(lsd_vctr)) {
      return(tibble::tibble(relation = c("credit", "debit"),
               !! cn := deb_lsd(c(0, 0), c(0, 0), c(0, 0), bases = bases)))
    } else {
      return(tibble::tibble(relation = c("credit", "debit"),
               !! cn := deb_decimal(c(0, 0), bases = bases)))
    }
  }

  pos <- sum(vals[vals > 0])
  neg <- sum(vals[vals < 0])

  tibble::tibble(relation = c("credit", "debit"),
                 !! cn := c(pos, neg))
}
