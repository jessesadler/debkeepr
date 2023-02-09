# transaction_check works: Error messages

    Code
      deb_account("hello", "a")
    Condition
      Error in `deb_account()`:
      ! `df` must be a data frame.
      x You've supplied a <character>.

---

    Code
      deb_account(x, "a", credit = from, debit = to)
    Condition
      Error in `deb_account()`:
      ! `lsd` must be provided if the default is not present in `df`.

---

    Code
      deb_account(x, "a", lsd = data)
    Condition
      Error in `deb_account()`:
      ! Column names for `credit` and `debit` must be provided if the default names are not present in `df`.

---

    Code
      deb_account(x, "a", credit = from, debit = y, lsd = data)
    Condition
      Error in `deb_account()`:
      ! The `credit` and `debit` columns must be of the same type.
      * `credit` is type <character>.
      * `debit` is type <numeric>.

---

    Code
      deb_account(x, credit = from, debit = to, lsd = data)
    Condition
      Error in `deb_account()`:
      ! `account_id` is absent but must be supplied.

---

    Code
      deb_account(x, account_id = NULL, credit = from, debit = to, lsd = data)
    Condition
      Error in `deb_account()`:
      ! `account_id` cannot be `NULL`.
      x `account_id` must be present in `credit` and/or `debit` columns.

---

    Code
      deb_account(x, account_id = "x", credit = from, debit = to, lsd = data)
    Condition
      Error in `deb_account()`:
      ! `account_id` must be present in `credit` and/or `debit` columns.
      x "x" is not present in either the `credit` or `debit` columns.

---

    Code
      deb_account(x, "a", lsd = y, credit = from, debit = to)
    Condition
      Error in `deb_account()`:
      ! `lsd` must be of a column of type <deb_lsd>, <deb_tetra>, or <deb_decimal>.
      x You've supplied a column of type <numeric>.

