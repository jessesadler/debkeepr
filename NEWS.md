# debkeepr 0.0.5

- Changes for vctrs 0.3.0.
- Adopt dplyr 1.0.0 as minimum version. This enables dplyr methods with record-style vectors such as `deb_lsd`.
- Rewrite coercion and casting methods for 0.3.0.
- Change documentation style of exporting methods.

# debkeepr 0.0.4

- Rewrite package using `vctrs` to create `deb_lsd` and `deb_decimal` classes.
- Normalization of `deb_lsd` vectors.
- Coercion and casting methods with `deb_lsd` and `deb_decimal`.
- Mathematical functions and arithmetic operators with `deb_lsd` and `deb_decimal`.
- Functions to convert bases of `deb_lsd` and `deb_decimal` vectors and unit of `deb_decimal` vectors.
- Rewrite README to reflect changes.
- Rewrite transactions vignette. Getting started and ledger vignettes temporarily removed, because they need more substantial revision.

## Breaking changes

- Basically everything.
- Remove old functions using list-lsd class.
- Remove accounts and list-column functions. This functionality is not replicated due to `vctrs` not integrating with `dplyr` yet.

# debkeepr 0.0.3

- Add vignette: Analysis of Richard Dafforne’s Journal and Ledger
- pkgdown website for `debkeepr`

# debkeepr 0.0.2

- `deb_account_summary()` changed to have separate credit, debit, and current lsd list columns.
- Improve handling of `NA` and values of £0 in account functions.
- `deb_credit()` and `deb_debit()` show values of £0 if no credit or debit in an account.
- `deb_summarise()` returns value of £0 if column only has `NA` values.
- Add vignette: Transactions in Richard Dafforne's Journal
- Added a `NEWS.md` file to track changes to the package.

# debkeepr 0.0.1

- Implementation of `lsd` class and `lsd` list columns.
- Add vignette: Getting started with debkeepr
