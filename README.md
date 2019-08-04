
# debkeepr: Analysis of Non-Decimal Currencies

[![Travis build
status](https://travis-ci.org/jessesadler/debkeepr.svg?branch=master)](https://travis-ci.org/jessesadler/debkeepr)
[![Coverage
status](https://codecov.io/gh/jessesadler/debkeepr/branch/master/graph/badge.svg)](https://codecov.io/github/jessesadler/debkeepr?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`debkeepr` integrates non-decimal currencies that use the tripartite
system of pounds, shillings, and pence into the methodologies of Digital
Humanities and the practices of reproducible research. The package makes
it possible for historical non-decimal currencies to behave like
decimalized numeric values, while also providing support for values with
multiple units whose bases can differ. This is accomplished through the
implementation of the `deb_lsd` and `deb_decimal` classes, which are
based on the infrastructure provided by the [vctrs
package](https://vctrs.r-lib.org/). `debkkeepr` simplifies the process
of performing arithmetic calculations with non-decimal currencies — such
as adding £3 13s. 4d. sterling to £8 15s. 9d. sterling — and also
provides a basis for analyzing account books with thousands of
transactions recorded in non-decimal currencies. The name of the
`debkeepr` package derives from this latter capability of analyzing
historical account books that use [double-entry
bookkeeping](https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system).

## Installation

You can install `debkeepr` from GitHub with
[remotes](https://remotes.r-lib.org):

``` r
# install.packages("remotes")
remotes::install_github("jessesadler/debkeepr")
```

Please open an [issue](https://github.com/jessesadler/debkeepr/issues)
if you have any questions, comments, or requests.

## Historical Background

The `debkeepr` package uses the nomenclature of [l, s, and
d](https://en.wikipedia.org/wiki/%C2%A3sd) to represent pounds,
shillings, and pence units in non-decimal currencies. The abbreviations
derive from the Latin terms
[libra](https://en.wikipedia.org/wiki/French_livre),
[solidus](https://en.wikipedia.org/wiki/Solidus_\(coin\)), and
[denarius](https://en.wikipedia.org/wiki/Denarius). The libra was a
Roman measurement of weight, while the solidus and denarius were both
Roman coins. The denarius was a silver coin from the era of the
Republic, in contrast to the golden solidus that was issued in the Late
Empire. As the production of silver coins overtook that of gold by the
8th century, a solidus came to represent 12 silver denarii coins, and
240 denarii were — for a time — made from one libra or pound of silver.
The custom of counting coins in dozens (solidi) and scores of dozens
(librae) spread throughout the Carolingian Empire and became engrained
in much of Europe. However, a variety of currencies or monies of account
used [other bases](https://en.wikipedia.org/wiki/Non-decimal_currency)
for the solidus and denarius units. `debkeepr` provides a consistent
manner for dealing with any set of bases within a tripartite system
through the `bases` attribute of `deb_lsd` and `deb_decimal` vectors and
the `unit` attribute of `deb_decimal` vectors.

Translations of libra, solidus, and denarius units:

  - English: pounds, shillings, pence
  - French: livres, sols or sous, deniers
  - Italian: lire, soldi, denari
  - Flemish: ponden, schellingen, groten
  - Dutch: guilders, stuivers, penningen

## Resources

  - [Getting Started with debkeepr
    vignette](https://jessesadler.github.io/debkeepr/articles/debkeepr.html):
    An introduction to the `deb_lsd` and `deb_decimal` classes and their
    use as vectors and as columns in data frames.
  - [Transactions in Richard Dafforne’s Journal
    vignette](https://jessesadler.github.io/debkeepr/articles/transactions.html):
    Examples of financial and arithmetic calculations dealing with
    various currencies taken from the practice journal in Richard
    Dafforne’s *Merchant’s Mirrour* (1660).
  - [Analysis of Richard Dafforne’s Journal and Ledger
    vignette](https://jessesadler.github.io/debkeepr/articles/ledger.html):
    An analysis of the practice journal and ledger in Dafforne’s
    *Merchant’s Mirrour* using the `dafforne_transactions` and
    `dafforne_accounts` data provided in `debkeepr`.
  - [A PDF copy of Dafforne’s practice
    journal](https://github.com/jessesadler/debkeepr/blob/master/data-raw/dafforne-journal.pdf)
    can be consulted to further investigate the practices of early
    modern double-entry bookkeeping.

## Usage

The `deb_lsd` and `deb_decimal` classes are implemented to deal with two
interrelated problems inherent in historical currencies. Firstly,
historical currencies consist of three separate non-decimal units:
pounds, shillings, and pence. Secondly, the bases of the shillings and
pence units differed by region, coinage, and era. The `deb_lsd` class
maintains the tripartite structure of non-decimal currencies and
provides a `bases` attribute to record the bases for the shillings and
pence units. The print methods for both classes show the `bases`
attribute, and `deb_decimal` vectors include the `unit`.

Note that all of the functions in `debkeepr` begin with the prefix
`deb_`, which is short for double-entry bookkeeping.

``` r
library(debkeepr)

# Create deb_lsd vectors with standard bases of 20s. 12d.
lsd1 <- deb_lsd(l = 3, s = 13, d = 4)
lsd2 <- deb_lsd(l = 8, s = 15, d = 9)

# Perform arithmetic
lsd1 + lsd2
#> <deb_lsd[1]>
#> [1] 12:9s:1d
#> # Bases: 20s 12d
lsd2 - lsd1
#> <deb_lsd[1]>
#> [1] 5:2s:5d
#> # Bases: 20s 12d
lsd2 * 2 - lsd1
#> <deb_lsd[1]>
#> [1] 13:18s:2d
#> # Bases: 20s 12d

# Combine multiple values together
c(lsd1, lsd2)
#> <deb_lsd[2]>
#> [1] 3:13s:4d 8:15s:9d
#> # Bases: 20s 12d
```

Both classes allow the user to define the solidus and denarius units of
the values, enabling integration of currencies that do not use the
standardized bases of 20 shillings to the pound and 12 pence to the
shilling. An example of non-standard [money of
account](https://en.wikipedia.org/wiki/Unit_of_account) is the Polish
florin found in Dafforne’s practice journal in which a florin consisted
of 30 gros of 18 denars. All arithmetic calculations with `deb_lsd`
vectors — `sum()`, `round()`, `+`, `-`, etc. —
[normalize](https://en.wikipedia.org/wiki/Arithmetic#Compound_unit_arithmetic)
the values according to the chosen bases, making it much easier to do
the compound unit arithmetic that non-decimal currencies make necessary.

<img src="man/figures/compound-arithmetic.png" width="50%" />

``` r
# Create deb_lsd vector with standard bases of 20s. 12d.
(lsd3 <- deb_lsd(l = c(28, 32, 54, 18),
                 s = c(15, 8, 18, 12),
                 d = c(8, 11, 7, 9)))
#> <deb_lsd[4]>
#> [1] 28:15s:8d 32:8s:11d 54:18s:7d 18:12s:9d
#> # Bases: 20s 12d

# Same numerical values as Polish florins
(florins <- deb_lsd(l = c(28, 32, 54, 18),
                    s = c(15, 8, 18, 12),
                    d = c(8, 11, 7, 9),
                    bases = c(30, 18)))
#> <deb_lsd[4]>
#> [1] 28:15s:8d 32:8s:11d 54:18s:7d 18:12s:9d
#> # Bases: 30s 18d

# Different outcome with sum due to the bases
sum(lsd3)
#> <deb_lsd[1]>
#> [1] 134:15s:11d
#> # Bases: 20s 12d
sum(florins)
#> <deb_lsd[1]>
#> [1] 133:24s:17d
#> # Bases: 30s 18d

# Normalize a non-standard value to default bases
non_standard <- deb_lsd(132, 53, 35)
deb_normalize(non_standard)
#> <deb_lsd[1]>
#> [1] 134:15s:11d
#> # Bases: 20s 12d
```

The `deb_decimal` class represents non-decimal currencies in decimalized
form. The class tracks the solidus and denarius bases and the unit
represented by the decimalized values through the `bases` and `unit`
attributes. When working with decimalized data is preferable, the
`deb_decimal` class makes casting from and to `deb_lsd` possible without
losing any metadata about the `bases` used, and therefore the actual
value being represented.

``` r
# Create deb_decimal from numeric vector
(dec1 <- deb_decimal(c(5.525, 12.235, 8.45)))
#> <deb_decimal[3]>
#> [1]  5.525 12.235  8.450
#> # Unit: libra
#> # Bases: 20s 12d

# Same curreny values in solidus unit
(dec2 <- deb_decimal(c(110.5, 244.7, 169), unit = "s"))
#> <deb_decimal[3]>
#> [1] 110.5 244.7 169.0
#> # Unit: solidus
#> # Bases: 20s 12d

# Equality between different units
dec1 == dec2
#> [1] TRUE TRUE TRUE

# Combining deb_lsd and deb_decimal gives a deb_lsd vector
c(dec1, lsd1, lsd2)
#> <deb_lsd[5]>
#> [1] 5:10s:6d   12:4s:8.4d 8:9s:0d    3:13s:4d   8:15s:9d  
#> # Bases: 20s 12d

# Cast between deb_lsd and deb_decimal vectors
deb_as_lsd(dec1)
#> <deb_lsd[3]>
#> [1] 5:10s:6d   12:4s:8.4d 8:9s:0d   
#> # Bases: 20s 12d
deb_as_decimal(lsd3)
#> <deb_decimal[4]>
#> [1] 28.78333 32.44583 54.92917 18.63750
#> # Unit: libra
#> # Bases: 20s 12d
deb_as_decimal(florins)
#> <deb_decimal[4]>
#> [1] 28.51481 32.28704 54.61296 18.41667
#> # Unit: libra
#> # Bases: 30s 18d

# Represented by solidus/shillings unit
deb_as_decimal(lsd3, unit = "s")
#> <deb_decimal[4]>
#> [1]  575.6667  648.9167 1098.5833  372.7500
#> # Unit: solidus
#> # Bases: 20s 12d

# Represented by denarius/pence unit
deb_as_decimal(lsd3, unit = "d")
#> <deb_decimal[4]>
#> [1]  6908  7787 13183  4473
#> # Unit: denarius
#> # Bases: 20s 12d

# Either class can be cast to base numeric, which,
# of course, leads to the loss of all metadata
as.numeric(lsd3)
#> [1] 28.78333 32.44583 54.92917 18.63750
as.numeric(dec1)
#> [1]  5.525 12.235  8.450
```

## Differences between `deb_lsd` and `deb_decimal` vectors

See the [Getting Started with debkeepr
vignette](https://jessesadler.github.io/debkeepr/articles/debkeepr.html)
for an in depth discussion of the similarities and differences between
the two classes.

  - The `deb_lsd` class has the advantage of maintaining the structure
    and values used by non-decimal currencies, making it easier to
    identify and present such values.
  - `deb_decimal` implements a wider array of mathematical functions and
    arithmetic operations than `deb_lsd`.
  - Until [dplyr](https://dplyr.tidyverse.org) integrates record-style
    vectors based on the `vctrs` package more fully — which should
    happen with version 0.9.0 — `deb_lsd` vectors do not work with the
    `dplyr` functions `mutate()`, `summarise()`, and `arrange()`.
    However, the full complement of `dplyr` functions work with
    `deb_decimal` vectors.
  - [ggplot2](https://ggplot2.tidyverse.org) does not know how to pick a
    scale for `deb_lsd` vectors. In contrast, `deb_decimal` vectors work
    properly with `ggplot2`, though explicitly identifying the scale as
    continuous — with `scale_y_continuous()` or `scale_x_continuous()` —
    is needed to avoid the appearance of a message.
  - Casting methods between the two classes with `deb_as_lsd()` and
    `deb_as_decimal()` make it possible to move between the two classes
    without losing any data.
  - `deb_lsd` and `deb_decimal` vectors cannot be combined in a single
    function if they have different `bases`. The only way to transform
    the bases of `deb_lsd` and `deb_decimal` vectors is explicitly with
    `deb_convert_bases()`.
