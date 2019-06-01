
# debkeepr: Analysis of Non-Decimal Currencies

[![Travis build
status](https://travis-ci.org/jessesadler/debkeepr.svg?branch=master)](https://travis-ci.org/jessesadler/debkeepr)
[![Coverage
status](https://codecov.io/gh/jessesadler/debkeepr/branch/master/graph/badge.svg)](https://codecov.io/github/jessesadler/debkeepr?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`debkeepr` provides an interface for working with non-decimal currencies
that use the tripartite system of pounds, shillings, and pence. The
package makes it possible for historical non-decimal currencies to
behave like decimalized numeric values in many circumstances, while also
providing support for values with multiple units whose bases can differ.
This is accomplished through the creation of the `deb_lsd` and
`deb_decimal` classes, which are based on the infrastructure provided by
the [vctrs package](https://vctrs.r-lib.org/).

## Installation

You can install `debkeepr` from GitHub with
[remotes](https://remotes.r-lib.org):

``` r
# install.packages("remotes")
remotes::install_github("jessesadler/debkeepr")
```

## Lifecycle

`debkeepr` is under active development and is at an experimental stage
in its lifecycle. `debkeepr` is dependent upon `vctrs`, which is also at
an experimental stage. The retired [lsd list-class
branch](https://github.com/jessesadler/debkeepr/tree/list-class) uses
the previous API, which currently provides more capabilities. In
particular, `vctrs` has yet to be integrated into `tibble` and `dplyr`,
so the use of `deb_lsd` and `deb_decimal` vectors in data frames is
limited. These capabilities will be added when possible. Please open an
[issue](https://github.com/jessesadler/debkeepr/issues) if you have any
questions, comments, or requests.

## Usage

`debkeepr` allows non-decimal currency values to be used as normal
numeric values as much as possible.

``` r
library(debkeepr)

# Create deb_lsd vectors with standard bases of 20s. 12d.
x <- deb_lsd(15, 13, 4)
y <- deb_lsd(8, 15, 9)

# Perform arithmetic as usual
x + y
#> <deb_lsd[1]>
#> [1] 24:9s:1d
#> # Bases: 20s 12d
x - y
#> <deb_lsd[1]>
#> [1] 6:17s:7d
#> # Bases: 20s 12d
y * 2 - x
#> <deb_lsd[1]>
#> [1] 1:18s:2d
#> # Bases: 20s 12d

# Combine multiple values together
c(x, y)
#> <deb_lsd[2]>
#> [1] 15:13s:4d 8:15s:9d 
#> # Bases: 20s 12d
```

## Classes: deb\_lsd and deb\_decimal

`debkeepr` introduces two classes that both deal with two interrelated
problems associated with historical currencies. Firstly, historical
currencies consisted of three separate non-decimal units: pounds,
shillings, and pence. Secondly, the bases of the units
[differed](https://en.wikipedia.org/wiki/Non-decimal_currency) by
region, coinage, and era. The `deb_lsd` class maintains the tripartite
structure of non-decimal currencies and provides a `bases` attribute to
record the bases for the shillings and pence units. The `deb_decimal`
class represents the values in decimalized form, while also keeping
track of the shillings and pence bases and the unit represented by the
decimalized values through `bases` and `unit` attributes. When working
with decimalized data is preferable, the `deb_decimal` class make
casting from and to `deb_lsd` possible without losing any metadata about
the bases used.

``` r
# Create deb_decimal from numeric values
(z <- deb_decimal(c(5.525, 8.45, 12.235)))
#> <deb_decimal[3]>
#> [1]  5.525  8.450 12.235
#> # Unit: libra
#> # Bases: 20s 12d

# Combining deb_lsd and deb_decimal gives a deb_lsd vector
c(x, y, z)
#> <deb_lsd[5]>
#> [1] 15:13s:4d  8:15s:9d   5:10s:6d   8:9s:0d    12:4s:8.4d
#> # Bases: 20s 12d

# Transform deb_lsd vector to deb_decimal
deb_as_decimal(c(x, y))
#> <deb_decimal[2]>
#> [1] 15.66667  8.78750
#> # Unit: libra
#> # Bases: 20s 12d

# Represented by solidus/shillings unit
deb_as_decimal(c(x, y), unit = "s")
#> <deb_decimal[2]>
#> [1] 313.3333 175.7500
#> # Unit: solidus
#> # Bases: 20s 12d

# Represented by denarius/pence unit
deb_as_decimal(c(x, y), unit = "d")
#> <deb_decimal[2]>
#> [1] 3760 2109
#> # Unit: denarius
#> # Bases: 20s 12d

# Either class can also be transformed to base numeric
as.numeric(c(x, y))
#> [1] 15.66667  8.78750
```

## Historical Background

The `debkeepr` package uses the nomenclature of [l, s, and
d](https://en.wikipedia.org/wiki/%C2%A3sd) to represent pounds,
shillings, and pence units. The abbreviations derive from the Latin
terms [libra](https://en.wikipedia.org/wiki/French_livre),
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
in much of Europe. However, a variety of [other
bases](https://en.wikipedia.org/wiki/Non-decimal_currency) for the
solidi and denarii units were in use in different regions and at
different times. `debkeepr` provides a consistent manner for dealing
with any set of bases within a tripartite system through the `bases`
attribute of `deb_lsd` and `deb_decimal` vectors and the `unit`
attribute of `deb_decimal` vectors.

## Overview

  - All of the functions in `debkeepr` begin with the prefix `deb_`,
    which is short for double-entry bookkeeping.
  - The nomenclature used throughout the package follows the [original
    Latin terms](https://en.wikipedia.org/wiki/%C2%A3sd) in using “l”,
    “s”, and “d” to represent librae, solidi, and denarii
    respectively. These terms were translated into the various European
    languages.
      - English: pounds, shillings, pence
      - French: livres, sols or sous, deniers
      - Italian: lire, soldi, denari
      - Dutch: ponden, schellingen, groten or penningen
  - [The Transactions in Richard Dafforne’s Journal
    vignette](https://jessesadler.github.io/debkeepr/articles/transactions.html)
    provides examples of performing arithmetic operations used by
    historical accountants.

## Examples

At the heart of `debkeepr` is the need to normalize pounds, shillings,
and pence values to specified non-decimal unit bases in the process of
making various calculations. Even in the simplest arithmetic operations
can be tricky with non-decimal currencies. `debkeepr` simplifies this
process, while also making it safer to work with values that use
different bases. The `bases` of `deb_lsd` and `deb_decimal` vectors can
only be modified explicitly.

For example, adding together a set of values by hand might result in the
non-standard form of £131 62s. 41d. in a currency with the standard
bases of 20 shillings per £1 and 12 pence per shilling.

``` r
# Normalize £131 62s. 41d.
x <- deb_lsd(131, 67, 42)
deb_normalize(x)
#> <deb_lsd[1]>
#> [1] 134:10s:6d
#> # Bases: 20s 12d

# Or create a deb_lsd vector and add
(y <- deb_lsd(l = c(15, 32, 18, 54, 12),
             s = c(9, 17, 8, 15, 18),
             d = c(11, 8, 9, 4, 10)))
#> <deb_lsd[5]>
#> [1] 15:9s:11d  32:17s:8d  18:8s:9d   54:15s:4d  12:18s:10d
#> # Bases: 20s 12d

# Do the addition
sum(y)
#> <deb_lsd[1]>
#> [1] 134:10s:6d
#> # Bases: 20s 12d

# The process can be redone with non-standard bases
# Compare this to deb_normalize(x)
deb_lsd(131, 67, 42, bases = c(60, 16)) %>% 
  deb_normalize()
#> <deb_lsd[1]>
#> [1] 132:9s:10d
#> # Bases: 60s 16d
```

`debkeepr` ensures that `deb_lsd` and `deb_decimal` vectors with
different `bases` and `deb_decimal` vectors with a different `unit`
cannot be combined: `c(x, deb_lsd(131, 67, 42, bases = c(60, 16)))`
throws an error. For many more examples, including how to do exchanges
between currencies with different bases, see the [Transactions in
Richard Dafforne’s Journal
vignette](https://jessesadler.github.io/debkeepr/articles/transactions.html).
