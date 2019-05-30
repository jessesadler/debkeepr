
<!-- README.md is generated from README.Rmd. Please edit that file -->

# debkeepr: Analysis of Non-Decimal Currencies

[![Travis build
status](https://travis-ci.org/jessesadler/debkeepr.svg?branch=master)](https://travis-ci.org/jessesadler/debkeepr)
[![Coverage
status](https://codecov.io/gh/jessesadler/debkeepr/branch/master/graph/badge.svg)](https://codecov.io/github/jessesadler/debkeepr?branch=master)

`debkeepr` provides an interface for working with non-decimal currencies
that use the tripartite system of pounds, shillings, and pence. The
package includes functions to apply arithmetic and financial operations
to single or multiple values and to analyze account books that use
either [single-entry
bookkeeping](https://en.wikipedia.org/wiki/Single-entry_bookkeeping_system)
or [double-entry
bookkeeping](https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system)
with the latter providing the name for `debkeepr`.

## Installation

You can install `debkeepr` from GitHub with
[devtools](https://github.com/hadley/devtools):

``` r
# install.packages("devtools")
devtools::install_github("jessesadler/debkeepr")
```

## lsd Class

The pounds, shillings, and pence monetary system complicates even
relatively simple arithmetic manipulations, as each unit has to be
[normalized](https://en.wikipedia.org/wiki/Arithmetic#Compound_unit_arithmetic)
or converted to the correct base. `debkeepr` uses the [vctrs
package](https://vctrs.r-lib.org) to integrate non-decimal currencies
into standard R practices. `debkeepr` introduces two classes: `deb_lsd`
and `deb_decimal`. The `deb_lsd` maintains the tripartite structure of
most non-decimal currencies and provides a `bases` attribute to define
the bases of the solidus or shillings unit and denarius or pence unit.
The `deb_decimal` class represents the same values as `deb_lsd` in
decimal form. It too provides a `bases` attribute, as well as a `unit`
attribute so that values can be represented in either libra (pounds),
solidus, or denarius units.

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
attribute of an `lsd` object.

## Overview

  - All of the functions in `debkeepr` begin with the prefix `deb_`,
    which is short for double-entry bookkeeping.
  - The nomenclature used throughout the package follows the [original
    Latin terms](https://en.wikipedia.org/wiki/%C2%A3sd) in using l, s,
    and d to represent librae, solidi, and denarii respectively and to
    refer to such values as lsd values. These terms were translated into
    the various European languages.
      - English: pounds, shillings, pence
      - French: livres, sols or sous, deniers
      - Italian: lire, soldi, denari
      - Dutch: ponden, schellingen, groten or penningen

## Examples

At the heart of `debkeepr` is the need to normalize pounds, shillings,
and pence values to specified non-decimal unit bases in the process or
making various calculations. Even in the simplest arithmetic operations
can be tricky with non-decimal currencies. `debkeepr` extracts away much
of the difficulties and in many cases makes it possible to procede as if
you are working with normal decimalized data. When working with
decimalized data is preferable, the `deb_decimal` class make casting
from and to `deb_lsd` easy.

For example, adding together a set of values by hand might result in the
non-standard form of £131 62s. 41d. in a currency with the standard
bases of 20 shillings per £1 and 12 pence per shilling.

``` r
library(debkeepr)

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
```

Other arithmetic operations are also made available by `debkeepr`. If
you think of `y` as profits to be divided evenly among three parterns,
the calculations can be made quickly.

``` r

# Profits for each partner
sum(y) / 3
#> <deb_lsd[1]>
#> [1] 44:16s:10d
#> # Bases: 20s 12d
```

`y` could also be transformed into its decimalized form in any of the
three units.

``` r
# Decimalized libra
deb_as_decimal(y)
#> <deb_decimal[5]>
#> [1] 15.49583 32.88333 18.43750 54.76667 12.94167
#> # Unit: libra
#> # Bases: 20s 12d

# Decimalized solidus
deb_as_decimal(y, unit = "s")
#> <deb_decimal[5]>
#> [1]  309.9167  657.6667  368.7500 1095.3333  258.8333
#> # Unit: solidus
#> # Bases: 20s 12d

# Decimalized denarius
deb_as_decimal(y, unit = "d")
#> <deb_decimal[5]>
#> [1]  3719  7892  4425 13144  3106
#> # Unit: denarius
#> # Bases: 20s 12d
```
