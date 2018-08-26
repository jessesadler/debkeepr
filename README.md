
<!-- README.md is generated from README.Rmd. Please edit that file -->
debkeepr: Analysis of Non-Decimal Currencies and Double-Entry Bookkeeping
=========================================================================

[![Travis build status](https://travis-ci.org/jessesadler/debkeepr.svg?branch=master)](https://travis-ci.org/jessesadler/debkeepr) [![Coverage status](https://codecov.io/gh/jessesadler/debkeepr/branch/master/graph/badge.svg)](https://codecov.io/github/jessesadler/debkeepr?branch=master)

`debkeepr` provides an interface for analyzing non-decimal currencies that use the tripartite system of pounds, shillings, and pence. It includes functions to apply arithmetic and financial operations to single or multiple values and to analyze account books that use either [single-entry bookkeeping](https://en.wikipedia.org/wiki/Single-entry_bookkeeping_system) or [double-entry bookkeeping](https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system) with the latter providing the name for `debkeepr`. The use of non-decimal currencies throughout the medieval and early modern period presents difficulties for the analysis of historical accounts. The pounds, shillings, and pence system complicates even relatively simple arithmetic manipulations, as each unit has to be [normalized](https://en.wikipedia.org/wiki/Arithmetic#Compound_unit_arithmetic) or converted to the correct base. To unite pounds, shillings, and pence units into a single value and associate the shillings and pence units with non-decimal bases `debkeepr` implements a special class of R object. The **lsd** class consists of a list of one or more numeric vectors of length 3 and a `bases` attribute attached to the list. `debkeepr` provides functions to manipulate `lsd` objects on their own or within a data frame as list column.

The `debkeepr` package uses the nomenclature of [l, s, and d](https://en.wikipedia.org/wiki/%C2%A3sd) to represent pounds, shillings, and pence units. The abbreviations derive from the Latin terms [libra](https://en.wikipedia.org/wiki/French_livre), [solidus](https://en.wikipedia.org/wiki/Solidus_(coin)), and [denarius](https://en.wikipedia.org/wiki/Denarius). The libra was a Roman measurement of weight, while the solidus and denarius were both Roman coins. The denarius was a silver coin from the era of the Republic, in contrast to the golden solidus that was issued in the Late Empire. As the production of silver coins overtook that of gold by the 8th century, a solidus came to represent 12 silver denarii coins, and 240 denarii were — for a time — made from one libra or pound of silver. The custom of counting coins in dozens (solidi) and scores of dozens (librae) spread throughout the Carolingian Empire and became engrained in much of Europe. However, a variety of [other bases](https://en.wikipedia.org/wiki/Non-decimal_currency) for the solidi and denarii units were in use in different regions and at different times.

Installation
------------

You can install `debkeepr` from GitHub with [devtools](https://github.com/hadley/devtools):

``` r
# install.packages("devtools")
devtools::install_github("jessesadler/debkeepr")
```

Overview
--------

-   All of the functions in `debkeepr` begin with the prefix `deb_`, which is short for double-entry bookkeeping.
-   The nomenclature used throughout the package follows the [original Latin terms](https://en.wikipedia.org/wiki/%C2%A3sd) in using l, s, and d to represent librae, solidi, and denarii respectively and to refer to such values as lsd values. These terms were translated into the various European languages.
    -   English: pounds, shillings, pence
    -   French: livres, sols or sous, deniers
    -   Italian: lire, soldi, denari
    -   Dutch: ponden, schellingen, groten or penningen
-   The functions are designed to be used in three types of contexts:
    1.  Manipulation of single or multiple lsd values represented as an object of class `lsd` or an object that can be coerced to class `lsd`: a numeric vector of length 3 or a list of such vectors.
    2.  A data frame with one or more list columns of class `lsd`.
    3.  A data frame that mimics the structure of an account book and can be thought of as a transactions data frame. In addition to an `lsd` list column that denotes the value of each transaction, a transactions data frame contains variables recording the [credit and debit account](https://en.wikipedia.org/wiki/Debits_and_credits) for each transaction.
-   Data
    -   `debkeepr` contains two data sets from the example journal and ledger in Richard Dafforne's *Merchant's Mirrour* from 1660. `dafforne_transactions` is a transactions data frame with 177 transactions. `dafforne_accounts` possesses information about the 46 accounts in the journal and ledger.

lsd objects
-----------

The `lsd` class is implemented to deal with two interrelated problems in manipulating pounds, shillings, and pence (lsd) values in a programming environment such as R. In the first place, a single lsd value possesses three separate units. Making matters more difficult, the shillings and pence units are non-decimal, and the bases for the the units differed by region, coinage used as the [money of account](https://en.wikipedia.org/wiki/Unit_of_account), and era. The latter issue means that the actual value represented by three numbers standing in for pounds, shillings, and pence cannot be known outside of the context of the bases for the shillings and pence units.

The `lsd` class provides solutions to these issues by unifying pounds, shillings, and pence values into a numeric vector of length three and the creation of a `bases` attribute to track and implement the specified bases for the shillings and pence units. The first position of each vector represents the pounds unit, the second the shillings unit, and the third the pence unit. The `bases` attribute consists of a numeric vector of length 2. The first position records the base for the shillings unit, and the second position provides the base for the pence unit. The `bases` attribute ensures that pounds, shillings, and pence values are always associated with their correct bases and that values with different bases are never unknowingly brought together. `lsd` objects are stored as [lists](http://r4ds.had.co.nz/vectors.html#lists), making it possible to have multiple lsd values in a single `lsd` object and to implement `lsd` objects as list columns in a data frame or [tibble](https://tibble.tidyverse.org). The `lsd` class uses its own printing method to reduce the space taken up by each lsd value compared to the default list print method. The print method borrows from that of data frame, since the structures of the objects are similar.

### Creating lsd objects

Objects of class `lsd` can be created either explicitly or implicitly. `debkeepr` provides two methods for the explicit creation of `lsd` objects.

1.  `deb_lsd()` uses separate pounds (`l`), shillings (`s`), and pence (`d`) vectors, alongside a bases vector, to create an `lsd` object. `l`, `s`, and `d` must all be the same length, and the length of the `lsd` object will be the same as the input vectors.
2.  `deb_as_lsd()` converts a numeric vector of length three or a list of such vectors into an `lsd` object.

In addition, all `debkeepr` functions that produce a pounds, shillings, and pence value return an object of class `lsd`. Whether the input to the `lsd` argument of a `debkeepr` function is an `lsd` object, a numeric vector of length 3, or a list of such numeric vectors, the returned value will be of class `lsd`.

``` r
library(debkeepr)

# Explicitly create an lsd object from separate l, s, and d vectors

# single lsd value
deb_lsd(l = 10, s = 8, d = 11)
#>      l s  d
#> [1] 10 8 11

# multiple lsd values
deb_lsd(l = c(10, 12, 5), s = c(8, 16, 3), d = c(11, 2, 8))
#>      l  s  d
#> [1] 10  8 11
#> [2] 12 16  2
#> [3]  5  3  8

# Explicitly create an lsd object from lsd vectors

# single lsd value
deb_as_lsd(lsd = c(10, 8, 11))
#>      l s  d
#> [1] 10 8 11

# multiple lsd values
deb_as_lsd(lsd = list(c(10, 8, 11), c(12, 16, 2), c(5, 3, 8)))
#>      l  s  d
#> [1] 10  8 11
#> [2] 12 16  2
#> [3]  5  3  8
```

An `lsd` list column in a data frame can be constructed at the time of making the data frame or from separate pounds, shillings, and pence variables. The former method necessitates the use of the [tibble package](https://tibble.tidyverse.org), as a list column cannot be added to a data frame with the base `data.frame()` function. Creating a list column of class `lsd` with `tibble()` can be done with either `deb_lsd()` or `deb_as_lsd()`. Another method, which can be used in both tibbles and base data frames, is to convert separate pounds, shillings, and pence variables to an `lsd` list column with `deb_lsd_gather()`. The function possesses an optional `replace` argument to remove the original pounds, shillings, and pence variables. The inverse operation of converting an `lsd` list column to separate pounds, shillings, and pence variables can be done with `deb_lsd_spread()`.

``` r
library(tibble)

# lsd list column from scratch with tibble
tibble(lsd = deb_lsd(l = c(10, 12, 5), s = c(8, 16, 3), d = c(11, 2, 8)))
#> # A tibble: 3 x 1
#>   lsd      
#>   <S3: lsd>
#> 1 10, 8, 11
#> 2 12, 16, 2
#> 3 5, 3, 8

# lsd list column from separate l, s, and d variables
lsd_df <- data.frame(l = c(10, 12, 5), s = c(8, 16, 3), d = c(11, 2, 8))
deb_lsd_gather(df = lsd_df, replace = TRUE)
#>         lsd
#> 1 10, 8, 11
#> 2 12, 16, 2
#> 3   5, 3, 8
```

With few exceptions all of the functions in `debkeepr` possess a `bases` argument that provides the values to be stored as the `bases` attribute of an `lsd` object. The values for the `bases` argument must be positive and cannot be missing or zero. The default is `c(20, 12)`, which conforms to the most widely used system of 1 pound = 20 shillings and 1 shilling = 12 pence. Unless an alternative set of bases is provided, the default value will be applied. However, if the input to a `debkeepr` function is an object of class `lsd`, the `bases` attribute will be used and override any values passed to the `bases` argument. This means that the use of `debkeepr` functions on an `lsd` list column do not apply the `bases` argument. The only way to alter the `bases` attribute of an `lsd` object is to do so explicitly with `deb_convert_bases()`. An easy way to access the `bases` attribute of an `lsd` object is with `deb_bases()`.

``` r
# lsd object with default bases
deb_lsd(l = c(10, 12, 5), s = c(8, 16, 3), d = c(11, 2, 8))
#>      l  s  d
#> [1] 10  8 11
#> [2] 12 16  2
#> [3]  5  3  8

# lsd object with alternative bases of Holland guilders
deb_lsd(l = c(10, 12, 5), s = c(8, 16, 3), d = c(11, 2, 8), bases = c(20, 16))
#>      l  s  d
#> [1] 10  8 11
#> [2] 12 16  2
#> [3]  5  3  8

# confirm the bases
deb_lsd(l = c(10, 12, 5), s = c(8, 16, 3), d = c(11, 2, 8), bases = c(20, 16)) %>% 
  deb_bases()
#>  s  d 
#> 20 16
```

Context 1: lsd objects
----------------------

The creation and use of `lsd` objects by themselves are most likely to be useful for doing quick calculations on values in historical sources since they cannot be directly attached to information about dates, accounts involved, or other attribute data as is possible in a data frame with an `lsd` list column. However, the dispersed nature of accounting practices and mediums in which historians will encounter pounds, shillings, and pence values makes these quick calculations quite useful.

The pounds, shillings, and pence values used as inputs to `debkeepr` functions can be of three types:

1.  An object of class `lsd`
2.  A numeric vector of length 3 for a single lsd value
3.  A list of numeric vectors of length 3 for multiple lsd values

The examples below will show the various types of possible inputs.

``` r
# Create lsd objects with default bases to be used in examples
single_lsd <- deb_as_lsd(c(5, 13, 8))
mult_lsd <- deb_lsd(l = c(10, 12, 5), s = c(8, 16, 3), d = c(11, 2, 8))
```

### Normalization

A particularly useful function for one-off uses is `deb_normalize()`, which normalizes an lsd value to the bases provided in the `bases` argument or in the `bases` attribute. For instance, adding together a set of values by hand might result in the non-standard form of £10 64s. 71d.

``` r
# Normalize a non-standard lsd value
deb_normalize(lsd = c(10, 64, 71))
#>      l s  d
#> [1] 13 9 11
```

The bases for the shillings and pence units can be changed with the bases argument.

``` r
# Normalize a non-standard lsd value with alternative bases
deb_normalize(lsd = c(10, 64, 71), bases = c(20, 16))
#>      l s d
#> [1] 13 8 7
```

Multiple lsd values can be normalized by placing numeric vectors into a list. The below example demonstrates some of the types of values that can be properly normalized by `deb_normalize()`. The function accepts negative values or even a mix of positive and negative values within a numeric vector. Any of the units can also possess decimalized values.

``` r
# To normalize multiple lsd values use a list of lsd vectors
deb_normalize(lsd = list(c(4, 34, 89), c(-9, -75, -19), c(15.85, 36.15, 56)))
#>       l   s    d
#> [1]   6   1  5.0
#> [2] -12 -16 -7.0
#> [3]  17  17  9.8
```

Another option available in all of the functions that deal with lsd values is to round the pence unit to a specified decimal place. The `round` argument helps to simplify the output and avoids the issue of rounding lsd values to a non-normalized value. The default is to round to the 5th decimal place.

``` r
# Using round outside of debkeepr functions can lead to non-normalized value
round(c(9, 19, 11.999999), digits = 5)
#> [1]  9 19 12

# Normalization applied within rounding
deb_normalize(lsd = c(9, 19, 11.999999), round = 5)
#>      l s d
#> [1] 10 0 0

# Rounding is useful to return all pence values as whole numbers
deb_normalize(lsd = c(5, 11, 8.95), round = 0)
#>     l  s d
#> [1] 5 11 9
```

### Arithmetic functions

`debkeepr` contains functions to perform arithmetic operations such as addition, subtraction, multiplication, and division on lsd values.

There are two different ways to add lsd values. `deb_sum()` takes any number of `lsd` objects or objects that can be coerced to class `lsd` and returns an `lsd` object of length one that is the sum of the input values. As with all functions that contain more than one lsd value, all inputs must have the same bases, and any non-`lsd` objects will be coerced to the bases of the `lsd` object(s).

`deb_add()` has a different use case. It only accepts two sets of lsd values: `lsd1` and `lsd2`. If `lsd1` and `lsd2` are both single lsd values, the output will be equivalent to `deb_sum()`. However, if one or both of the inputs contain multiple values, the output will be the length of the longer list and each element of the list is added to the equivalent element of the other input. The lists of lsd values must either be the same length or one must be length one. `deb_subtract()` is equivalent to `deb_add()` with `lsd2` subtracted from `lsd1`.

``` r
# Use deb_sum to reduce multiple lsd values to a single value
deb_sum(single_lsd, c(8, 14, 11))
#>      l s d
#> [1] 14 8 7
deb_sum(mult_lsd, single_lsd)
#>      l s d
#> [1] 34 2 5

# deb_add is the same as deb_sum if inputs are single lsd values
deb_add(lsd1 = single_lsd, lsd2 = c(8, 14, 11))
#>      l s d
#> [1] 14 8 7

# If one input has multiple lsd values, the output will have the same number of lsd values.
# Here, £5 13s. 8d. is added to each lsd value in mult_lsd.
deb_add(lsd1 = mult_lsd, lsd2 = single_lsd)
#>      l  s  d
#> [1] 16  2  7
#> [2] 18  9 10
#> [3] 10 17  4

# deb_subtract has the same use case as deb_add
deb_subtract(lsd1 = c(8, 14, 11), lsd2 = single_lsd)
#>     l s d
#> [1] 3 1 3

# Subtract lsd value from multiple lsd values
deb_subtract(lsd1 = mult_lsd, lsd2 = single_lsd)
#>     l   s d
#> [1] 4  15 3
#> [2] 7   2 6
#> [3] 0 -10 0
```

Multiplication and division work similarly to `deb_add()` and `deb_subtract()`, but instead of using a second lsd value, `deb_multiply()` and `deb_divide()` multiply and divide a lsd values by a single value: `x`.

``` r
# Multiplcation
deb_multiply(lsd = single_lsd, x = 5)
#>      l s d
#> [1] 28 8 4

# Multiplication of multiple lsd values
deb_multiply(lsd = mult_lsd, x = 5)
#>      l  s  d
#> [1] 52  4  7
#> [2] 64  0 10
#> [3] 25 18  4

# Division
deb_divide(lsd = c(136, 17, 9), x = 5)
#>      l s   d
#> [1] 27 7 6.6

# Division of multiple lsd values
deb_divide(lsd = mult_lsd, x = 3)
#>     l  s       d
#> [1] 3  9 7.66667
#> [2] 4  5 4.66667
#> [3] 1 14 6.66667
```

Because `lsd` is always the first argument in `debkeepr` functions that deal with lsd vectors, `debkeepr` functions can be chained together with the pipe (`%>%`). For instance, you can find the revenue gained by each of two partners who worked on a commission of 3% for the sum of four separate sales.

``` r
# 3% commision between two partners
deb_sum(c(245, 14, 10), c(379, 16, 0), c(764, 9, 11), c(302, 4, 8)) %>% 
  deb_multiply(x = 0.03) %>% 
  deb_divide(x = 2)
#>      l s     d
#> [1] 25 7 8.175
```

Modifying the `round` argument may be especially useful when dealing with multiplication and division. For example, an lsd value divided by a number and then the result multiplied by the same number may not be equivalent because of the default rounding.

``` r
# Not equivalent due to default rounding
deb_divide(c(6, 8, 1), x = 3) %>% 
  deb_multiply(x = 3)
#>     l s       d
#> [1] 6 8 0.99999

# Can be solved by reducing the rounding argument
deb_divide(c(6, 8, 1), x = 3) %>% 
  deb_multiply(x = 3, round = 4)
#>     l s d
#> [1] 6 8 1
```

### Financial functions

Alongside arithmetic operations, `debkeepr` possesses functions to do common financial operations such as calculate interest and the exchange between different currencies. `deb_interest()` contains arguments for the interest rate, the duration over which the interest should be calculated, and whether the principal should be included in the returned value. The default interest rate is 6.25%, which was the standard interest rate in the Low Countries at the end of the 16th century.

``` r
# Interest rate at 6.25% over a 5 year period with principal
deb_interest(lsd = c(100, 0, 0), interest = 0.0625, duration = 5)
#>       l s d
#> [1] 131 5 0

# Calculate only the accrued interest
deb_interest(lsd = c(100, 0, 0), interest = 0.0625, duration = 5, with_principal = FALSE)
#>      l s d
#> [1] 31 5 0
```

Exchange between different currencies creates a number of potential problems, but `debkeepr` provides multiple ways to handle currency exchanges. The most straightforward method for calculating the exchange between two currencies is to use `deb_exchange()`, which uses the shillings unit to compare the currencies. If the exchange rate includes a pence value, you can either convert to a decimalized shillings or add the pence over the base of the pence as shown below. However, the exchange rate between currencies may not always be given in terms of shillings. For instance, a rate might be provided as 90d. This could be normalized to shillings, but in this case it may be easier to use `deb_multiply()` and compare 90d. to 240d. in a pound.

``` r
# Exchange between currencies at rate of 31 shillings or 1 to 1.55
deb_exchange(lsd = c(100, 0, 0), shillings_rate = 31)
#>       l s d
#> [1] 155 0 0

# Exchange rate of 31s. 4d.
deb_exchange(lsd = c(100, 0, 0), shillings_rate = 31 + 4/12)
#>       l  s d
#> [1] 156 13 4

# Exchange rate of 90d. using deb_multiply
deb_multiply(lsd = c(100, 0, 0), x = 90 / 240)
#>      l  s d
#> [1] 37 10 0
```

`debkeepr` also provides two functions that can help in dealing with exchange rates between currencies. `deb_exchange_rate()` accepts two lsd values to find the exchange rate between the values. The first argument, `lsd1`, is the fixed currency that is reduced to £1 and `lsd2` is the variable currency. Because it may not always make sense to return the exchange rate in a single format, this function has an option to return the exchange rate as a normalized lsd value, in terms of shillings and pence as used by `deb_exchange()`, or just in terms of pence. Though the latter two formats are not normalized, all options return an `lsd` object.

The second helper function is `deb_invert_rate()`. Given an exchange rate, `deb_invert_rate()` finds the exchange rate in the opposite direction. Like `deb_exchange_rate()`, it has options to return the exchange rate as a normalized lsd value, in terms of shillings and pence, or just in terms of pence. The default output for both `deb_exchange_rate()` and `deb_invert_rate()` is shillings and pence.

``` r
# Find the exchange rate between two values
deb_exchange_rate(lsd1 = c(100, 0, 0), lsd2 = c(166, 13, 4))
#>     l  s d
#> [1] 0 33 4

# Find the same rate in terms of pence
deb_exchange_rate(lsd1 = c(100, 0, 0), lsd2 = c(166, 13, 4), output = "pence")
#>     l s   d
#> [1] 0 0 400

# Can find the exchange in the opposite direction by switching lsd1 and lsd2
deb_exchange_rate(lsd1 = c(166, 13, 4), lsd2 = c(100, 0, 0))
#>     l  s d
#> [1] 0 12 0

# Or you can use deb_invert_rate
deb_invert_rate(exchange_rate = c(0, 33, 4))
#>     l  s d
#> [1] 0 12 0

# This can also be expressed in terms of pence
deb_invert_rate(exchange_rate = c(0, 33, 4), output = "pence")
#>     l s   d
#> [1] 0 0 144
```

The most challenging exchange between currencies takes place when the bases for the shillings and pence units differ between the two currencies. Though the system of 20-base shilling and 12-base penny was widespread, various currencies used other bases. `deb_convert_bases()` provides a way to convert from one base system to another. It also has a `ratio` argument that can be used similarly to `deb_multiply()` to include an exchange rate between currencies in addition to the base conversion. A good example is the conversion between the guilders monetary system from Holland and Flemish pounds, which were both in use in the seventeenth century. Flemish pounds used the 20 and 12 base system, but guilders consisted of 20 stuivers of 16 penningen. In addition, the guilders and Flemish pounds currencies were tied together at a rate of 6 guilders to £1 Flemish. `deb_convert_bases()` can perform this and various other conversions between forms of measurement that use the tripartite based system of lsd.

``` r
# Convert Flemish pounds to guilders
deb_convert_bases(lsd = c(104, 8, 3),
                  bases1 = c(20, 12),
                  bases2 = c(20, 16),
                  ratio = 6)
#>       l s d
#> [1] 626 9 8

# bases1 argument is not necessary with an lsd object
# Convert guilders to Flemish pounds
guilders <- deb_as_lsd(lsd = c(1224, 19, 8), bases = c(20, 16))

deb_convert_bases(lsd = guilders,
                  bases2 = c(20, 12),
                  ratio = 1 / 6)
#>       l s d
#> [1] 204 3 3

# Convert French crowns of 60 sous and 12 deniers to pound sterling
# at the rate of 72d. French crowns equals £1 sterling or 240d. sterling
deb_convert_bases(lsd = c(214, 50, 10),
                  bases1 = c(60, 12),
                  bases2 = c(20, 12),
                  ratio = 72 / 240)
#>      l s d
#> [1] 64 9 1

# Base conversion can also be done in concert with deb_exchange()
# Convert from guilders to pounds sterling at the rate of 12s. Flemish
deb_convert_bases(lsd = guilders,
                  bases2 = c(20, 12),
                  ratio = 1 / 6) %>% 
  deb_exchange(shillings_rate = 12)
#>       l s    d
#> [1] 122 9 11.4
```

### Decimalization

Sometimes it is useful to decimalize lsd values, reducing the values to a single unit with a base of 10. Alternatively, you may come across decimalized values that you want to expand to lsd values. `debkeepr` provides functions to convert between decimalized and non-decimalized values. One detail to keep in mind is that decimalization outputs a normal numeric vector, and so the `bases` attribute is dropped.

All of the decimalization functions follow the naming convention of `input_output`. For instance, `deb_lsd_l()` converts an lsd value to decimalized pounds, while conversion between decimalized pounds to an `lsd` object is done with `deb_l_lsd()`. There are functions to do the same process with both shillings and pence.

``` r
# decimalize pounds
deb_lsd_l(lsd = mult_lsd)
#> [1] 10.445833 12.808333  5.183333
deb_l_lsd(l = c(5.825, 10.666666666, 9.5))
#>      l  s d
#> [1]  5 16 6
#> [2] 10 13 4
#> [3]  9 10 0

# guilders to stuivers and back
deb_lsd_s(lsd = c(10, 12, 7), bases = c(20, 16))
#> [1] 212.4375
deb_s_lsd(s = 212.4375, bases = c(20, 16))
#>      l  s d
#> [1] 10 12 7

# decimalize pence
deb_lsd_d(lsd = c(10, 12, 7))
#> [1] 2551
deb_d_lsd(d = 2551)
#>      l  s d
#> [1] 10 12 7
```

Context 2: lsd list columns
---------------------------

Almost all of the functions that can be applied in the context of `lsd` objects can also be used in the context of an `lsd` list column in a data frame, even if the use of some functions make less sense than others. For instance, there is little cause to use `deb_exchange_rate()` on an `lsd` list column. The one major exception to this is `deb_sum()`, which does not work in the context of a data frame. However, there is a substitute that performs the same task in `deb_summarise()`. As the name for the latter function implies, `debkeepr` functions are designed to work within the [tidyverse](https://www.tidyverse.org) workflow and particularly with `dplyr::mutate()`, while `deb_summarise()` provides some of the functionality of `dplyr::summarise()` for an `lsd` list column. This section provides examples of manipulating `lsd` list columns with the help of [dplyr](https://dplyr.tidyverse.org).

``` r
# loan dplyr and create tibble with lsd list column
suppressPackageStartupMessages(library(dplyr))
lsd_tbl <- tibble(id = c(1, 3, 2, 1, 2),
                  lsd = deb_lsd(l = c(102, 215, 98, 251, 98),
                                s = c(18, 0, 9, 12, 4),
                                d = c(9, 11, 7, 5, 0)))
```

The “lsd” variable in `lsd_tbl` was created with `deb_lsd()` using the default bases of `c(20, 12)`, and so all functions will automatically use these bases unless they are explicitly converted. It is always possible to check the bases of an `lsd` list column with `deb_bases()`.

``` r
deb_bases(lsd_tbl$lsd)
#>  s  d 
#> 20 12
```

Most of the typical single-table verbs from `dplyr` work with an `lsd` list column as expected, though it is not possible to `arrange()` a data frame by an `lsd` list column. As noted above, the most useful workflow is to use `mutate()` or `transmute()` with `debkeepr` functions to create new variables. Thus, it is possible to multiply an `lsd` list column or calculate the accrued interest over a certain period of time.

``` r
# multiplication of an lsd list column
lsd_tbl %>% 
  mutate(lsd_x5 = deb_multiply(lsd = lsd, x = 5))
#> # A tibble: 5 x 3
#>      id lsd        lsd_x5    
#>   <dbl> <S3: lsd>  <S3: lsd> 
#> 1     1 102, 18, 9 514, 13, 9
#> 2     3 215, 0, 11 1075, 4, 7
#> 3     2 98, 9, 7   492, 7, 11
#> 4     1 251, 12, 5 1258, 2, 1
#> 5     2 98, 4, 0   491, 0, 0

# Interest at 8% after a 5 year period
lsd_tbl %>% 
  mutate(total = deb_interest(lsd = lsd, interest = 0.08, duration = 5))
#> # A tibble: 5 x 3
#>      id lsd        total          
#>   <dbl> <S3: lsd>  <S3: lsd>      
#> 1     1 102, 18, 9 144, 2, 3      
#> 2     3 215, 0, 11 301.0, 1.0, 3.4
#> 3     2 98, 9, 7   137, 17, 5     
#> 4     1 251, 12, 5 352.0, 5.0, 4.6
#> 5     2 98, 4, 0   137.0, 9.0, 7.2

# Make multiple new variables to find total due and interest charged
lsd_tbl %>% 
  mutate(total = deb_interest(lsd = lsd, interest = 0.08, duration = 5),
         interest = deb_subtract(lsd1 = total, lsd2 = lsd))
#> # A tibble: 5 x 4
#>      id lsd        total           interest         
#>   <dbl> <S3: lsd>  <S3: lsd>       <S3: lsd>        
#> 1     1 102, 18, 9 144, 2, 3       41, 3, 6         
#> 2     3 215, 0, 11 301.0, 1.0, 3.4 86.0, 0.0, 4.4   
#> 3     2 98, 9, 7   137, 17, 5      39, 7, 10        
#> 4     1 251, 12, 5 352.0, 5.0, 4.6 100.0, 12.0, 11.6
#> 5     2 98, 4, 0   137.0, 9.0, 7.2 39.0, 5.0, 7.2
```

It is also possible to create a new `lsd` list column with different bases. For instance, the `lsd` list column in `lsd_tbl` can be treated as containing Flemish pounds, which can be converted to guilders.

``` r
# Create guilders lsd column from flemish pounds
lsd_tbl %>% 
  mutate(guilders = deb_convert_bases(lsd = lsd, bases2 = c(20, 16), ratio = 6))
#> # A tibble: 5 x 3
#>      id lsd        guilders   
#>   <dbl> <S3: lsd>  <S3: lsd>  
#> 1     1 102, 18, 9 617, 12, 8 
#> 2     3 215, 0, 11 1290, 5, 8 
#> 3     2 98, 9, 7   590, 17, 8 
#> 4     1 251, 12, 5 1509, 14, 8
#> 5     2 98, 4, 0   589, 4, 0

# save the tbl for use below
lsd_tbl2 <- lsd_tbl %>% 
  mutate(guilders = deb_convert_bases(lsd = lsd, bases2 = c(20, 16), ratio = 6))
```

Decimalization of lsd values is particularly useful in the context of `lsd` list columns, because there may be times when it is easier to work with a normal numeric variable instead of a list column. In addition, the ability to return decimalized values to an `lsd` list column means that there is little trouble in going between the two, though it should be remembered that the `bases` attribute is lost in the process of decimalization.

``` r
# decimalize lsd values and back
lsd_tbl %>% 
  mutate(denarii = deb_lsd_d(lsd),
         lsd2 = deb_d_lsd(denarii, bases = c(20, 12)))
#> # A tibble: 5 x 4
#>      id lsd        denarii lsd2      
#>   <dbl> <S3: lsd>    <dbl> <S3: lsd> 
#> 1     1 102, 18, 9   24705 102, 18, 9
#> 2     3 215, 0, 11   51611 215, 0, 11
#> 3     2 98, 9, 7     23635 98, 9, 7  
#> 4     1 251, 12, 5   60389 251, 12, 5
#> 5     2 98, 4, 0     23568 98, 4, 0
```

The exception to the normal function of `dplyr` single-table functions is `summarise()`, which is not meant to be used on list columns. `deb_summarise()` provides some of the functionality of `summarise()` in giving a method to find the sum of an `lsd` list column and can be used it its place. To find other summarizing values it is always possible to decimalize an `lsd` list column and perform `summarise()` functions on this variable, returning to an `lsd` list column when necessary. `deb_summarise()` can find the sum of an `lsd` list column or can work on a grouped data frame. It can also be used on multiple `lsd` list columns at a time, because the function is based on `summarise_at()`.

``` r
# sum of lsd list column
lsd_tbl %>% 
  deb_summarise(lsd)
#> # A tibble: 1 x 1
#>   lsd      
#>   <S3: lsd>
#> 1 766, 5, 8

# sum on a grouped data frame
lsd_tbl %>% 
  group_by(id) %>% 
  deb_summarise(lsd)
#> # A tibble: 3 x 2
#>      id lsd       
#>   <dbl> <S3: lsd> 
#> 1     1 354, 11, 2
#> 2     2 196, 13, 7
#> 3     3 215, 0, 11

# sum of multiple lsd list columns
lsd_tbl2 %>% 
  group_by(id) %>% 
  deb_summarise(lsd, guilders)
#> # A tibble: 3 x 3
#>      id lsd        guilders  
#>   <dbl> <S3: lsd>  <S3: lsd> 
#> 1     1 354, 11, 2 2127, 7, 0
#> 2     2 196, 13, 7 1180, 1, 8
#> 3     3 215, 0, 11 1290, 5, 8
```

Context 3: transactions data frame
----------------------------------

Transaction data frames present a way to record data from an account book as a native R object. A transactions data frame consist of three necessary variables: credit and debit variables to record the creditor and debtor accounts of each transaction and an `lsd` list column for the value of the transactions. Following the principles of double-entry bookkeeping, the credit account is the account that gives or sends the value, and the debit account receives the value. The `debkeepr` functions that work specifically on transaction data frames use "credit" and "debit" as the default names for these variables, though one could follow the convention of [network analysis](http://kateto.net/network-visualization) and name the variables "from" and "to".

Here is an example transactions data frame with four accounts named “wheat”, “silk”, “linen”, and “cash” and 15 transactions with values randomly created:

``` r
accounts <- c("wheat", "silk", "linen", "cash")
set.seed(240)
transactions_tbl <- tibble(credit = sample(accounts, 15, replace = TRUE),
                           debit = sample(accounts, 15, replace = TRUE),
                           lsd = deb_lsd(l = sample(20:100, 15, replace = TRUE),
                                         s = sample(1:19, 15, replace = TRUE),
                                         d = sample(1:11, 15, replace = TRUE)))
```

`debkeepr` possesses an account family of functions that are designed to analyze a transaction data frame such as `transactions_tbl`. `deb_account()` provides information about the total credit and debit and the current balance of a single account. `deb_account_summary()` gives the same type of information but includes all accounts in the transactions data frame.

``` r
# Credit, debit, and current values for the "cash" account
deb_account(df = transactions_tbl,
            account_id = "cash",
            credit = credit,
            debit = debit,
            lsd = lsd)
#> # A tibble: 3 x 2
#>   relation lsd         
#>   <chr>    <S3: lsd>   
#> 1 credit   194, 14, 3  
#> 2 debit    375, 3, 0   
#> 3 current  -180, -8, -9

# Credit, debit, and current values for all accounts
deb_account_summary(df = transactions_tbl)
#> # A tibble: 12 x 3
#>    account_id relation lsd         
#>    <chr>      <chr>    <S3: lsd>   
#>  1 cash       credit   194, 14, 3  
#>  2 cash       debit    375, 3, 0   
#>  3 cash       current  -180, -8, -9
#>  4 linen      credit   292, 16, 8  
#>  5 linen      debit    261, 5, 5   
#>  6 linen      current  31, 11, 3   
#>  7 silk       credit   413, 4, 3   
#>  8 silk       debit    286, 7, 3   
#>  9 silk       current  126, 17, 0  
#> 10 wheat      credit   99, 4, 2    
#> 11 wheat      debit    77, 3, 8    
#> 12 wheat      current  22, 0, 6
```

The remaining functions build on `deb_account_summary()`. Three functions simplify the information produced by `deb_account_summary`: `deb_credit()` shows the total credit for each account, `deb_debit()` does the same for the debits, and `deb_current()` shows on the current value for each account.

``` r
# Total credit for each account
deb_credit(df = transactions_tbl)
#> # A tibble: 4 x 2
#>   account_id lsd       
#>   <chr>      <S3: lsd> 
#> 1 cash       194, 14, 3
#> 2 linen      292, 16, 8
#> 3 silk       413, 4, 3 
#> 4 wheat      99, 4, 2

# Total debit for all accounts by getting sum of deb_debit
deb_debit(df = transactions_tbl) %>% 
  deb_summarise(lsd = lsd)
#> # A tibble: 1 x 1
#>   lsd       
#>   <S3: lsd> 
#> 1 999, 19, 4

# Current value of each account
deb_current(df = transactions_tbl)
#> # A tibble: 4 x 2
#>   account_id lsd         
#>   <chr>      <S3: lsd>   
#> 1 cash       -180, -8, -9
#> 2 linen      31, 11, 3   
#> 3 silk       126, 17, 0  
#> 4 wheat      22, 0, 6
```

`deb_open()` is similar to `deb_current()`, but it removes any accounts that have been closed by being zeroed out. This is useful if there are many accounts in the transactions data frame that have been closed. In the example of `transactions_tbl` all accounts are open, and so `deb_open()` has the same result as `deb_current()`. `deb_balance()` shows the total credit and debit remaining in the transactions data frame. The values for credit and debit will always be the same in `deb_balance()`, as there should always be the same amount of credit as debit in an account book.

``` r
# Balance remaining on transactions_df
deb_balance(transactions_tbl)
#> # A tibble: 2 x 2
#>   relation lsd      
#>   <chr>    <S3: lsd>
#> 1 credit   180, 8, 9
#> 2 debit    180, 8, 9
```

Useful works on monetary systems and the history of accounting
--------------------------------------------------------------

-   Peter Spufford, *Money and its Use in Medieval Europe* (Cambridge: Cambridge University Press, 1988), especially pages 411–414, for a discussion of money of account in medieval Europe.
-   John Richard Edwards and Stephen P. Walker, eds. *The Routledge Companion to Accounting History* (New York: Routledge Taylor & Francis Group, 2009).
-   Jacob Soll, *The Reckoning: Financial Accountability and the Rise and Fall of Nations* (New York: Basic Books, 2014).
-   John Geijsbeek, *Ancient Double-Entry Bookkeeping: Lucas Pacioli’s Treatise (A.D. 1494, the Earliest Known Writer on Bookkeeping) Reproduced and Translated with Reproductions, Notes, and Abstracts from Manzoni, Pietra, Mainardi, Ympyn, Stevin, and Dafforne* (Denver: John Geijsbeek, 1914).
