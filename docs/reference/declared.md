# Labelled vectors with declared missing values

The labelled vectors are mainly used to analyse social science data, and
the missing values declaration is an important step in the analysis.

## Usage

``` r
as.declared(x, ...)

declared(
  x,
  labels = NULL,
  na_values = NULL,
  na_range = NULL,
  label = NULL,
  measurement = NULL,
  decimals = NULL,
  llevels = FALSE,
  ...
)

is.declared(x)

anyNAdeclared(x)
```

## Arguments

- x:

  A numeric vector to label, or a declared labelled vector (for
  `undeclare`)

- ...:

  Other arguments used by various other methods

- labels:

  A named vector or `NULL`. The vector should be the same type as `x`.
  Unlike factors, labels don't need to be exhaustive: only a fraction of
  the values might be labelled

- na_values:

  A vector of values that should also be considered as missing

- na_range:

  A numeric vector of length two giving the (inclusive) extents of the
  range. Use `-Inf` and `Inf` if you want the range to be open ended

- label:

  A short, human-readable description of the vector

- measurement:

  Optional, user specified measurement level

- decimals:

  Optional, maximum number of decimal places used for display only

- llevels:

  Logical, when `x` is a factor only use those levels that have labels

## Value

`declared()` and `as.declared()` return labelled vector of class
"declared". When applied to a data frame, `as.declared()` will return a
corresponding data frame with declared variables. `is.declared()` and
`anyNAdeclared()` return a logical value.

## Details

The `declared` objects are very similar to the `haven_labelled_spss`
objects from package **haven**. It has exactly the same arguments, but
it features a fundamental difference in the treatment of (declared)
missing values.

In package **haven**, existing values are treated as if they were
missing. By contrast, in package **declared** the NA values are treated
as if they were existing values.

This difference is fundamental and points to an inconsistency in package
**haven**: while existing values can be identified as missing using the
function [`is.na()`](https://rdrr.io/r/base/NA.html), they are in fact
present in the vector and other packages (most importantly the base
ones) do not know these values should be treated as missing.

Consequently, the existing values are interpreted as missing only by
package **haven**. Statistical procedures will use those values as if
they were valid values.

Package **declared** approaches the problem in exactly the opposite way:
instead of treating existing values as missing, it treats (certain) NA
values as existing. It does that by storing an attribute containing the
indices of those NA values which are to be treated as declared missing
values, and it refreshes this attribute each time the declared object is
changed.

This is a trade off and has important implications when subsetting
datasets: all declared variables get this attribute refreshed, which
consumes some time depending on the number of variables in the data.

The generic function `as.declared()` attempts to coerce only the
compatible types of objects, namely `haven_labelled` and `factor`s.
Dedicated class methods can be written for any other type of object, and
users are free to write their own. To end of with a declared object,
additional metadata is needed such as value labels, which values should
be treated as missing etc.

The measurement level is optional and, for the moment, purely aesthetic.
It might however be useful to (automatically) determine if a declared
object is suitable for a certain statistical analysis, for instance
regression requires quantitative variables, while some declared objects
are certainly categorical despite using numbers to denote categories.

It distinguishes between `"categorical"` and `"quantitative"` types of
variables, and additionally recognizes `"nominal"` and `"ordinal"` as
categorical, and similarly recognizes `"interval"`, `"ratio"`,
`"discrete"` and `"continuous"` as quantitative.

## Examples

``` r
x <- declared(
    c(1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_values = -1
)

x
#> <declared<numeric>[6]>
#> [1]      1      2      3      4      5 NA(-1)
#> Missing values: -1
#> 
#> Labels:
#>  value label
#>      1  Good
#>      5   Bad
#>     -1    DK

is.na(x)
#> [1] FALSE FALSE FALSE FALSE FALSE  TRUE

x > 0
#> [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE

x == -1
#> [1] FALSE FALSE FALSE FALSE FALSE  TRUE

# Values are actually placeholder for categories,
# so labels work as if they were factors:
x == "DK"
#> [1] FALSE FALSE FALSE FALSE FALSE  TRUE


# when newly added values are already declared as missing,
# they are automatically coerced
c(x, 2, -1)
#> <declared<numeric>[8]>
#> [1]      1      2      3      4      5 NA(-1)      2 NA(-1)
#> Missing values: -1
#> 
#> Labels:
#>  value label
#>     -1    DK
#>      1  Good
#>      5   Bad

# switch NAs with their original values
undeclare(x)
#> <declared<numeric>[6]>
#> [1]  1  2  3  4  5 -1
#> 
#> Labels:
#>  value label
#>      1  Good
#>      5   Bad
#>     -1    DK

as.character(x)
#> [1] "Good" "2"    "3"    "4"    "Bad"  NA    

# Returning values instead of categories
as.character(x, values = TRUE)
#> [1] "1" "2" "3" "4" "5" NA 

anyNAdeclared(x) # contains declared missing values
#> [1] TRUE

anyNAdeclared(c(1:5, NA)) # no declared missing values
#> [1] FALSE
```
