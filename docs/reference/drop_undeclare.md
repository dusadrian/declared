# Drop information / undeclare labelled objects

A function to obtain a version of the object with all information about
declared missing values, dropped

## Usage

``` r
undeclare(x, drop = FALSE, ...)

drop_na(x, drop_labels = TRUE)
```

## Arguments

- x:

  A labelled object with declared missing values

- drop:

  Logical, drop all attributes

- ...:

  Other internal arguments

- drop_labels:

  Logical, drop the labels for the declared missing values

## Value

A declared labelled object.

## Details

The function `undeclare()` replaces the NA entries into their original
numeric values, and drops all attributes related to missing values:
`na_values`, `na_range` and `na_index`, and it preserves the labels
referring to the missing values.

The result can be a regular vector (dropping all attributes, including
the class "declared") by activating the argument `drop`.

Function `drop_na()` transforms the declared missing values in regular
empty NAs, and the labels referring to the missing values are deleted by
default.

Function [`drop()`](https://rdrr.io/r/base/drop.html) deletes all
attributes.

## See also

Other labelling functions: [`labels`](labels.md),
[`measurement()`](measurement.md)

## Examples

``` r
x <- declared(
    c(-2, 1:5, -1),
    labels = c("Good" = 1, "Bad" = 5, "DK" = -1),
    na_values = c(-1, -2),
    label = "Test variable"
)

x
#> <declared<numeric>[7]> Test variable
#> [1] NA(-2)      1      2      3      4      5 NA(-1)
#> Missing values: -1, -2
#> 
#> Labels:
#>  value label
#>      1  Good
#>      5   Bad
#>     -1    DK

undeclare(x)
#> <declared<numeric>[7]> Test variable
#> [1] -2  1  2  3  4  5 -1
#> 
#> Labels:
#>  value label
#>      1  Good
#>      5   Bad
#>     -1    DK

drop_na(x)
#> <declared<numeric>[7]> Test variable
#> [1] NA  1  2  3  4  5 NA
#> 
#> Labels:
#>  value label
#>      1  Good
#>      5   Bad

drop(x)
#> [1] NA  1  2  3  4  5 NA

undeclare(x, drop = TRUE)
#> [1] -2  1  2  3  4  5 -1

# similar to:
drop(undeclare(x))
#> [1] -2  1  2  3  4  5 -1
```
