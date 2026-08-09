# Get / Declare value labels

Functions to extract information about the declared variable / value
labels, or to declare such values if they are present in the data.

## Usage

``` r
label(x)

label(x, ...) <- value

labels(x) <- value
```

## Arguments

- x:

  Any vector of values that should be declared as missing (for `labels`)
  or a numeric vector of length two giving the (inclusive) extents of
  the range of missing values (for `label`).

- ...:

  Other arguments, for internal use.

- value:

  The variable label, or a list of (named) variable labels

## Value

`labels()` will return a named vector.

`label()` will return a single character string.

## Details

The function `labels()` is a adaptation of the base function to the
objects of class `declared`. In addition to the regular arguments, it
has two additional (logical) arguments called `prefixed` (FALSE by
default), to retrieve the value labels prefixed with their values, and
`print_as_df` (TRUE by default) to print the result as a data frame.

## See also

Other labelling functions: [`drop_undeclare`](drop_undeclare.md),
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

labels(x)
#>  value label
#>      1  Good
#>      5   Bad
#>     -1    DK

labels(x, prefixed = TRUE)
#>  value    label
#>      1 [1] Good
#>      5  [5] Bad
#>     -1  [-1] DK

labels(x) <- c("Good" = 1, "Bad" = 5, "DK" = -1, "Not applicable" = -2)

label(x)
#> [1] "Test variable"

label(x) <- "This is a proper label"

x
#> <declared<numeric>[7]> This is a proper label
#> [1] NA(-2)      1      2      3      4      5 NA(-1)
#> Missing values: -1, -2
#> 
#> Labels:
#>  value          label
#>      1           Good
#>      5            Bad
#>     -1             DK
#>     -2 Not applicable
```
