# Test the presence of empty (undeclared) missing values

Functions that indicate which elements are empty `NA` missing values, in
contrast to declared missing values.

## Usage

``` r
is.empty(x)

anyNAempty(x)
```

## Arguments

- x:

  A vector

## Value

A logical vector.

## Details

All missing values, declared or undeclared, as stored as regular `NA`
values, therefore the base function `is_na()` does not differentiate
between them.

These functions are specifically adapted to objects of class
`"declared"`, to return a truth value only for those elements that are
completely missing with no reason.

## Examples

``` r
x <- declared(
    c(1:2, -91),
    labels = c(Good = 1, Bad = 2, Missing = -91),
    na_values = -91
)

x
#> <declared<numeric>[3]>
#> [1]       1       2 NA(-91)
#> Missing values: -91
#> 
#> Labels:
#>  value   label
#>      1    Good
#>      2     Bad
#>    -91 Missing

is.empty(x) # FALSE FALSE FALSE
#> [1] FALSE FALSE FALSE

anyNAempty(x) # FALSE
#> [1] FALSE

x <- c(x, NA)

is.empty(x) # FALSE FALSE FALSE  TRUE
#> [1] FALSE FALSE FALSE  TRUE

anyNAempty(x) # TRUE
#> [1] TRUE
```
