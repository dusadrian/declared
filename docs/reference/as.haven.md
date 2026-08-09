# Coerce to haven / labelled objects

Convert declared labelled objects to haven labelled objects

## Usage

``` r
as.haven(x, ...)
```

## Arguments

- x:

  A declared labelled vector

- ...:

  Other arguments used by various methods

## Value

A labelled vector of class "haven_labelled_spss".

## Details

This is a function that reverses the process of
[`as.declared()`](https://dusadrian.github.io/declared/reference/declared.md),
making a round trip between `declared` and `haven_labelled_spss`
classes.

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

as.haven(x)
#> <labelled_spss<double>[6]>
#> [1]  1  2  3  4  5 -1
#> Missing values: -1
#> 
#> Labels:
#>  value label
#>      1  Good
#>      5   Bad
#>     -1    DK
```
