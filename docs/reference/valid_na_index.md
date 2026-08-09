# Validate the declared missing values index

Check whether every position stored in the `na_index` attribute
identifies a genuine missing value in the vector. This can detect when
external code has changed a vector without refreshing its declared
missing values metadata.

## Usage

``` r
valid_na_index(x)
```

## Arguments

- x:

  An atomic vector, possibly containing an `na_index` attribute.

## Value

A logical value. `TRUE` when every indexed value is a genuine `NA`;
otherwise `FALSE`.

## Details

The check examines only the positions in `na_index` and stops at the
first invalid position. A missing or empty `na_index` attribute is
valid.

## Examples

``` r
x <- declared(c(1, -1, 3), na_values = -1)

valid_na_index(x)
#> [1] TRUE

attr(x, "na_index") <- 1
valid_na_index(x)
#> [1] FALSE
```
