# Get / Declare missing values

Functions to extract information about the declared missing values, or
to declare such values if they are present in the data.

## Usage

``` r
missing_range(x)

missing_range(x) <- value

missing_values(x)

missing_values(x) <- value
```

## Arguments

- x:

  A vector.

- value:

  Any vector of values that should be declared as missing (for
  `missing_values`) or a numeric vector of length two giving the
  (inclusive) extents of the range of missing values (for
  `missing_range`).

## Value

`missing_values()` will return a vector of one or more values.

`missing_range()` will return a numeric vector of length 2.

## Examples

``` r
x <- declared(c(-2, 1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1, NotApplicable = -2),
    na_values = c(-1, -2)
)
x
#> <declared<numeric>[7]>
#> [1] NA(-2)      1      2      3      4      5 NA(-1)
#> Missing values: -1, -2
#> 
#> Labels:
#>  value         label
#>      1          Good
#>      5           Bad
#>     -1            DK
#>     -2 NotApplicable

missing_values(x)
#> [1] -1 -2

missing_range(x) <- c(-10, -7)

missing_range(x)
#> [1] -10  -7
```
