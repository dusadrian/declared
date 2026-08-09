# Get / Set measurement levels for declared objects

Functions to extract information about the measurement levels of a
variable (if already present), or to specify such measurement levels.

## Usage

``` r
measurement(x)

measurement(x) <- value
```

## Arguments

- x:

  A declared vector.

- value:

  A single character string of measurement levels, separated by commas.

## Value

A character vector.

## Details

This function creates an attribute called `"measurement"` to a declared
object. This is an optional feature, at this point for purely aesthetic
reasons, but it might become useful in the future to (automatically)
determine if a declared object is suitable for a certain statistical
analysis, for instance regression requires quantitative variables, while
some declared objects are certainly categorical despite using numbers to
denote categories.

It distinguishes between `"categorical"` and `"quantitative"` types of
variables, and additionally recognizes `"nominal"` and `"ordinal"` as
categorical, and similarly recognizes `"interval"`, `"ratio"`,
`"discrete"` and `"continuous"` as quantitative.

The words `"qualitative"` is treated as a synonym for `"categorical"`,
and the words `"metric"` and `"numeric"` are treated as synonyms for
`"quantitative"`, respectively.

## See also

Other labelling functions: [`drop_undeclare`](drop_undeclare.md),
[`labels`](labels.md)

## Examples

``` r
x <- declared(
    c(-2, 1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
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

measurement(x)
#> [1] "Unspecified, but likely categorical"

# automatically recognized as categorical
measurement(x) <- "ordinal"

measurement(x)
#> [1] "categorical, ordinal"

# the same with
measurement(x) <- "categorical, ordinal"

set.seed(1890)
x <- declared(
    sample(c(18:90, -91), 20, replace = TRUE),
    labels = c("No answer" = -91),
    na_values = -91,
    label = "Respondent's age"
)

# automatically recognized as quantitative
measurement(x) <- "discrete"

measurement(x)
#> [1] "quantitative, discrete"

# the same with
measurement(x) <- "metric, discrete"
```
