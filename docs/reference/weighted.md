# Compute weighted summaries for declared objects

Functions to compute weighted tables or summaries, based on a vector of
frequency weights. These are reimplementations of various existing
functions, adapted to objects of class `"declared"` (see Details below)

## Usage

``` r
wIQR(x, wt = NULL, na.rm = FALSE, ...)

wfivenum(x, wt = NULL, na.rm = FALSE)

wmean(x, wt = NULL, trim = 0, na.rm = TRUE)

wmeasures(
  x,
  what = c("n", "mode", "mean", "median", "range", "var", "sd"),
  wt = NULL,
  na.rm = TRUE,
  ...
)

wmedian(x, wt = NULL, na.rm = TRUE, ...)

wmode(x, wt = NULL)

wquantile(x, wt = NULL, probs = seq(0, 1, 0.25), na.rm = TRUE, ...)

wsd(x, wt = NULL, method = NULL, na.rm = TRUE)

wstandardize(x, wt = NULL, na.rm = TRUE)

wsummary(x, wt = NULL, ...)

wtable(
  x,
  y = NULL,
  wt = NULL,
  values = TRUE,
  valid = TRUE,
  observed = TRUE,
  vlabel = FALSE
)

wvar(x, wt = NULL, method = NULL, na.rm = TRUE)
```

## Arguments

- x:

  A numeric vector for summaries, or declared / factor for frequency
  tables

- wt:

  A numeric vector of frequency weights

- na.rm:

  Logical, should the empty missing values be removed?

- ...:

  Further arguments passed to or from other methods.

- trim:

  A fraction (0 to 0.5) of observations to be trimmed from each end of x
  before the mean is computed. Values of trim outside that range are
  taken as the nearest endpoint.

- what:

  A character vector or a named list of functions, identifying what to
  compute.

- probs:

  Numeric vector of probabilities with values in \[0,1\]

- method:

  Character, specifying how the result is scaled, see 'Details' below.

- y:

  An optional variable, to create crosstabs; must have the same length
  as x

- values:

  Logical, print the values in the table rows

- valid:

  Logical, print separate percent distribution for valid values, if any
  missing values are present; for cross tables, use valid values only

- observed:

  Logical, print the observed categories only

- vlabel:

  Logical, print the variable label, if existing

## Value

A vector of (weighted) values.

## Details

In an expression evaluated in the context of a data frame, such as
[`admisc::using()`](https://rdrr.io/pkg/admisc/man/using.html), `.` can
be used as a placeholder for all variables in the current dataset.

Weighted summaries

A frequency table is usually performed for a categorical variable,
displaying the frequencies of the respective categories. Note that
general variables containing text are not necessarily factors, despite
having a small number of characters.

A general table of frequencies, using the base function
[`table()`](https://rdrr.io/r/base/table.html), ignores the defined
missing values (which are all stored as NAs). The reimplementation of
this function in `wtable()` takes care of this detail, and presents
frequencies for each separately defined missing values. Similar
reimplementations for the other functions have the same underlying
objective.

It is also possible to perform a frequency table for numerical
variables, if the number of values is limited (an arbitrary and
debatable upper limit of 15 is used). An example of such variable can be
the number of children, where each value can be interpreted as a class,
containing a single value (for instance 0 meaning the category of people
with no children).

Objects of class `declared` are not pure categorical variables (R
factors) but they are nevertheless interpreted as if they were factors,
to allow producing frequency tables. Given the high similarity with
package **`haven`**, objects of class `haven_labelled_spss` are
automatically coerced to objects of class `declared` and treated
accordingly.

The argument `values` makes sense only when the input is of family class
`declared`, otherwise for regular (base R) factors the values are just a
sequence of numbers.

The later introduced argument `observed` is useful in situations when a
variable has a very large number of potential values, and a smaller
subset of actually observed ones. As an example, the variable
“Occupation” has hundreds of possible values in the ISCO08 codelist, and
not all of them might be actually observed. When activated, this
argument restricts the printed frequency table to the subset of observed
values only.

The argument `method` can be one of `"unbiased"` or `"ML"`.

When this is set to `"unbiased"`, the result is an unbiased estimate
using Bessel's correction. When this is set to `"ML"`, the result is the
maximum likelihood estimate for a Gaussian distribution.

The argument `wt` refers only to frequency weights. Users should be
aware of the differences between frequency weights, analytic weights,
probability weights, design weights, post-stratification weights etc.
For purposes of inferential testing, Thomas Lumley's package
**`survey`** should be employed.

If no frequency weights are provided, the result is identical to the
corresponding base functions.

The function `wquantile()` extensively borrowed ideas from packages
**`stats`** and **`Hmisc`**, to ensure a constant interpolation that
would produce the same quantiles if no weights are provided or if all
weights are equal to 1.

Other arguments can be passed to the stats function
[`quantile()`](https://rdrr.io/r/stats/quantile.html) via the three dots
`...` argument, and their extensive explanation is found in the
corresponding stats function's help page.

For all functions, the argument `na.rm` refers to the empty missing
values and its default is set to TRUE. The declared missing values are
automatically eliminated from the summary statistics, even if this
argument is deactivated.

The function `wmode()` returns the weighted mode of a variable. Unlike
the other functions where the prefix `w` signals a weighted version of
the base function with the same name, this has nothing to do with the
base function [`mode()`](https://rdrr.io/r/base/mode.html) which refers
to the storage mode / type of an R object.

## Author

Adrian Dusa

## Examples

``` r
set.seed(215)

# a pure categorical variable
x <- factor(sample(letters[1:5], 215, replace = TRUE))
wtable(x)
#> 
#>   fre    rel   per   cpd
#>   ----------------------
#> a  37  0.172  17.2  17.2 
#> b  45  0.209  20.9  38.1 
#> c  43  0.200  20.0  58.1 
#> d  44  0.205  20.5  78.6 
#> e  46  0.214  21.4 100.0 
#>   ----------------------
#>   215  1.000 100.0
#> 


# simulate number of children
x <- sample(0:4, 215, replace = TRUE)
wtable(x)
#> 
#>   fre    rel   per   cpd
#>   ----------------------
#> 0  36  0.167  16.7  16.7 
#> 1  41  0.191  19.1  35.8 
#> 2  49  0.228  22.8  58.6 
#> 3  45  0.209  20.9  79.5 
#> 4  44  0.205  20.5 100.0 
#>   ----------------------
#>   215  1.000 100.0
#> 

# simulate a Likert type response scale from 1 to 7
values <- sample(c(1:7, -91), 215, replace = TRUE)
x <- declared(values, labels = c("Good" = 1, "Bad" = 7))
wtable(x)
#> 
#>          fre    rel   per   cpd
#>          ----------------------
#> -91       16  0.074   7.4   7.4 
#>   1 Good  25  0.116  11.6  19.1 
#>   2       25  0.116  11.6  30.7 
#>   3       32  0.149  14.9  45.6 
#>   4       37  0.172  17.2  62.8 
#>   5       24  0.112  11.2  74.0 
#>   6       21  0.098   9.8  83.7 
#>   7 Bad   35  0.163  16.3 100.0 
#>          ----------------------
#>          215  1.000 100.0
#> 


# Defining missing values
missing_values(x) <- -91
wtable(x)
#> 
#>          fre    rel   per   vld   cpd
#>          ----------------------------
#>   1 Good  25  0.116  11.6  12.6  12.6 
#>   2       25  0.116  11.6  12.6  25.1 
#>   3       32  0.149  14.9  16.1  41.2 
#>   4       37  0.172  17.2  18.6  59.8 
#>   5       24  0.112  11.2  12.1  71.9 
#>   6       21  0.098   9.8  10.6  82.4 
#>   7 Bad   35  0.163  16.3  17.6 100.0 
#> -----
#> -91       16  0.074   7.4 
#>          ----------------------------
#>          215  1.000 100.0
#> 


# Defined missing values with labels
values <- sample(c(1:7, -91, NA), 215, replace = TRUE)
x <- declared(
    values,
    labels = c("Good" = 1, "Bad" = 7, "Don't know" = -91),
    na_values = -91
)

wtable(x)
#> 
#>                fre    rel   per   vld   cpd
#>                ----------------------------
#>   1 Good        20  0.093   9.3  12.0  12.0 
#>   2             33  0.153  15.3  19.9  31.9 
#>   3             27  0.126  12.6  16.3  48.2 
#>   4             27  0.126  12.6  16.3  64.5 
#>   5             22  0.102  10.2  13.3  77.7 
#>   6             22  0.102  10.2  13.3  91.0 
#>   7 Bad         15  0.070   7.0   9.0 100.0 
#> -----
#> -91 Don't know  24  0.112  11.2 
#>  NA             25  0.116  11.6 
#>                ----------------------------
#>                215  1.000 100.0
#> 

# Including the values in the table of frequencies
wtable(x, values = TRUE)
#> 
#>                fre    rel   per   vld   cpd
#>                ----------------------------
#>   1 Good        20  0.093   9.3  12.0  12.0 
#>   2             33  0.153  15.3  19.9  31.9 
#>   3             27  0.126  12.6  16.3  48.2 
#>   4             27  0.126  12.6  16.3  64.5 
#>   5             22  0.102  10.2  13.3  77.7 
#>   6             22  0.102  10.2  13.3  91.0 
#>   7 Bad         15  0.070   7.0   9.0 100.0 
#> -----
#> -91 Don't know  24  0.112  11.2 
#>  NA             25  0.116  11.6 
#>                ----------------------------
#>                215  1.000 100.0
#> 


# An example involving multiple variables
DF <- data.frame(
    Area = declared(
        sample(1:2, 215, replace = TRUE, prob = c(0.45, 0.55)),
        labels = c(Rural = 1, Urban = 2)
    ),
    Gender = declared(
        sample(1:2, 215, replace = TRUE, prob = c(0.55, 0.45)),
        labels = c(Males = 1, Females = 2)
    ),
    Age = sample(18:90, 215, replace = TRUE),
    Children = sample(0:5, 215, replace = TRUE)
)

wtable(DF$Gender)
#> 
#>           fre    rel   per   cpd
#>           ----------------------
#> 1 Males   119  0.553  55.3  55.3 
#> 2 Females  96  0.447  44.7 100.0 
#>           ----------------------
#>           215  1.000 100.0
#> 

wsd(DF$Age)
#> [1] 20.19372

wmeasures(DF[c("Age", "Children")], what = c("n", "mean", "sd"))
#> 
#>           n    mean     sd  
#> Age      215  54.628  20.194
#> Children 215   2.465   1.723
#> 


# Weighting: observed proportions
op <- proportions(with(DF, table(Gender, Area)))

# Theoretical proportions: 53% Rural, and 50.2% Females
tp <- rep(c(0.53, 0.47), each = 2) * rep(c(0.498, 0.502), 2)

# Corrections by strata
fweights <- tp / op

DF$fweight <- fweights[match(10 * DF$Area + DF$Gender, c(11, 12, 21, 22))]

with(DF, wtable(Gender, wt = fweight))
#> 
#>           fre    rel   per   cpd
#>           ----------------------
#> 1 Males   107  0.498  49.8  49.8 
#> 2 Females 108  0.502  50.2 100.0 
#>           ----------------------
#>           215  1.000 100.0
#> 

with(DF, wmean(Age, wt = fweight))
#> [1] 55.12584

with(DF, wquantile(Age, wt = fweight))
#> 
#>   0%   25%  50%  75% 100%
#>   18   38   55   71   90 
#> 
```
