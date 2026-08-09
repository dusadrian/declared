# declared internal functions

Functions to be used internally, only by developers and contributors.

## Usage

``` r
direct_declared(
  x,
  na_index = NULL,
  na_values = NULL,
  na_range = NULL,
  labels = NULL,
  label = NULL,
  measurement = NULL,
  decimals = NULL,
  date = inherits(x, "Date")
)

format_declared(x, digits = getOption("digits"))

order_declared(
  x,
  na.last = NA,
  decreasing = FALSE,
  method = c("auto", "shell", "radix"),
  empty.last = na.last,
  ...
)

value_labels(...)

variable_label(...)

names_values(x, drop_na = FALSE, observed = TRUE)

makeTag_(...)

hasTag_(x, tag = NULL)

getTag_(x)

anyTagged_(x)

w_IQR(...)

w_fivenum(...)

w_mean(...)

w_median(...)

w_mode(...)

w_quantile(...)

w_sd(...)

w_standardize(...)

w_summary(...)

w_table(...)

w_var(...)
```
