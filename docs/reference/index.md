# Package index

## The declared S3 class

Construct and coerce to declared.

- [`as.declared()`](declared.md) [`declared()`](declared.md)
  [`is.declared()`](declared.md) [`anyNAdeclared()`](declared.md) :
  Labelled vectors with declared missing values
- [`declared_package`](declared_package.md) : Functions for Declared
  Missing Values

## Coercion to other classes

Convert to other classes, remove declared attributes.

- [`undeclare()`](drop_undeclare.md) [`drop_na()`](drop_undeclare.md) :
  Drop information / undeclare labelled objects
- [`as.haven()`](as.haven.md) : Coerce to haven / labelled objects

## Working with labels

Working with value labels and measurement labels.

- [`label()`](labels.md) [`` `label<-`() ``](labels.md)
  [`` `labels<-`() ``](labels.md) : Get / Declare value labels
- [`measurement()`](measurement.md)
  [`` `measurement<-`() ``](measurement.md) : Get / Set measurement
  levels for declared objects

## Working with missing values

Declaring missing values.

- [`missing_range()`](missing_values.md)
  [`` `missing_range<-`() ``](missing_values.md)
  [`missing_values()`](missing_values.md)
  [`` `missing_values<-`() ``](missing_values.md) : Get / Declare
  missing values
- [`is.empty()`](is.empty.md) [`anyNAempty()`](is.empty.md) : Test the
  presence of empty (undeclared) missing values

## Arithmetic methods

Correctly calculate (weighted) statistical values and summaries, when
declared missing values are present.

- [`direct_declared()`](declared_internal.md)
  [`format_declared()`](declared_internal.md)
  [`order_declared()`](declared_internal.md)
  [`value_labels()`](declared_internal.md)
  [`variable_label()`](declared_internal.md)
  [`names_values()`](declared_internal.md)
  [`makeTag_()`](declared_internal.md)
  [`hasTag_()`](declared_internal.md)
  [`getTag_()`](declared_internal.md)
  [`anyTagged_()`](declared_internal.md)
  [`w_IQR()`](declared_internal.md)
  [`w_fivenum()`](declared_internal.md)
  [`w_mean()`](declared_internal.md)
  [`w_median()`](declared_internal.md)
  [`w_mode()`](declared_internal.md)
  [`w_quantile()`](declared_internal.md)
  [`w_sd()`](declared_internal.md)
  [`w_standardize()`](declared_internal.md)
  [`w_summary()`](declared_internal.md)
  [`w_table()`](declared_internal.md) [`w_var()`](declared_internal.md)
  : declared internal functions
- [`wIQR()`](weighted.md) [`wfivenum()`](weighted.md)
  [`wmean()`](weighted.md) [`wmeasures()`](weighted.md)
  [`wmedian()`](weighted.md) [`wmode()`](weighted.md)
  [`wquantile()`](weighted.md) [`wsd()`](weighted.md)
  [`wstandardize()`](weighted.md) [`wsummary()`](weighted.md)
  [`wtable()`](weighted.md) [`wvar()`](weighted.md) : Compute weighted
  summaries for declared objects
