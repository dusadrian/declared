# Package index

## The declared S3 class

Construct and coerce to declared.

- [`as.declared()`](https://dusadrian.github.io/declared/reference/declared.md)
  [`declared()`](https://dusadrian.github.io/declared/reference/declared.md)
  [`is.declared()`](https://dusadrian.github.io/declared/reference/declared.md)
  [`anyNAdeclared()`](https://dusadrian.github.io/declared/reference/declared.md)
  : Labelled vectors with declared missing values
- [`declared_package`](https://dusadrian.github.io/declared/reference/declared_package.md)
  : Functions for Declared Missing Values

## Coercion to other classes

Convert to other classes, remove declared attributes.

- [`undeclare()`](https://dusadrian.github.io/declared/reference/drop_undeclare.md)
  [`drop_na()`](https://dusadrian.github.io/declared/reference/drop_undeclare.md)
  : Drop information / undeclare labelled objects
- [`as.haven()`](https://dusadrian.github.io/declared/reference/as.haven.md)
  : Coerce to haven / labelled objects

## Working with labels

Working with value labels and measurement labels.

- [`label()`](https://dusadrian.github.io/declared/reference/labels.md)
  [`` `label<-`() ``](https://dusadrian.github.io/declared/reference/labels.md)
  [`` `labels<-`() ``](https://dusadrian.github.io/declared/reference/labels.md)
  : Get / Declare value labels
- [`measurement()`](https://dusadrian.github.io/declared/reference/measurement.md)
  [`` `measurement<-`() ``](https://dusadrian.github.io/declared/reference/measurement.md)
  : Get / Set measurement levels for declared objects

## Working with missing values

Declaring missing values.

- [`missing_range()`](https://dusadrian.github.io/declared/reference/missing_values.md)
  [`` `missing_range<-`() ``](https://dusadrian.github.io/declared/reference/missing_values.md)
  [`missing_values()`](https://dusadrian.github.io/declared/reference/missing_values.md)
  [`` `missing_values<-`() ``](https://dusadrian.github.io/declared/reference/missing_values.md)
  : Get / Declare missing values
- [`is.empty()`](https://dusadrian.github.io/declared/reference/is.empty.md)
  [`anyNAempty()`](https://dusadrian.github.io/declared/reference/is.empty.md)
  : Test the presence of empty (undeclared) missing values
- [`valid_na_index()`](https://dusadrian.github.io/declared/reference/valid_na_index.md)
  : Validate the declared missing values index

## Arithmetic methods

Correctly calculate (weighted) statistical values and summaries, when
declared missing values are present.

- [`direct_declared()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`format_declared()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`order_declared()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`value_labels()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`variable_label()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`names_values()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`makeTag_()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`hasTag_()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`getTag_()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`anyTagged_()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_IQR()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_fivenum()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_mean()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_median()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_mode()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_quantile()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_sd()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_standardize()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_summary()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_table()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  [`w_var()`](https://dusadrian.github.io/declared/reference/declared_internal.md)
  : declared internal functions
- [`wIQR()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wfivenum()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wmean()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wmeasures()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wmedian()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wmode()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wquantile()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wsd()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wstandardize()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wsummary()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wtable()`](https://dusadrian.github.io/declared/reference/weighted.md)
  [`wvar()`](https://dusadrian.github.io/declared/reference/weighted.md)
  : Compute weighted summaries for declared objects
