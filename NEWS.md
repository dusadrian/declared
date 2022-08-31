# Version 0.18
    * Improved method for `rbind()` data frames, taking care of declared variables
    * Function value_labels() has been replaced with the generic base function
      `labels()`, and the function `variable_label()` has been replaced with the
      function `label()`
    * Changed default for `as.factor()`, with values and (where available) levels
      (thanks to Daniel Antal for the report)
    * New generic function `drop_na()` to drop information about the declared
      missing values

# Version 0.17
    * Correctly determine the object mode, function of both value labels and
      input values (especially when all input values are empty NA values)
    * New argument "`measurement`" in function `declared()`
    * New function `measurement()`, to query and allocate measurement levels

# Version 0.16
    * New logical argument "`values`" in the S3 method for `as.character()`
    * Removed aliases `as_declared()` and `as_haven()` from the namespace
    * Fixed `as.haven()` which should return double objects

# Version 0.15
    * Class method for many / most primitive functions, with declared objects
    * Bug fix in `w_table()` affecting zero frequency categories
    * Dropped alias `frtable()` from function `w_table()`
    * New function `is.empty()`, testing the presence of undeclared missing values
    * Main function `declare()` is now generic, allowing flexibility to create
      custom methods for any other classes of objects
    * Removed dependency on package admisc, by incorporating the necessary
      functionality as internal functions

# Version 0.13
    * New functions `w_mode()` and `w_standardize()`
    * Fixed `w_summary()`, summaries should not be rounded by default
    * Fixed `w_mean()` to weight negative values
    Thanks to an anonymous reviewer for reporting and suggesting the following:
    * New argument "`drop`" in function `undeclare()`
    * Bug fix in the class methods for `head()` and `tail()`
    * Function declared now duly recognizes input factors
    * Extended `as.factor()` for objects of class "`declared`"
    * Added proper class methods for primitive arithmetic operations
    * New functions `as.declared()`, `is.declared()`
    * Added explanatory documentation (for instance in the DESCRIPTION file) to
      explain the reasons for reimplementing some of the base functions

# Version 0.12
    * New function `w_summary()`
    * Functions `w_table()` and `w_quantile()` now properly deal with missing weights
    * New argument "`trim`" in function `w_mean()`

# Version 0.11
    * Fixed `w_table()`, an internal object was inadvertently returned
    * Improved printing of crosstables

# Version 0.10
    * New functions `w_mean()`, `w_var()` and `w_sd()`, `w_quantile()` and
      `w_median()`, to  obtain weighted versions of mean, variance and standard
      deviation, quantiles and median, respectively, adapted to declared objects
    * Fixed a bug in `as.haven.declared()`, when all values are (declared)
      missing and labels are not numeric
    * Argument "`weight`" changed to "`wt`" in `w_table()`
    * New arguments in function `w_table()`: "`observed`" to only print observed
      values, and "`margin`" to calculate proportions in crosstabs
    * Function `w_table()` now accepts a second argument "`y`" to create crosstabs
    * Function `frtable()` renamed to `w_table()` standing for weighted table

# Version 0.9
    * New argument "`weight`" in function `frtable()`, allowing to weight not
      only valid but also missing values
    * More robust function `frtable()` in treating declared values with
      overlapping labels (e.g. hierarchical ISCO codes and categories)
    * Fixed a bug in `declared()` when all values are completely missing

# Version 0.8
    * Improved treatment of equality and non-equality, now accepting labels as
      well as values
    * Reverted functionality to not dropping the attributes in arithmetic
      operations, this is now left to the users' preference

# Version 0.7
    * Bug fix saving declared data frames as .csv files
    * `as.declared()` now retains the "format.spss" attribute, if existing

# Version 0.6
    * Improved treatment of arithmetic operations, attributes are no longer
      propagated (thanks to Randall Pruim for the suggestion)
    * Modified as.haven() for objects of class "`declared`", to avoid a bug in
      package ReadStat that prevents exporting missing values to SPSS for
      integer variables

# Version 0.5
    * Initial version
