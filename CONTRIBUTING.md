

# Contributing Guidelines

This repository is structured as a standard R package
following the conventions outlined in the [Writing R
extensions](http://cran.r-project.org/doc/manuals/R-exts.html) manual.

We encourage contributions from the community.


## How you can Contribute

### Report an issue

If you find an issue with the package functionality, first search through the
[existing issues](https://github.com/dusadrian/declared/issues) to find if the
issue is already reported. If your issue is not there, submit a new one by
providing all the details you can provide, along with a reproducible example.


### Documenting existing or new functions

All code for this package is found in `R/`, (except compiled source
code, if used, which is in `/src`).  All functions should be thoroughly
documented with [roxygen2](https://github.com/r-lib/roxygen2) notation in the
`.R` files, after which the function documentation is generated automatically.

Please do not edit any of the documentation files in `man/` or the `NAMESPACE`. 
Instead, construct the appropriate `roxygen2` documentation in the
function files in `R/` themselves.  The documentation is then generated
by running the `document()` function from the `devtools` package. Do please
consult the [Advanced R programming](http://adv-r.had.co.nz/) guide if
this workflow is unfamiliar to you.  Note that functions should include
examples in the documentation. Examples that should demontrate an error, or take
more than a few seconds to execute, or require an internet connection, should be
written within the `\dontrun` statement.


### Testing

Any new feature or bug-fix should include a unit-test demonstrating the
change.  Unit tests follow the `testthat` framework with files in
`tests/testthat`.  Please make sure that the testing suite passes
before issuing a pull request.  This can be done by running `check()`
from the `devtools` package, which will also check for consistent
documentation, etc.


This package uses the [AppVeyor](https://github.com/krlmlr/r-appveyor)
continuous testing mechanism for R to ensure that the test suite is run
on each push to Github.  An icon at the top of the README.md indicates
whether or not the tests are currently passing. 

This package also uses
[codecov.io](https://codecov.io/) to
measure test coverage.  While not all code can be covered by automated 
tests (in particular, functions involving user prompts), try to avoid
decreasing coverage by writing unit tests for any contributed code. 
Codecov.io will flag PRs that decrease coverage. 


**Thank you for your contributions!**
