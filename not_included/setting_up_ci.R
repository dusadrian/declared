library(devtools)
library(usethis)

# I added unit tests, see the tests/testthat directory.

## The simplest service to set up is appveyor.
## https://www.appveyor.com/
## You can also sign up manually with a Github login.

## The code below I think it will take you there and ask you to
## connect with your Github account.

usethis::use_appveyor()
✔ Setting active project to 'C:/_packages/DDIwR/declared'
Error in `stop_bad_github_remote_config()`:
  ! Unsupported GitHub remote configuration: 'fork_upstream_is_not_origin_parent'
## I cannot do it from a non-parent github fork
## You will get a warning
## Warning message:
##  `use_appveyor()` was deprecated in usethis 2.0.0.
## Please use `use_github_actions()` instead.
## But it is more complicated to do it parametrically, so just use the use_appveyor, it will do the trick.

## Place the badge in the README.Rmd, then Knit to README.md
usethis::use_appveyor_badge()

## Now you can set up the coverage
usethis::use_coverage()

## You should exclude functions that are verbatim coming from haven or else
usethis::use_covr_ignore()
