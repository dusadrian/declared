\name{declared-package}

\alias{declared-package}

\docType{package}

\title{
Functions to Declare Missing Values
}

\description{
A set of functions to declare labels and missing values, coupled with associated
functions to create (weighted) tables of frequencies and various other summary
measures.
Various base functions are reimplemented to make use of the specific information
about the missing values. For instance, the function mean() interprets declared
missing values as actual missing values, but these are different from regular
NA values precisely because they are declared. The reimplementation takes care
of this aspect and automatically removes the declared missing values, leaving
the argument "na.rm" intact for pure NAs.
And since most of the base function, for instance median(), do not have specific
functionality for weighted values, corresponding functions have beed added to
this package having the prefix "w_".
There are potentially thousands of other functions that need similar
reimplementations. This package will likely grow in the future, and potential
code developers / contributors are welcome to get inspired from the existing
code base.
Many functions have a similar functionality with the corresponding functions
from packages "haven" and "labelled". A lot of effort was spent to ensure as
much compatibility as possible with these packages, for the intention is not to
replace through a namespace conflict, but to complement them by adding specific
functionality for the objects of class "declared".
}

\details{
    \tabular{ll}{
    Package: \tab declared\cr
    Type: \tab Package\cr
    Version: \tab 0.13\cr
    Date: \tab 2022-04-03\cr
    License: \tab GPL-v3\cr
    }
}
\author{
Adrian Dusa

Maintainer: Adrian Dusa (dusa.adrian@unibuc.ro)
}

\keyword{misc}
