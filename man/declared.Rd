\name{declared}
\alias{declared}
\alias{is.declared}
\alias{as.declared}
\alias{as.haven}
\alias{undeclare}

\title{Labelled vectors with declared missing values}

\description{
The labelled vectors are mainly used to analyse social science data,
and the missing values declaration is an important step in the analysis.
}

\usage{
declared(
    x,
    labels = NULL,
    na_values = NULL,
    na_range = NULL,
    label = NULL,
    measurement = NULL,
    ...
)

is.declared(x)

as.declared(x, ...)

as.haven(x, ...)

undeclare(x, drop = FALSE, ...)
}

\arguments{
\item{x}{A numeric vector to label, or a declared labelled vector (for \code{undeclare})}

\item{labels}{A named vector or \code{NULL}. The vector should be the same type
as \code{x}. Unlike factors, labels don't need to be exhaustive: only a fraction
of the values might be labelled}

\item{na_values}{A vector of values that should also be considered as missing.}

\item{na_range}{A numeric vector of length two giving the (inclusive) extents
of the range. Use \code{-Inf} and \code{Inf} if you want the range to be
open ended}

\item{label}{A short, human-readable description of the vector}

\item{drop}{Logical, drop all attributes}

\item{measurement}{Optional, user specified measurement level}

\item{...}{Other arguments used by various other methods}
}


\details{
The \code{declared} objects are very similar to the \code{haven_labelled_spss}
objects from package \bold{haven}. It has exactly the same arguments, but it
features a fundamental difference in the treatment of (declared) missing values.

In package \bold{haven}, existing values are treated as if they were missing. By
contrast, in package \bold{declared} the NA values are treated as if they were
existing values.

This difference is fundamental and points to an inconsistency in package
\bold{haven}: while existing values can be identified as missing using the
function \code{is.na()}, they are in fact present in the vector and other
packages (most importantly the base ones) do not know these values should be
treated as missing.

Consequently, the existing values are interpreted as missing only by package
\bold{haven}. Statistical procedures will use those values as if they were
valid values.

Package \bold{declared} approaches the problem in exactly the opposite way:
instead of treating existing values as missing, it treats (certain) NA values as
existing. It does that by storing an attribute containing the indices of those
NA values which are to be treated as declared missing values, and it refreshes
this attribute each time the declared object is changed.

This is a trade off and has important implications when subsetting datasets: all
declared variables get this attribute refreshed, which consumes some time
depending on the number of variables in the data.

The generic function \code{as.declared()} attempts to coerce only the compatible
types of objects, namely \code{haven_labelled} and \code{factor}s. Dedicated
class methods can be written for any other type of object, and users are free to
write their own. To end of with a declared object, additional metadata is needed
such as value labels, which values should be treated as missing etc.

The function \code{undeclare()} replaces the NA entries into their original
numeric values, and drops all attributes related to missing values:
\code{na_values}, \code{na_range} and \code{na_index}. The result can be a
regular vector (thus dropping all attributes, including the class "declared")
by activating the argument \code{drop}.

The measurement level is optional and, for the moment, purely aesthetic. It might
however be useful to (automatically) determine if a declared object is suitable
for a certain statistical analysis, for instance regression requires quantitative
variables, while some declared objects are certainly categorical despite using
numbers to denote categories.

It distinguishes between \code{"categorial"} and \code{"quantitative"} types of
variables, and additionally recognizes \code{"nominal"} and \code{"ordinal"} as
categorical, and similarly recognizes \code{"interval"}, \code{"ratio"},
\code{"discrete"} and \code{"continuous"} as quantitative.
}

\value{
\code{declared()}, \code{as.declared()} and \code{is.declared()} will return a
labelled vector.

\code{is.declared()} and \code{is.declared()} will return a logical scalar.

\code{undeclare()} will return a an object of class \code{declared} without the
declared missing values

\code{as.haven()} returns an object of class \code{haven_labelled_spss}
}

\examples{

x <- declared(
    c(1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_values = -1
)

x

is.na(x)

x > 0

x == -1

# Values are actually placeholder for categories, so labels work as if they were factors:
x == "DK"


# when newly added values are already declared as missing, they are automatically coerced
c(x, 2, -1)

# switch NAs with their original values
undeclare(x)

as.character(x)

# Returning values instead of categories
as.character(x, values = TRUE)
}
