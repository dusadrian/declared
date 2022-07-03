\name{Labels}
\alias{labels}
\alias{labels<-}
\alias{value_labels}
\alias{label}
\alias{label<-}
\alias{variable_label}

\title{Get / Declare value labels}

\description{
Functions to extract information about the declared variable / value labels, or
to declare such values if they are present in the data.
}

\usage{
labels(x) <- value

label(x)

label(x) <- value
}


\arguments{
\item{x}{A declared vector.}

\item{value}{Any vector of values that should be declared as missing
(for \code{labels}) or a numeric vector of length two giving the (inclusive)
extents of the range of missing values (for \code{label}).}
}

\details{
The function \code{labels()} is a adaptation of the base function to the objects
of class \code{declared}. In addition to the regular arguments, it has an
additional logical one called \code{prefixed}, to retrieve the value labels
prefixed with their values.
}

\value{
\code{labels()} will return a named vector.

\code{label()} will return a single character string.
}


\examples{
x <- declared(
    c(-2, 1:5, -1),
    labels = c(Good = 1, Bad = 5, DK = -1),
    na_values = c(-1, -2),
    label = "Test variable"
)
x

labels(x)

labels(x, prefixed = TRUE)

labels(x) <- c(Good = 1, Bad = 5, DK = -1, NotApplicable = -2)

label(x)

label(x) <- "This is a proper label"

x
}
