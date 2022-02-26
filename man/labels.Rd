\name{Labels}
\alias{value_labels}
\alias{value_labels<-}
\alias{variable_label}
\alias{variable_label<-}

\title{Get / Declare missing values}

\description{
Functions to extract information about the declared variable / value labels, or
to declare such values if they are present in the data.
}

\usage{
value_labels(x, prefixed = FALSE)

value_labels(x) <- value

variable_label(x)

variable_label(x) <- value
}


\arguments{
\item{x}{A declared vector.}

\item{prefixed}{Logical, prefix labels with values.}

\item{value}{Any vector of values that should be declared as missing
(for \code{value_labels}) or a numeric vector of length two giving the (inclusive)
extents of the range of missing values (for \code{variable_label}).}
}

\value{
\code{value_labels()} will return a named vector.

\code{variable_label()} will return a single character string.
}


\examples{
x <- declared(c(-2, 1:5, -1),
            labels = c(Good = 1, Bad = 5, DK = -1),
            na_values = c(-1, -2),
            label = "Test variable")
x

value_labels(x)

value_labels(x) <- c(Good = 1, Bad = 5, DK = -1, NotApplicable = -2)

variable_label(x)

variable_label(x) <- "This is a proper label"

x
}
