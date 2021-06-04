\name{Missing values}
\alias{missing_values}
\alias{missing_values<-}
\alias{missing_range}
\alias{missing_range<-}

\title{Get / Declare missing values}

\description{
Functions to extract information about the declared missing values, or to
declare such values if they are present in the data.
}

\usage{
missing_values(x)

missing_values(x) <- value

missing_range(x)

missing_range(x) <- value
}


\arguments{
\item{x}{A vector.}

\item{value}{Any vector of values that should be declared as missing
(for \code{missing_values}) or a numeric vector of length two giving the (inclusive)
extents of the range of missing values (for \code{missing_range}).}
}

\value{
\code{missing_values()} will return a vector of one or more values.

\code{missing_range()} will return a numeric vector of length 2.
}


\examples{
x <- declared(c(-2, 1:5, -1),
        labels = c(Good = 1, Bad = 5, DK = -1, NotApplicable = -2),
        na_values = c(-1, -2))
x

missing_values(x)

missing_range(x) <- c(-10, -7)

missing_range(x)
}
