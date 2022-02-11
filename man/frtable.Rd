\name{frtable}
\alias{frtable}

\title{Create a frequency table}

\description{
Presents the usual frequency distributions from a frequency table, separating
valid from missing values.
}

\usage{
frtable(x, values = TRUE, valid = TRUE, weight = NULL)
}

\arguments{
  \item{x}{A categorical variable, either a factor or a \code{declared} variable}
  \item{values}{Logical, print the values in the table rows}
  \item{valid}{Logical, print the valid percentage distribution}
  \item{weight}{Frequency weight variable}
}

\details{
A frequency table is usually performed for a categorical variable, displaying the
frequencies of the respective categories. Note that general variables containing
text are not necessarily factors, despite having a small number of characters.

It is sometimes possible to perform a frequency table for numerical variables,
if the number of values is very limited (an arbitrary and debatable upper limit
of 15 is used). An example of such variable can be the number of children, where
each value can be interpreted as a class, containing a single value (for
instance 0 meaning the category of people with no children).

Objects of class  are not pure categorical variables (R factors)
but they are nevertheless intepreted as similar to factors, and allow producing
frequency tables for. Given the high similarity with package \bold{\code{haven}},
objects of class \code{haven_labelled_spss} are automatically coerced to objects
of class \code{declared} and treated accordingly.

The argument \code{values} makes sense only when the input is of class
\code{declared}, otherwise for regular (base R) factors the values are
just a sequence of numbers.
}

\author{Adrian Dusa}


\examples{

# a pure categorical variable
x <- factor(sample(letters[1:5], 215, replace = TRUE))
frtable(x)


# simulate number of children
x <- sample(0:4, 215, replace = TRUE)
frtable(x)

# simulate a Likert type response scale from 1 to 7
values <- sample(c(1:7, -99), 215, replace = TRUE)
x <- declared(values, labels = c("Good" = 1, "Bad" = 7))
frtable(x)


# Defining missing values
missing_values(x) <- -99
frtable(x)


# Defined missing values with labels
values <- sample(c(1:7, -99, NA), 215, replace = TRUE)
x <- declared(
    values,
    labels = c("Good" = 1, "Bad" = 7, "Don't know" = -99),
    na_values = -99
)

frtable(x)


# Categories only (without values)
frtable(x, values = FALSE)


}

\keyword{functions}
