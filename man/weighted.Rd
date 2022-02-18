\name{Weighted summaries and objects}
\alias{w_table}
\alias{w_var}
\alias{w_sd}
\alias{frtable}

\title{Compute weighted summaries for declared objects}

\description{
Functions to compute weighted summaries, when a vector of frequency weights is
provided, inspired by similar functions in packages \bold{\code{stats}} and
and \bold{\code{Hmisc}}, adapted to objects of class \code{"declared"}.
}

\usage{
w_table(x, wt = NULL, values = TRUE, valid = TRUE, observed = FALSE)

w_var(x, wt = NULL, method = NULL, na.rm = TRUE)

w_sd(x, wt = NULL, method = NULL, na.rm = TRUE)
}


\arguments{
  \item{x}{A numeric vector}
  \item{wt}{A numeric vector of frequency weights}
  \item{values}{Logical, print the values in the table rows}
  \item{valid}{Logical, print the percent distribution for non-missing values}
  \item{observed}{Logical, print the observed categories only}
  \item{method}{Character, specifying how the result is scaled, see 'Details' below.}
  \item{na.rm}{Logical, should (undeclared) missing values be removed?}
}

\value{
A weighted variance or standard deviation.
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

Objects of class \code{declared} are not pure categorical variables (R factors)
but they are nevertheless interpreted as similar to factors, to allow producing
frequency tables. Given the high similarity with package \bold{\code{haven}},
objects of class \code{haven_labelled_spss} are automatically coerced to objects
of class \code{declared} and treated accordingly.

The argument \code{values} makes sense only when the input is of family class
\code{declared}, otherwise for regular (base R) factors the values are
just a sequence of numbers.

The later introduced argument \code{observed} is useful in situations when a
variable has a very large number of potential values, and a smaller subset of
actually observed ones. As an example, the variable \dQuote{Occupation} has
hundreds of possible values in the ISCO08 codelist, and not all of them might be
actually observed. When activated, this argument restricts the printed frequency
table to the subset of observed values only.

The argument \code{method} can be one of \code{"unbiased"} or \code{"ML"}.

When this is set to \code{"unbiased"}, the result is an unbiased estimate using
Bessel's correction. When this is set to \code{"ML"}, the result is the
maximum likelihood estimate for a Gaussian distribution.

The argument \code{wt} refers only to frequency weights. Users should be
aware of the differences between frequency weights, analytic weights, probability
weights, design weights, post-stratification weights etc. For purposes of
inferential testing, Thomas Lumley's package \bold{\code{survey}} should be
employed.

If no frequency weights are provided, the result is identical to the
corresponding base functions.
}

\author{Adrian Dusa}


\examples{
set.seed(12345)

# a pure categorical variable
x <- factor(sample(letters[1:5], 215, replace = TRUE))
w_table(x)


# simulate number of children
x <- sample(0:4, 215, replace = TRUE)
w_table(x)

# simulate a Likert type response scale from 1 to 7
values <- sample(c(1:7, -91), 215, replace = TRUE)
x <- declared(values, labels = c("Good" = 1, "Bad" = 7))
w_table(x)


# Defining missing values
missing_values(x) <- -91
w_table(x)


# Defined missing values with labels
values <- sample(c(1:7, -91, NA), 215, replace = TRUE)
x <- declared(
    values,
    labels = c("Good" = 1, "Bad" = 7, "Don't know" = -91),
    na_values = -91
)

w_table(x)


# Categories only (without values)
w_table(x, values = FALSE)


# An example involving multiple variables
df <- data.frame(
    Area = declared(
        sample(1:2, 215, replace = TRUE, prob = c(0.45, 0.55)),
        labels = c(Rural = 1, Urban = 2)
    ),
    Gender = declared(
        sample(1:2, 215, replace = TRUE, prob = c(0.55, 0.45)),
        labels = c(Males = 1, Females = 2)
    ),
    Age = sample(18:90, 215, replace = TRUE),
    Children = sample(0:5, 215, replace = TRUE)
)

using(df, w_table(Gender), split.by = Area)

using(df, w_sd(Age), split.by = Gender & Area)


# Weighting: observed frequencies
obs <- with(df, table(Gender, Area))

# Theoretical proportions: 53% Rural, and 50% Males
tp <- rep(c(0.53, 0.47), each = 2) * rep(0.5, 4) / prop.table(obs)

df$fweight <- recode(
    10 * df$Area + df$Gender,
    sprintf(
        "11 = %s; 12 = %s; 21 = %s; 22 = %s",
        tp[1], tp[2], tp[3], tp[4]
    )
)


using(df, w_table(Gender, fweight), split.by = Area)

using(df, w_sd(Age, fweight), split.by = Gender & Area)
}

\keyword{functions}
