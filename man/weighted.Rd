\name{Weighted summaries}
\alias{w_table}
\alias{w_mean}
\alias{w_median}
\alias{w_var}
\alias{w_sd}
\alias{w_quantile}
\alias{frtable}

\title{Compute weighted summaries for declared objects}

\description{
Functions to compute weighted tables or summaries, based on a vector of frequency
weights, adapted to objects of class \code{"declared"}.
}

\usage{
w_table(x, y = NULL, wt = NULL, values = FALSE, valid = TRUE,
        observed = FALSE, margin = NULL)

w_mean(x, wt = NULL, na.rm = TRUE)

w_median(x, wt, na.rm = TRUE, ...)

w_var(x, wt = NULL, method = NULL, na.rm = TRUE)

w_sd(x, wt = NULL, method = NULL, na.rm = TRUE)

w_quantile(x, wt = NULL, probs = seq(0, 1, 0.25), na.rm = TRUE, ...)
}


\arguments{
  \item{x}{A numeric vector for summaries, or declared / factor for frequency
      tables}
  \item{y}{An optional variable, to create crosstabs; must have the same length as x}
  \item{wt}{A numeric vector of frequency weights}
  \item{values}{Logical, print the values in the table rows}
  \item{valid}{Logical, print the percent distribution for non-missing values,
      if any missing values are present}
  \item{observed}{Logical, print the observed categories only}
  \item{method}{Character, specifying how the result is scaled, see 'Details' below.}
  \item{probs}{Numeric vector of probabilities with values in [0,1].}
  \item{na.rm}{Logical, should (undeclared) missing values be removed?}
  \item{margin}{Numeric, indicating the margin to calculate crosstab proportions:
      0 from the total, 1 from row totals and 2 from column totals}
  \item{...}{Further arguments passed to or from other methods.}
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

The function \code{w_quantile()} extensively borrowed ideas from packages
\bold{\code{stats}} and \bold{\code{Hmisc}}, to ensure a constant interpolation
that would produce the same quantiles if no weights are provided or if all
weights are equal to 1.

Other arguments can be passed to the stats function \code{quantile()} via the
three dots \code{...} argument, and their extensive explanation is found in the
corresponding stats function's help page.

For all functions, the argument \code{na.rm} refers to the undeclared missing
values and its default is set to TRUE. The declared missing values are
automatically eliminated from the summary statistics, even if this argument is
deactivated.
}

\author{Adrian Dusa}


\examples{
set.seed(215)

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


# Weighting: observed proportions
op <- prop.table(using(df, table(Gender, Area)))

# Theoretical proportions: 53% Rural, and 50% Females
tp <- rep(c(0.53, 0.47), each = 2) * rep(c(0.498, 0.502), 2) / op

df$fweight <- recode(
    10 * df$Area + df$Gender,
    sprintf(
        "11 = \%s; 12 = \%s; 21 = \%s; 22 = \%s",
        tp[1], tp[2], tp[3], tp[4]
    )
)

using(df, w_table(Gender, wt = fweight), split.by = Area)

using(df, w_mean(Age, wt = fweight), split.by = Gender & Area)

using(df, w_quantile(Age, wt = fweight), split.by = Area)
}

\keyword{functions}
