% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_serial.R
\name{pbnftest}
\alias{pbnftest}
\alias{pbnftest.panelmodel}
\alias{pbnftest.formula}
\title{Modified BNF--Durbin--Watson Test and Baltagi--Wu's LBI Test for Panel
Models}
\usage{
pbnftest(x, ...)

\method{pbnftest}{panelmodel}(x, test = c("bnf", "lbi"), ...)

\method{pbnftest}{formula}(
  x,
  data,
  test = c("bnf", "lbi"),
  model = c("pooling", "within", "random"),
  ...
)
}
\arguments{
\item{x}{an object of class \code{"panelmodel"} or of class \code{"formula"},}

\item{\dots}{only relevant for formula interface: further arguments
to specify the model to test (arguments passed on to plm()),
e.g., \code{effect}.}

\item{test}{a character indicating the test to be performed, either
\code{"bnf"} or \code{"lbi"} for the (modified) BNF statistic or
Baltagi--Wu's LBI statistic, respectively,}

\item{data}{a \code{data.frame} (only relevant for formula interface),}

\item{model}{a character indicating on which type of model the test
shall be performed (\code{"pooling"}, \code{"within"}, \code{"random"}, only
relevant for formula interface),}
}
\value{
An object of class \code{"htest"}.
}
\description{
Tests for AR(1) disturbances in panel models.
}
\details{
The default, \code{test = "bnf"}, gives the (modified) BNF statistic,
the generalised Durbin-Watson statistic for panels. For balanced
and consecutive panels, the reference is
Bhargava/Franzini/Narendranathan (1982). The modified BNF is given
for unbalanced and/or non-consecutive panels (d1 in formula 16 of
\insertCite{BALT:WU:99;textual}{plm}).

\code{test = "lbi"} yields Baltagi--Wu's LBI statistic
\insertCite{BALT:WU:99}{plm}, the locally best invariant test which
is based on the modified BNF statistic.

No specific variants of these tests are available for random effect models.
As the within estimator is consistent also under the random effects
assumptions, the test for random effect models is performed by taking the
within residuals.

No p-values are given for the statistics as their distribution is
quite difficult. \insertCite{BHAR:FRAN:NARE:82;textual}{plm} supply
tabulated bounds for p = 0.05 for the balanced case and consecutive
case.

For large N, \insertCite{BHAR:FRAN:NARE:82}{plm} suggest it is
sufficient to check whether the BNF statistic is < 2 to test
against positive serial correlation.
}
\examples{

data("Grunfeld", package = "plm")

# formula interface, replicate Baltagi/Wu (1999), table 1, test case A:
data_A <- Grunfeld[!Grunfeld[["year"]] \%in\% c("1943", "1944"), ]
pbnftest(inv ~ value + capital, data = data_A, model = "within")
pbnftest(inv ~ value + capital, data = data_A, test = "lbi", model = "within")

# replicate Baltagi (2013), p. 101, table 5.1:
re <- plm(inv ~ value + capital, data = Grunfeld, model = "random")
pbnftest(re)
pbnftest(re, test = "lbi")

}
\references{
\insertRef{BALT:13}{plm}

\insertRef{BALT:WU:99}{plm}

\insertRef{BHAR:FRAN:NARE:82}{plm}
}
\seealso{
\code{\link[=pdwtest]{pdwtest()}} for the original Durbin--Watson test using
(quasi-)demeaned residuals of the panel model without taking
the panel structure into account. \code{\link[=pbltest]{pbltest()}}, \code{\link[=pbsytest]{pbsytest()}},
\code{\link[=pwartest]{pwartest()}} and \code{\link[=pwfdtest]{pwfdtest()}} for other serial correlation
tests for panel models.
}
\author{
Kevin Tappe
}
\keyword{htest}
