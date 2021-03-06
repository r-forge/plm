\name{fixef.plm}
\alias{fixef.plm}
\alias{fixef}
\alias{print.fixef}
\alias{summary.fixef}
\alias{print.summary.fixef}
\title{Extract the Fixed Effects}

\description{
  Function to extract the fixed effects from a \code{plm} object and associated summary method.
}

\usage{
\method{fixef}{plm}(object, effect = NULL,
                      type = c("level", "dfirst", "dmean"), .vcov = NULL, ...)
\method{print}{fixef}(x, digits = max(3, getOption("digits") - 2),
width = getOption("width"),...)
\method{summary}{fixef}(object, ...)
\method{print}{summary.fixef}(x, digits = max(3, getOption("digits") -
2), width = getOption("width"),...)
}

\arguments{
  \item{x,object}{an object of class \code{"plm"}, an object
    of class \code{"fixef"} for the \code{print} and the \code{summary} method,}
  \item{effect}{one of \code{"individual"} or \code{"time"}, only
    relevant in case of two--ways effects models,}
  \item{.vcov}{a variance--covariance matrix furnished by the user or a function 
    to calculate one (see \bold{Examples}),}
  \item{type}{one of \code{"level"}, \code{"dfirst"}, or \code{"dmean"},}
  \item{digits}{digits,}
  \item{width}{the maximum length of the lines in the print output,}
  \item{\dots}{further arguments.}
}

\value{
  For function \code{fixef} an object of class \code{"fixef"} is returned:\cr
  It is a numeric vector containing the fixed effects with attribute \code{se} 
  which contains the standard errors. There are two further attributes to the 
  numeric vector:
  attribute \code{type} contains the chosen type (the value of argument \code{type} 
  as a character); attribute \code{df.residual} holds the residual degrees of 
  freedom (integer) from the fixed effects model (plm object) on which \code{fixef} was run.
  
  For function \code{summary.fixef} an object of class \code{"summary.fixef"} is returned:\cr
  It is (internally) a matrix with four columns in this order: the estimated fixed 
  effects, their standard errors and associated t--values and p--values.
  The type of the fixed effects and the standard errors in the summary.fixef objects 
  corresponds to was requested in the \code{fixef} function by arguments \code{type} 
  and \code{.vcov}.
}

\details{
Function \code{fixef} calculates the fixed effects and returns an object of class \code{"fixef"}.
With the \code{type} argument, the fixed effects may be returned in levels (\code{"level"}), 
as deviations from the first value of the index (\code{"dfirst"}), or as deviations from the 
overall mean (\code{"dmean"}). If the argument \code{.vcov} was specified, the standard errors
(stored as attribute "se" in the return value) are the respective robust standard errors. 

The associated \code{summary} method returns an extended object of class \code{"summary.fixef"} 
with more information (see sections \bold{Value} and \bold{Examples}).
}

\seealso{
  \code{\link{within_intercept}} for the overall intercept of fixed effect models along its 
  standard error, \code{\link{plm}} for plm objects and within models (= fixed effects models) 
  in general.
}

\author{Yves Croissant}

\examples{
data("Grunfeld", package = "plm")
gi <- plm(inv ~ value + capital, data = Grunfeld, model = "within")
fixef(gi)
summary(fixef(gi))
summary(fixef(gi))[ , c("Estimate", "Pr(>|t|)")] # only estimates and p-values

# relationship of type = "dmean" and "level" and overall intercept
fx_level <- fixef(gi, type = "level")
fx_dmean <- fixef(gi, type = "dmean")
overallint <- within_intercept(gi)
all.equal(overallint + fx_dmean, fx_level, check.attributes = FALSE) # TRUE

# extract time effects in a twoways effects model
gi_tw <- plm(inv ~ value + capital, data = Grunfeld,
          model = "within", effect = "twoways")
fixef(gi_tw, effect = "time")

# with supplied variance-covariance matrix as matrix, function,
# and function with additional arguments
fx_level_robust1 <- fixef(gi, .vcov = vcovHC(gi))
fx_level_robust2 <- fixef(gi, .vcov = vcovHC)
fx_level_robust3 <- fixef(gi, .vcov = function(x) vcovHC(x, method = "white2"))
summary(fx_level_robust1) # gives fixed effects, robust SEs, t- and p-values

# calc. fitted values of oneway within model:
fixefs <- fixef(gi)[index(gi, which = "id")]
fitted_by_hand <- fixefs + gi$coefficients["value"] * gi$model$value +
                           gi$coefficients["capital"] * gi$model$capital
}

\keyword{regression}