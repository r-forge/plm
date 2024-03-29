% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tool_pdata.frame.R
\name{pseriesfy}
\alias{pseriesfy}
\title{Turn all columns of a pdata.frame into class pseries.}
\usage{
pseriesfy(x, ...)
}
\arguments{
\item{x}{an object of class \code{"pdata.frame"},}

\item{\dots}{further arguments (currently not used).}
}
\value{
A pdata.frame like the input pdata.frame but with all columns
turned into pseries.
}
\description{
This function takes a pdata.frame and turns all of its columns into
objects of class pseries.
}
\details{
Background: Initially created pdata.frames have as columns the pure/basic
class (e.g., numeric, factor, character). When extracting a column from such
a pdata.frame, the extracted column is turned into a pseries.

At times, it can be convenient to apply data transformation operations on
such a \code{pseriesfy}-ed pdata.frame, see Examples.
}
\examples{
library("plm")
data("Grunfeld", package = "plm")
pGrun <- pdata.frame(Grunfeld[ , 1:4], drop.index = TRUE)
pGrun2 <- pseriesfy(pGrun) # pseriesfy-ed pdata.frame

# compare classes of columns
lapply(pGrun,  class)
lapply(pGrun2, class)

# When using with()
with(pGrun,  lag(value)) # dispatches to base R's lag() 
with(pGrun2, lag(value)) # dispatches to plm's lag() respect. panel structure

# When lapply()-ing 
lapply(pGrun,  lag) # dispatches to base R's lag() 
lapply(pGrun2, lag) # dispatches to plm's lag() respect. panel structure

# as.list(., keep.attributes = TRUE) on a non-pseriesfy-ed
# pdata.frame is similar and dispatches to plm's lag
lapply(as.list(pGrun, keep.attributes = TRUE), lag) 

}
\seealso{
\code{\link[=pdata.frame]{pdata.frame()}}, \code{\link[=as.list]{as.list()}}
}
\keyword{attribute}
