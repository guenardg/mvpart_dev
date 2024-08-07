% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rpconvert.R
\name{rpconvert}
\alias{rpconvert}
\title{Update an Rpart Object}
\usage{
rpconvert(x)
}
\arguments{
\item{x}{An \code{rpart} object.}
}
\value{
An updated rpart object
}
\description{
Rpart objects changed (slightly) in their internal format in order to
accomdate the changes for user-written split functions.  This routine updates
an old object to the new format.
}
\seealso{
\code{rpart}
}
\keyword{tree}
