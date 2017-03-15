\name{get.Data}
\alias{get.Data}
\alias{get.Data-method}
\alias{get.Data,ANY-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get.Data}
\description{
returns a matrix with colnames() being samples and rownames() being probes
}
\usage{
get.Data(theObject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{An object returned from one of the getDataObject_ functions.}
}
\details{
See the return object description in \link{getDataObject_Probe_Meth27} or  \link{getDataObject_Probe_Meth450}
}
\value{
returns a matrix with colnames() being samples and rownames() being probes or gene symbols, etc.  Samples and row ids are in sorted order, not requested order.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{#See examples in tests directory in the install.}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
