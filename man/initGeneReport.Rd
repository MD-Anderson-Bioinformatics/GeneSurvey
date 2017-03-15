\name{initGeneReport}
\alias{initGeneReport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
initGeneReport
}
\description{
This function sets up rJava.
}
\usage{
initGeneReport(theParameters="-Xmx2400m")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theParameters}{ Parameters to Java. This allows you to adjust the arguments to the rJava Java VM. As noted when called, on our servers, using measurements other than m or larger than 42000 would fail. The HPC would take larger values of m. }
}
\details{
Must be called before using other functions in package.
}
\value{
Nothing useful.
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
