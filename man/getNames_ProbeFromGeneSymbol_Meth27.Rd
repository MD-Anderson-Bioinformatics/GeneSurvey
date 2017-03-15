\name{getNames_ProbeFromGeneSymbol_Meth27}
\alias{getNames_ProbeFromGeneSymbol_Meth27}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getNames_ProbeFromGeneSymbol_Meth27
}
\description{
A vector of valid probe identifiers that map to the given Gene Symbol.
}
\usage{
getNames_ProbeFromGeneSymbol_Meth27(theGene,
theZipFile="/geneSurveyData/GeneSurvey.zip",
theVerboseFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGene}{ The gene symbol to use in mapping to probes. }
  \item{theZipFile}{ Default: For DQS Servers. If you are on a server
  where the GENE_REPORT directory is on a different path, you can pass
  in a value to override the default of
  "/geneSurveyData/GeneSurvey.zip".
  NOTE: this is a subdirectory of GENE_REPORT. }
  \item{theVerboseFlag}{ Default: FALSE.
  TRUE means write all output, which can be very verbose. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of valid probe identifiers that map to the given Gene Symbol.
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
