\name{plot_GeneSymbol_SNP6}
\alias{plot_GeneSymbol_SNP6}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot_GeneSymbol_SNP6
}
\description{
Plots stripcharts and replicate diagrams for the given arguments.
}
\usage{
plot_GeneSymbol_SNP6(theGeneEq, theOutputDir,
theZipFile="/geneSurveyData/GeneSurvey.zip",
theVerboseFlag = FALSE, theUseDeltaFlag = FALSE, theReplicateFlag=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGeneEq}{ The gene symbol to plot which corresponds to an id
  from the appropriate getNames_ function. }
  \item{theOutputDir}{ Directory to which to write files.
  Directory will be created if needed/possible. }
  \item{theZipFile}{ Default: For DQS Servers. If you are on a server
  where the GENE_REPORT directory is on a different path, you can pass
  in a value to override the default of
  "/geneSurveyData/GeneSurvey.zip". }
  \item{theVerboseFlag}{ Default: FALSE. TRUE means write all output,
  which can be very verbose. }
  \item{theUseDeltaFlag}{ Default: FALSE. A flag indicating to use the
  differences in value between tumor and normal samples. }
  \item{theReplicateFlag}{ Default: FALSE.
  A flag indicating to write the replicate plots. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of files output.
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
