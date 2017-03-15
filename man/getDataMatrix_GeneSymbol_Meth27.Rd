\name{getDataMatrix_GeneSymbol_Meth27}
\alias{getDataMatrix_GeneSymbol_Meth27}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getDataMatrix_GeneSymbol_Meth27
}
\description{
Retrieves a matrix of values for all probes mapped to by the given gene
symbol, with values for all samples/diseases.
}
\usage{
getDataMatrix_GeneSymbol_Meth27(theGene,
theZipFile="/geneSurveyData/GeneSurvey.zip",
theRemoveDupFlag = TRUE, theVerboseFlag = FALSE, theUseDeltaFlag=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGene}{ A vector of gene symbol names as found in the
  getNames_ function for this data type. }
  \item{theZipFile}{ Default: For DQS Servers. If you are on a server
  where the GENE_REPORT directory is on a different path, you can pass in a
  value to override the default of
  "/geneSurveyData/GeneSurvey.zip". }
  \item{theRemoveDupFlag}{ Default: FALSE. Remove the duplicate barcodes
  from the same control samples used in different diseases. }
  \item{theVerboseFlag}{ Default: FALSE. TRUE means write all output,
  which can be very verbose. }
  \item{theUseDeltaFlag}{ Default: FALSE. A flag indicating to use the
  differences in value between tumor and normal samples. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a matrix of values, with colnames() for the vector being the sample
ids (barcodes) and rownames() being the probes. Samples and row ids are in
sorted order, not requested order.
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
