\name{getDataObject_GeneSymbol_Mutations}
\alias{getDataObject_GeneSymbol_Mutations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getDataObject_GeneSymbol_Mutations}
\description{
Returns an R object with the data for the requested vector, and
annotations for samples, genome, and diseases.
}
\usage{
getDataObject_GeneSymbol_Mutations(theGeneEqList,
theZipFile="/geneSurveyData/GeneSurvey.zip",
theRemoveDupFlag = TRUE, theVerboseFlag = FALSE, theRDataFile = NULL,
theListOnlyFlag = FALSE, theUseDeltaFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGeneEqList}{ A vector of gene symbol names as found in the
  getNames_ function for this data type. }
  \item{theZipFile}{ Default: For DQS Servers. If you are on a server
  where the GENE_REPORT directory is on a different path, you can pass in
  a value to override the default of
  "/geneSurveyData/GeneSurvey.zip". }
  \item{theRemoveDupFlag}{ Default: FALSE. Remove the duplicate barcodes
  from the same control samples used in different diseases. }
  \item{theVerboseFlag}{ Default: FALSE. TRUE means write all output,
  which can be very verbose. }
  \item{theRDataFile}{ Default: NULL. If non null, uses this value as a
  full path, including filename, to write a file with the given object. }
  \item{theListOnlyFlag}{ Default: FALSE. If TRUE, save the RData file as
  a list with elements matching the object slots, so it can be shared with
  people who do not have the package. }
  \item{theUseDeltaFlag}{ Default: FALSE. A flag indicating to use the
  differences in value between tumor and normal samples. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The return value is an object. The following functions work to
pull data out of the object.
  \item{get.Data}{ \link{get.Data} }
  \item{get.GenomeAnnotations}{ \link{get.GenomeAnnotations} }
  \item{get.SampleAnnotations}{ \link{get.SampleAnnotations} }
  \item{get.TissueAnnotations}{ \link{get.TissueAnnotations} }
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
