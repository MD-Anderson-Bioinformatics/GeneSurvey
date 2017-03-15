\name{getMetadata_Probe_Meth450}
\alias{getMetadata_Probe_Meth450}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getMetadata_Probe_Meth450
}
\description{
Get the metadata about the given probe.
}
\usage{
getMetadata_Probe_Meth450(theProbe,
theZipFile="/geneSurveyData/GeneSurvey.zip",
theVerboseFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theProbe}{ A vector of probe names as found in the
  getNames_ function for this data type. }
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
The return value is a list of objects with probe ids and names().
The following functions work to pull data out of the object.
  \item{get.Name}{ \link{get.Name} }
  \item{get.Chromosome}{ \link{get.Chromosome} }
  \item{get.ProbeLocation}{ \link{get.ProbeLocation} }
  \item{get.Strand}{ \link{get.Strand} }
  \item{get.GeneStructure}{ \link{get.GeneStructure} }
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
