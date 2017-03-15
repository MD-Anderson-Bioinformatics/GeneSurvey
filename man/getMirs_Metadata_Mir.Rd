\name{getMirs_Metadata_Mir}
\alias{getMirs_Metadata_Mir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getMirs_Metadata_Mir}
\description{Give a mir id, get the list of metadata mirs that match that id}
\usage{
getMirs_Metadata_Mir(theMirId,
theZipFile="/geneSurveyData/GeneSurvey.zip",
theVerboseFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theMirId}{A mir id--one of the strings from getMirs_List_Mir}
  \item{theZipFile}{ Default: For DQS Servers. If you are on a server
  where the GENE_REPORT directory is on a different path, you can pass in a
  value to override the default of
  "/geneSurveyData/GeneSurvey.zip".
  NOTE: this is a subdirectory of GENE_REPORT. }
  \item{theVerboseFlag}{ Default: FALSE.
  TRUE means write all output, which can be very verbose. }
}
\details{
}
\value{
Return a list of Metadata_Mir objects.
The following functions work to pull data out of the object.
  \item{get.MirId}{ \link{get.MirId} }
  \item{get.MimatId}{ \link{get.MimatId} }
  \item{get.MirType}{ \link{get.MirType} }
  \item{get.Chromosome}{ \link{get.Chromosome} }
  \item{get.ProbeLocation}{ \link{get.ProbeLocation} }
  \item{get.Strand}{ \link{get.Strand} }
  \item{get.GeneStructure}{ \link{get.GeneStructure} }
  \item{get.DerivedFrom}{ \link{get.DerivedFrom} }
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
