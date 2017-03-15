\name{getNames_Mutation_Details}
\alias{getNames_Mutation_Details}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getNames_Mutation_Details}
\description{Return a vector of gene symbols with mutation details provided.}
\usage{
getNames_Mutation_Details(
theZipFile = "/geneSurveyData/GeneSurvey.zip",
theVerboseFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theZipFile}{ Default: For DQS Servers. If you are on a server
  where the GENE_REPORT directory is on a different path, you can pass
  in a value to override the default of
  "/geneSurveyData/GeneSurvey.zip".
  NOTE: This is a subdirectory of the main GENE_REPORT directory. }
  \item{theVerboseFlag}{ Default: FALSE.
  TRUE means write all output, which can be very verbose. }
}
\details{}
\value{
Vector of gene symbols with mutation details provided.
}
\references{}
\author{}
\note{}
\seealso{}
\examples{#See examples in tests directory in the install.}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
