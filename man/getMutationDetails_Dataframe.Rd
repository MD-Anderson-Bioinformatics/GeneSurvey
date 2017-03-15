\name{getMutationDetails_Dataframe}
\alias{getMutationDetails_Dataframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getMutationDetails_Dataframe}
\description{Get a dataframe for a given gene with individual mutations noted.}
\usage{
getMutationDetails_Dataframe(theGene,
theZipFile = "/geneSurveyData/GeneSurvey.zip",
theVerboseFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGene}{Gene symbol corresponding with results from
  getNames_Mutation_Details results.}
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
Dataframe with columns being headers and each row an individual mutation.
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
