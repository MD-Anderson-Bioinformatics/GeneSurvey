\name{getContrast}
\alias{getContrast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getContrast}
\description{
Give two genes and two assays (platforms), match up the data by sample/patient,
calculate the correlations, print scatterplots, and return the correlations
and data used for them.
}
\usage{
getContrast(theOutdir, theGene1, theAssay1, theGene2, theAssay2,
theZipFile="/geneSurveyData/GeneSurvey.zip",
theMethod = "spearman", theUse = "pairwise.complete.obs",
theUseDeltaFlag = FALSE, theRemoveDupFlag = TRUE, theVerboseFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theOutdir}{Directory to which to write the diagrams}
  \item{theGene1}{Gene or gene equivalent, which should match the
  gene equivalents for the matching assay}
  \item{theAssay1}{One of Meth450, Meth27, miRNASeq, SNP6, RnaSeq, or RnaSeq2}
  \item{theGene2}{Gene or gene equivalent, which should match the gene
  equivalents for the matching assay}
  \item{theAssay2}{One of Meth450, Meth27, miRNASeq, SNP6, RnaSeq, or RnaSeq2}
  \item{theZipFile}{ Default: For DQS Servers. If you are on a server where
  the GENE_REPORT directory is on a different path, you can pass in a value
  to override the default of
  "/geneSurveyData/GeneSurvey.zip". }
  \item{theMethod}{Default: "spearman". Value for the method argument to cor:
  "pearson", "kendall", "spearman"}
  \item{theUse}{Default: "pairwise.complete.obs". Value for the use argument
  to cor: "everything", "all.obs", "complete.obs",
  "na.or.complete", "pairwise.complete.obs"}
  \item{theUseDeltaFlag}{ Default: FALSE. A flag indicating to use the
  differences in value between tumor and normal samples. }
  \item{theRemoveDupFlag}{ Default: FALSE. Remove the duplicate barcodes
  from the same control samples used in different diseases. }
  \item{theVerboseFlag}{ Default: FALSE. TRUE means write all output,
  which can be very verbose. }
}
\details{
Give two genes and two assays (platforms), match up the data by
sample/patient, calculate the correlations, print scatterplots,
and return the corrrelations and data used for them.
}
\value{
A list of sublists (with names()) corresponding to "Overall"
and then the valid disease types.
Each sublist has [[1]] the results from calling cor,
[[2]] the first data set used in calling cor, and
[[3]] the second data set used in calling cor.
Sample ids are barcodes reduced to 19 characters unless delta data is used,
in which case it is the patient id (12 characters).
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
