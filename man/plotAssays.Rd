\name{plotAssays}
\alias{plotAssays}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotAssays}
\description{
Plot data from different assays against each other
}
\usage{
plotAssays(theGene1, theGene2, theAssay1, theAssay2, theOutputDir,
theZipFile="/geneSurveyData/GeneSurvey.zip",
theUseDeltaFlag = FALSE, theRemoveDupFlag = TRUE, theVerboseFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGene1}{Gene equivalent for first axis. A string used to form
  the title and file name for the images.
  This should be the gene equivalent being plotted.}
  \item{theGene2}{Gene equivalent for second axis.
  A string used to form the title and file name for the images.
  This should be the gene equivalent being plotted.}
  \item{theAssay1}{Platform/assay for first axis.
  A string, one of Mutations, RnaSeq2, RnaSeq,
  SNP6, Meth450, Meth27, miRNASeq. }
  \item{theAssay2}{Platform/assay for first axis. A string, one of
  Mutations, RnaSeq2, RnaSeq, SNP6, Meth450, Meth27, miRNASeq. }
  \item{theOutputDir}{Directory in which to create the output}
  \item{theZipFile}{ Default: For DQS Servers. If you are on a server
  where the GENE_REPORT directory is on a different path, you can pass
  in a value to override the default of
  "/geneSurveyData/GeneSurvey.zip". }
  \item{theUseDeltaFlag}{ Default: FALSE. A flag indicating to use the
  differences in value between tumor and normal samples. }
  \item{theRemoveDupFlag}{ Default: FALSE. Remove the duplicate barcodes
  from the same control samples used in different diseases. }
  \item{theVerboseFlag}{ Default: FALSE. TRUE means write all output,
  which can be very verbose. }
}
\details{
Creates points plots of one gene-equivalent plus platform-assay
against another. There will be an ALL plot, with all disease types on
one plot, and then plots for each disease type separately.
}
\value{
returns vector of file names created.
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
