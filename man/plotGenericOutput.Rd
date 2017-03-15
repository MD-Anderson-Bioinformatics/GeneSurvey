\name{plotGenericOutput}
\alias{plotGenericOutput}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotGenericOutput}
\description{
Creates a stripchart plot with optional replicate plot.
}
\usage{
plotGenericOutput(theGeneEq, theOutputDir, myGeneData, myBarcodeDiseases,
myBarcodeSampleType, theDataType, theDataTypeLabel, theVerboseFlag,
theTag, theGeneEqPre="", theReplicateFlag=FALSE, theExtraTitle="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGeneEq}{A string used to form the title and file name for the
  images. This should be the gene equivalent being plotted.}
  \item{theOutputDir}{Directory in which to create the output}
  \item{myGeneData}{Data in the format from the getData function for a data
  type. This is a matrix with barcodes (or patiend ids) as columns (colnames)
  and rows (rownames) as genes.
  For this function there should only be one gene.}
  \item{myBarcodeDiseases}{Data in the format from the
  getMetadataPop_PatientDisease or getMetadataPop_BarcodeDisease function.
  This is a vector of string values (diseases) with the patient ids or
  barcodes as the names(). Regular data uses barcodes, delta data uses
  patient ids.}
  \item{myBarcodeSampleType}{Data in the format from the
  getMetadataPop_BarcodeSamplecode or getMetadataPop_PatientDisease.
  This is a vector of string values (diseases) with the patient ids or
  barcodes as the names(). Regular data uses barcodes,
  delta data uses patient ids.}
  \item{theDataType}{The data type is a string for the platform that is
  used in the title and file name.}
  \item{theDataTypeLabel}{A string label for the Y axis, describing the
  data type being plotted.}
  \item{theVerboseFlag}{ TRUE means write all output, which can be
  very verbose. This has no default here.}
  \item{theTag}{An optional tag used to form the last part of the title
  and file name. This might be a good place to describe your data tweaks.
  Use "" to skip use.}
  \item{theGeneEqPre}{An optional tag used at the beginning of the title
  and file name. Use "" to skip use.}
  \item{theReplicateFlag}{TRUE means to write a replicates plot.}
  \item{theExtraTitle}{...}
}
\details{
Creates a stripchart plot with optional replicate plot.
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
