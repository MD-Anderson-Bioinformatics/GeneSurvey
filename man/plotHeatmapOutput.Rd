\name{plotHeatmapOutput}
\alias{plotHeatmapOutput}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plotHeatmapOutput}
\description{
Plot heatmap for methylation probes.
}
\usage{
plotHeatmapOutput(theGene, theOutputDir, theProbeData, theBarcodeDiseases,
theBarcodeSampleType, theDataType, theDataTypeLabel, theZipFile,
theVerboseFlag, theReadProbeFunction, theTag = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGene}{A string used to form the title and file name for the
  images. This should be the gene equivalent being plotted.}
  \item{theOutputDir}{Directory in which to create the output}
  \item{theProbeData}{This is a matrix with probes as columns (colnames)
  and rows (rownames) as barcodes (or patient ids). For this function there
  can (and probably should) be multiple columns and multiple rows.
  This is the transposed output from getData_Probe_Meth450.}
  \item{theBarcodeDiseases}{Data in the format from the
  getMetadataPop_PatientDisease or getMetadataPop_BarcodeDisease function.
  This is a vector of string values (diseases) with the patient ids or
  barcodes as the names().
  Regular data uses barcodes, delta data uses patient ids.}
  \item{theBarcodeSampleType}{Data in the format from the
  getMetadataPop_BarcodeSamplecode or getMetadataPop_PatientDisease.
  This is a vector of string values (diseases) with the patient ids
  or barcodes as the names(). Regular data uses barcodes, delta
  data uses patient ids.}
  \item{theDataType}{The data type is a string for the platform
  that is used in the title and file name.}
  \item{theDataTypeLabel}{A string label for the Y axis,
  describing the data type being plotted.}
  \item{theZipFile}{ No default. For DQS Servers, use
  "/geneSurveyData/GeneSurvey.zip".}
  \item{theVerboseFlag}{ Default: FALSE. TRUE means write all output,
  which can be very verbose. }
  \item{theReadProbeFunction}{Function to get data about a probe --
  getData_Probe_Meth450 or getData_Probe_Meth27 }
  \item{theTag}{An optional tag used to form the last part of the
  title and file name. This might be a good place to
  describe your data tweaks. Use "" to skip use.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
