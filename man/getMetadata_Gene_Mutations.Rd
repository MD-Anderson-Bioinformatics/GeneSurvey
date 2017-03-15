\name{getMetadata_Gene_Mutations}
\alias{getMetadata_Gene_Mutations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getMetadata_Gene_Mutations
}
\description{Get metadata about the given gene}
\usage{
getMetadata_Gene_Mutations(theGene,
theZipFile="/geneSurveyData/GeneSurvey.zip",
theVerboseFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGene}{ A vector of gene symbols as found in the
  getNames_ function for this data type. }
  \item{theZipFile}{ Default: For DQS Servers.
  If you are on a server where the GENE_REPORT directory is on a
  different path, you can pass in a value to override the default of
  "/geneSurveyData/GeneSurvey.zip".
  NOTE: this is a subdirectory of GENE_REPORT. }
  \item{theVerboseFlag}{ Default: FALSE.
  TRUE means write all output, which can be very verbose. }
}
\details{
The getMetadata_gene functions return a list of sublists.
The sublists will often contain only a single metadata object.
However, the code is designed to handle multiple metadata objects for
the same gene symbol. This is necessary for handling the multiple
gene entries caused in various manners.

TCGA provided RNASeq (v1 and v2) gene mappings which are based on
UCSC's transcripts data file and have "duplicate" "gene" entries.
The "duplicates" are caused because the file in question,
KnownGenes.txt, does not actually provide gene annotations.
Instead of "genes", the file is measure transcript matches as generated
by an automated, uncurated, system. This means wherever a match
is "close enough", that part will be labeled with the matching gene,
even if it does not match the NIH approved gene coordinates.

Other platforms, such as Methylation 27 and 450 and SNP6 which use the
Gene Score algorithm have multiple entries for genes caused by alleles.
These entries are current uniquely identified by adding first Chromosome
and then some other identifier such as Entrez or Ensembl id to the gene symbol.
}
\value{
The return value is a list of objects with gene data.
The following functions work to pull data out of the object.
  \item{get.GeneSymbol}{ \link{get.GeneSymbol} }
  \item{get.GeneId}{ \link{get.GeneId} }
  \item{get.VersionIndex}{ \link{get.VersionIndex} }
  \item{get.LocationStart}{ \link{get.LocationStart} }
  \item{get.LocationEnd}{ \link{get.LocationEnd} }
  \item{get.Chromosome}{ \link{get.Chromosome} }
  \item{get.Strand}{ \link{get.Strand} }
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
