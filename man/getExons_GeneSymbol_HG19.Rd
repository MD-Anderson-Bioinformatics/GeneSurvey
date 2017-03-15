\name{getExons_GeneSymbol_HG19}
\alias{getExons_GeneSymbol_HG19}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getExons_GeneSymbol_HG19}
\description{Get the exons for a particular gene}
\usage{
getExons_GeneSymbol_HG19(theGeneEq, theZipFile =
"/geneSurveyData/GeneSurvey.zip", theVerboseFlag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theGeneEq}{The gene symbol (or other entry from the HG19 data)--
  does not need to have the Ensembl id. If given just the symbol,
  all gene symbol|Ensembl ids will be returned.}
  \item{theZipFile}{ Default: For DQS Servers. If you are on a server
  where the GENE_REPORT directory is on a different path, you can pass in a
  value to override the default of
  "/geneSurveyData/GeneSurvey.zip". }
  \item{theVerboseFlag}{ Default: FALSE. TRUE means write all output,
  which can be very verbose. }
}
\details{
Collects all exons from the Ensembl definitions. All exons for all
transcripts for the gene requested will be returned.
See value for more details.
}
\value{
Returns a data.frame with the following colnames.
"gene" The gene symbol|Ensembl id for this row, such as,
TP53TG3|ENSG00000183632.
"chromosome" The chromosome will be a number from 1 to 16 or an X or Y.
"start" Start location for the Exon.
"end" End location for the Exon.
"strand" The + or - strand.
"exon_type" The exon type such as "protein_coding" or
"nonsense_mediated_decay".
"exon_id" The Ensembl Exon id such as ENSE00002304100.
"exon_number" The exon number--an integer from 1 to number of
exons. (Number can vary between traqnscripts.)
"transcript_id" The transcript id to which this Exon belongs,
such as ENST00000398680.
"transcript_symbol" The transcript symbol, a gene symbol, a dash,
and an integer, such as TP53TG3-201.
}
\references{}
\author{}
\note{}
\seealso{}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
