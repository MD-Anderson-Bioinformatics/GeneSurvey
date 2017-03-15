\name{get.GenomeAnnotations}
\alias{get.GenomeAnnotations}
\alias{get.GenomeAnnotations-method}
\alias{get.GenomeAnnotations,ANY-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get.GenomeAnnotations}
\description{
for Probes returns a data.frame with six columns named: probeId, fromGene, chromosome, strand, bpLocation, and geneStructure
for Genes returns a data.frame with seven columns named: geneSymbol, geneId, versionIndex, chromosome, locationStart, locationEnd, and strand
}
\usage{
get.GenomeAnnotations(theObject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theObject}{An object returned from one of the getDataObject_ functions.}
}
\details{
for Probes returns a data.frame with six columns named: probeId, fromGene, chromosome, strand, bpLocation, and geneStructure
}
\value{
for Probes returns a data.frame with six columns named: probeId, fromGene, chromosome, strand, bpLocation, and geneStructure
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
