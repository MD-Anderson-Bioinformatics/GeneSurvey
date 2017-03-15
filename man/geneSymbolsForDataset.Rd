\name{geneSymbolsForDataset}
\alias{geneSymbolsForDataset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{geneSymbolsForDataset}
\description{
Take a vector of purported gene names and a vector of valid gene names and return a list containing matched and unmatched names. The matched names may also be corrected, such as when the RNASeq data adds a pipe and Entrez id to a gene symbol.
}
\usage{
geneSymbolsForDataset(theRequestVector, theGeneSymbolVector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theRequestVector}{Vector of purported gene symbols.}
  \item{theGeneSymbolVector}{Vector of valid gene symbols.}
}
\details{
Take a vector of purported gene symbol and a vector of valid gene names and return a list containing matched and unmatched names. The matched names may also be corrected, such as when the RNASeq data adds a pipe and Entrez id to a gene symbol.

This means a purported gene symbol of TP53 is returned as simply TP53, with a direct match.
A purported gene symbol of FBXW7 is returned as matched to FBXW7|55294, since the TCGA Standardized Data from MD Anderson keeps the Entrez id, since the FBXW7 gene symbol was not in the translation file when this data set was processed.
A purported gene symbol of FOO is returned as unmatched, since (at present) there is no such gene symbol.
}
\value{
A list with two named elements, matched and unmatched. The matched components gives valid/known genes for the give gene list, while unmatched indicates genes for which no match was found.
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
