\name{filterForCommonSamples}
\alias{filterForCommonSamples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
filterForCommonSamples
}
\description{
The filterForCommonSamples function does not need a path itself.
     You can pass it what ever getData_ results you would like. The
     getData_ vectors are returned within a list, filtered down to only
     common samples, using the first 15 characters of the TCGA sample
     id. (15 characters are used so tumor samples and normal are still
     distinguished.)
}
\usage{
filterForCommonSamples(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
The filterForCommonSamples function does not need a path itself.
     You can pass it what ever readGene results you would like. The
     getData_ vectors are returned within a list, filtered down to only
     common samples, using the first 15 characters of the TCGA sample
     id. (15 characters are used so tumor samples and normal are still
     distinguished.)

foo<-filterForCommonSamples(getData_GeneSymbol_RnaSeq2("TP53"), getData_GeneSymbol_RnaSeq("TP53"))
> foo[[1]][1:5]
TCGA-BL-A0C8-01 TCGA-BL-A13I-01 TCGA-BL-A13J-01 TCGA-BL-A13J-11 TCGA-BT-A0S7-01
      10.347709       11.158277       10.923567        9.501043       12.141164
> foo[[2]][1:5]
TCGA-BL-A0C8-01 TCGA-BL-A13I-01 TCGA-BL-A13J-01 TCGA-BL-A13J-11 TCGA-BT-A0S7-01
       5.193636        6.211073        5.821068        4.205293        6.926390

}
\value{
A list containing vectors filtered down to only
     common samples, using the first 15 characters of the TCGA sample
     id.
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
