\name{getColorsForDiseases}
\alias{getColorsForDiseases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getColorsForDiseases
}
\description{
Given a list of TCGA disease types, returns a list of pre-assigned colors, adding new diseases/colors dynamically.
}
\usage{
getColorsForDiseases(theDiseaseList)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theDiseaseList}{
A vector of diseases-known diseases are "ACC", "BLCA",
          "BRCA", "CESC", "CHOL", "CNTL", "COAD", "DLBC", "ESCA",
          "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC",
          "LUAD", "LUSC", "MESO", "OV", "PAAD", "PCPG", "PRAD", "READ",
          "SARC", "SKCM", "STAD", "TCGT", "THCA", "UCEC", "UCS", "UVM"
}
}
\details{
Given a list of TCGA disease types, returns a list of pre-assigned colors, adding new diseases/colors dynamically.
}
\value{
     A vector of color strings with diseases as the names() for the
     vector. Known disease colors are: "darkcyan", "green", "blue",
     "purple", "firebrick", "midnightblue", "brown", "red",
     "lightgray", "darkgreen", "magenta", "cadetblue1", "goldenrod",
     "violet", "grey", "olivedrab", "cyan", "gold", "turquoise",
     "chocolate", "pink", "dodgerblue", "mediumvioletred",
     "forestgreen", "brown1", "bisque", "darkgray", "orange", "khaki",
     "seagreen", "tomato", "sienna", "darkorchid".
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
