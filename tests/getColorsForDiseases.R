library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  foo <- getColorsForDiseases(c("ACC", "BLCA",
                              "BRCA", "CESC", "CHOL", "CNTL", "COAD", "FOO", "DLBC", "ESCA",
                              "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC",
                              "LUAD", "LUSC", "MESO", "OV", "PAAD", "PCPG", "BAR", "PRAD", "READ",
                              "SARC", "SKCM", "STAD", "TCGT", "THCA", "UCEC", "UCS", "UVM"))
  bar <- c(
    "darkcyan", "palegreen1", "green", "blue",
    "purple", "firebrick", "midnightblue", "brown",
    "red", "slategray", "lightslateblue", "darkgreen",
    "magenta", "cadetblue3", "goldenrod", "violet",
    "grey", "olivedrab", "cyan", "gold",
    "turquoise", "chocolate", "pink", "dodgerblue",
    "mediumvioletred", "forestgreen", "brown1", "burlywood2",
    "darkgray", "orange", "darkslategray1", "seagreen",
    "tomato", "sienna", "darkorchid")
  (all(foo==bar))
} else {
  message("No test data. Skip test.")
  TRUE
}

