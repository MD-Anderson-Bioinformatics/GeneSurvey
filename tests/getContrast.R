library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo <- getContrast(file.path(baseDir, "contrast"), "TP53", "SNP6", "SRC", "RnaSeq2", theZipFile=zipFile)
  ("-0.091055"==sprintf("%f", foo[[1]][[1]]))
} else {
  message("No test data. Skip test.")
  TRUE
}

