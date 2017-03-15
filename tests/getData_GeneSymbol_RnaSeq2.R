library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo <- getData_GeneSymbol_RnaSeq2("?|100130426", theZipFile=zipFile)

  (1==dim(foo)[1])&&
  (10464==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
