library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo <- getData_GeneSymbol_RnaSeq("?|100130426", theZipFile=zipFile)

  (1==dim(foo)[1])&&
  (2231==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
