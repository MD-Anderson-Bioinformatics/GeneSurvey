library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataPlatform_GeneSymbol_RnaSeq(theZipFile=zipFile)

  (20532==dim(foo)[1])&&
  (2231==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
