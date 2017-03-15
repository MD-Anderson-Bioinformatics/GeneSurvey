library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataPlatform_GeneSymbol_RnaSeq2(theZipFile=zipFile)

  (20531==dim(foo)[1])&&
  (10464==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
