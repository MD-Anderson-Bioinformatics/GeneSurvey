library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataPlatform_GeneSymbol_Mutations(theZipFile=zipFile)

  (31779==dim(foo)[1])&&
  (6441==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
