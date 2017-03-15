library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataPlatform_GeneSymbol_SNP6(theZipFile=zipFile)

  (57736==dim(foo)[1])&&
  (22447==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
