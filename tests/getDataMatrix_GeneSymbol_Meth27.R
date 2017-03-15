library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataMatrix_GeneSymbol_Meth27("7A5", theZipFile=zipFile)

  (1==dim(foo)[1])&&
  (2712==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
