library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataMatrix_GeneSymbol_Meth450(getNames_GeneSymbol_Meth450(theZipFile=zipFile)[1], theZipFile=zipFile)

  (16==dim(foo)[1])&&
  (10027==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
