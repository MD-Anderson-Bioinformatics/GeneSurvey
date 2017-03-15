library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo <- getDataObject_GeneSymbol_Meth27(getNames_GeneSymbol_Meth27(theZipFile=zipFile)[1:100], theZipFile=zipFile)

  (181==dim(foo@mData)[1])&&
  (2712==dim(foo@mData)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
