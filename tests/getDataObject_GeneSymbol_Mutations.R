library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo <- getDataObject_GeneSymbol_Mutations("101928757", theZipFile=zipFile)

  (1==dim(foo@mData)[1])&&
  (6434==dim(foo@mData)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
