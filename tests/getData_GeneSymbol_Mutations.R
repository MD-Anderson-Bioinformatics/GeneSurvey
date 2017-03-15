library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo <- getData_GeneSymbol_Mutations("101928757", theZipFile=zipFile)
  (1==dim(foo)[1])&&(6434==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}

