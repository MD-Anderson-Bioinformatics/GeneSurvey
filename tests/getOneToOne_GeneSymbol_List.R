library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getOneToOne_GeneSymbol_List(theZipFile=zipFile)

  (31692==length(foo))&&
  ("6M1-18"==foo[1])&&
  ("ACE"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
