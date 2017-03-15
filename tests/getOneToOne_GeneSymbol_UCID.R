library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getOneToOne_GeneSymbol_UCID("uc001aaa.3", theZipFile=zipFile)

  ("DDX11L1"==as.vector(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
