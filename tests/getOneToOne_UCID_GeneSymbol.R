library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getOneToOne_UCID_GeneSymbol("DDX11L1", theZipFile=zipFile)

  ("uc001aaa.3"==as.vector(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
