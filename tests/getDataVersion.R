library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataVersion(theZipFile=zipFile)

  foo
} else {
  message("No test data. Skip test.")
  TRUE
}
