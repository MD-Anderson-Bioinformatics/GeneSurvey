library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataPlatform_Probe_Meth27(theZipFile=zipFile)

  (27578==dim(foo)[1])&&
  (2717==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
