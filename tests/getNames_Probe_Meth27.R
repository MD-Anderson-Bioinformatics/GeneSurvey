library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_Probe_Meth27(theZipFile=zipFile)

  (27578==length(foo))&&
  ("cg00000292"==foo[1])&&
  ("cg00183916"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
