library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_Probe_Meth450(theZipFile=zipFile)

  (485577==length(foo))&&
  ("cg00000029"==foo[1])&&
  ("cg00008629"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
