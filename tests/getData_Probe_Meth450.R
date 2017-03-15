library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getData_Probe_Meth450("cg00000292", theZipFile=zipFile)

  (1==dim(foo)[1])&&
  (10027==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
