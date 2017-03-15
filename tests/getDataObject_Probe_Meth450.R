library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataObject_Probe_Meth450(getNames_Probe_Meth450(theZipFile=zipFile)[1:10], theZipFile=zipFile)

  (10==dim(foo@mData)[1])&&
  (10027==dim(foo@mData)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
