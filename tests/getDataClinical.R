library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataClinical(theZipFile=zipFile)

  (11160==dim(foo)[1])&&
  (15==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
