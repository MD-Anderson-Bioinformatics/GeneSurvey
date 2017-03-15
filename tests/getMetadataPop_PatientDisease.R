library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMetadataPop_PatientDisease(theZipFile=zipFile)

  (11320==length(foo))
} else {
  message("No test data. Skip test.")
  TRUE
}
