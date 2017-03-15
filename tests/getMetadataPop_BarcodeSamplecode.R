library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMetadataPop_BarcodeSamplecode(theZipFile=zipFile)

  (156972==length(foo))
} else {
  message("No test data. Skip test.")
  TRUE
}
