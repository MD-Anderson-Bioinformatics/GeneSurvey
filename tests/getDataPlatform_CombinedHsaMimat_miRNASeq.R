library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataPlatform_CombinedHsaMimat_miRNASeq(theZipFile=zipFile)

  (980==dim(foo)[1])&&
  (9695==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
