library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMirs_Metadata_Mimat("MIMAT0000062", theZipFile=zipFile)

  ("hsa-let-7a-5p"==foo[[1]]@mMirId)&&
  (122017276==foo[[1]]@mLocationStart)
} else {
  message("No test data. Skip test.")
  TRUE
}
