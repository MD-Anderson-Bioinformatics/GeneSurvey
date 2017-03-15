library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMirs_Metadata_Mir("hsa-let-7c-5p", theZipFile=zipFile)

  ("MIMAT0000064"==foo[[1]]@mMimatId)&&
  (17912158==foo[[1]]@mLocationStart)
} else {
  message("No test data. Skip test.")
  TRUE
}
