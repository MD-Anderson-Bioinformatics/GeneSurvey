library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getDataObject_CombinedHsaMimat_miRNASeq(getNames_CombinedHsaMimat_miRNASeq(theZipFile=zipFile)[1:10], theZipFile=zipFile)

  (10==dim(foo@mData)[1])&&
  (9657==dim(foo@mData)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
