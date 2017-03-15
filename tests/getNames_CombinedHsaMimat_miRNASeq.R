library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_CombinedHsaMimat_miRNASeq(theZipFile=zipFile)

  (980==length(foo))&&
  ("hsa-let-7a.MIMAT0000062"==foo[1])&&
  ("hsa-mir-190b.MIMAT0004929"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
