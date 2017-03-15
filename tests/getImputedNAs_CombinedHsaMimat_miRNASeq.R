library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  # "hsa-let-7a.MIMAT0000062"
  foo <- getImputedNAs_CombinedHsaMimat_miRNASeq(getNames_CombinedHsaMimat_miRNASeq(theZipFile=zipFile)[1], theZipFile=zipFile)

  (1==dim(foo)[1])&&
  (9657==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
