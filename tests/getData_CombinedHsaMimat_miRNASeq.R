library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo <-getData_CombinedHsaMimat_miRNASeq("hsa-let-7a.MIMAT0000062", theZipFile=zipFile)
  ("15.718006"==sprintf("%f", foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}

