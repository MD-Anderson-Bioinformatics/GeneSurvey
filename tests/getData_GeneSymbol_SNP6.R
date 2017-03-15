library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  message(baseDir)
  message(zipFile)
  initGeneReport()
  foo <- getData_GeneSymbol_SNP6("5S_rRNA|12", theZipFile=zipFile)
  (1==dim(foo)[1])&&(22447==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
