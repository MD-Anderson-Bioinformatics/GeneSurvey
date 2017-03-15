library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getImputedNAs_GeneSymbol_RnaSeq(getNames_GeneSymbol_RnaSeq(theZipFile=zipFile)[1], theZipFile=zipFile)

  (1==dim(foo)[1])&&
  (2231==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
