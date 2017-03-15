library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_GeneSymbol_RnaSeq2(theZipFile=zipFile)

  (20531==length(foo))&&
  ("?|100130426"==foo[1])&&
  ("ACPT"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
