library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_GeneSymbol_SNP6(theZipFile=zipFile)

  (57736==length(foo))&&
  ("5S_rRNA|12"==foo[1])&&
  ("AC002064.5"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
