library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_GeneSymbol_Meth450(theZipFile=zipFile)

  (21231==length(foo))&&
  ("A1BG"==foo[1])&&
  ("ACTB"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
