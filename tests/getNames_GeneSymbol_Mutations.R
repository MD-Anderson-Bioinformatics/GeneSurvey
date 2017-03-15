library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_GeneSymbol_Mutations(theZipFile=zipFile)

  (31779==length(foo))&&
  ("101928757"==foo[1])&&
  ("AC010547.9"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
