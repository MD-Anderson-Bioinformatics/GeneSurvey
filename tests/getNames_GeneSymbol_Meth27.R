library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_GeneSymbol_Meth27(theZipFile=zipFile)

  (14475==length(foo))&&
  ("7A5"==foo[1])&&
  ("ADAM23"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
