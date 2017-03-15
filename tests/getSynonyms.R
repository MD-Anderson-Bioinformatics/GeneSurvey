library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getSynonyms("P53", theZipFile=zipFile)

  (1==length(foo))&&
  ("TP53"==foo[1])
} else {
  message("No test data. Skip test.")
  TRUE
}
