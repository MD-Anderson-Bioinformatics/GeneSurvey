library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo <- getTranscripts_GeneSymbol_HG19("TP53TG3", theZipFile=zipFile)
  (9==length(foo))
} else {
  message("No test data. Skip test.")
  TRUE
}

