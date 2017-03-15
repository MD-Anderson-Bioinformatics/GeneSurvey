library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_ProbeFromGeneSymbol_Meth27("TP53", theZipFile=zipFile)

  ("cg11519508"==foo$TP53[1])&&
  ("cg22175811"==foo$TP53[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
