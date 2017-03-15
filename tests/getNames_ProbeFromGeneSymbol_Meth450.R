library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_ProbeFromGeneSymbol_Meth450("TP53", theZipFile=zipFile)

  (39==length(foo$TP53))&&
  ("cg00807143"==foo$TP53[1])&&
  ("cg01620719"==foo$TP53[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
