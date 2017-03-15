library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMetadata_Probe_Meth450(getNames_Probe_Meth450(theZipFile=zipFile)[100], theZipFile=zipFile)

  (74678836==foo$cg00004089@mProbeLocation)&&
  ("17"==foo$cg00004089@mChromosome)
} else {
  message("No test data. Skip test.")
  TRUE
}
