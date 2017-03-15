library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMetadata_Probe_Meth27(getNames_Probe_Meth27(theZipFile=zipFile)[100], theZipFile=zipFile)

  (11562367==foo$cg00090147@mProbeLocation)&&
  ("8"==foo$cg00090147@mChromosome)
} else {
  message("No test data. Skip test.")
  TRUE
}
