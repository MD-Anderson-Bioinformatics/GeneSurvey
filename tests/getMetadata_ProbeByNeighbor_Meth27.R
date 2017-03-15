library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMetadata_ProbeByNeighbor_Meth27(7500000, 7540000, "17", "-", theZipFile=zipFile)

  (7530998==foo$cg01191395@mProbeLocation)&&
  ("17"==foo$cg01191395@mChromosome)
} else {
  message("No test data. Skip test.")
  TRUE
}
