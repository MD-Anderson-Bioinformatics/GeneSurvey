library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMetadata_GeneByNeighbor_RnaSeq(7500000, 7540000, "17", "-", theZipFile=zipFile)

  (7531194==foo[[1]]@mLocationStart)&&
  ("17"==foo[[1]]@mChromosome)
} else {
  message("No test data. Skip test.")
  TRUE
}
