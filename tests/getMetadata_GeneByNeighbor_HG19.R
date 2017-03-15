library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  # 17	7512445	7531642	-
  foo <- getMetadata_GeneByNeighbor_HG18(7500000, 7540000, "17", "-", theZipFile=zipFile)

  (7512445==foo[[1]]@mLocationStart)&&
  ("17"==foo[[1]]@mChromosome)
} else {
  message("No test data. Skip test.")
  TRUE
}
