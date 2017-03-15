library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMetadata_Gene_Mutations("TP53", theZipFile=zipFile)

  (7565097==foo$TP53[[1]]@mLocationStart)&&
  ("17"==foo$TP53[[1]]@mChromosome)
} else {
  message("No test data. Skip test.")
  TRUE
}
