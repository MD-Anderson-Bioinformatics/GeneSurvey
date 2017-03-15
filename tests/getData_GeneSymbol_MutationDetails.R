library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo <- getData_GeneSymbol_MutationDetails("101928757", theZipFile=zipFile)
  ("TCGA-FX-A2QS-01A-11D-A21Q-09"==unlist(as.vector(unlist(foo["Tumor_Sample_Barcode"])))[1])
} else {
  message("No test data. Skip test.")
  TRUE
}

