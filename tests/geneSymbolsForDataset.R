library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  packageVector <- getNames_GeneSymbol_RnaSeq2(theZipFile=zipFile)
  requestVector <- c("ZWINT", "TP53", "FBXW7", "FOO", "TBC1D3P1-DHX40P1")
  matches <- geneSymbolsForDataset(requestVector, packageVector)

  ("FOO"==matches$unmatched)&&
  ("ZWINT|11130"==matches$matches[[1]])&&
  ("TP53"==matches$matches[[2]])&&
  ("FBXW7|55294"==matches$matches[[3]])&&
  ("TBC1D3P1-DHX40P1"==matches$matches[[4]])
} else {
  message("No test data. Skip test.")
  TRUE
}
