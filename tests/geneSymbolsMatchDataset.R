library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  (TRUE==geneSymbolsMatchDataset(c("ZWINT|11130", "TP53"), getNames_GeneSymbol_RnaSeq2(theZipFile=zipFile)))&&
  (FALSE==geneSymbolsMatchDataset(c("TP53", "foo"), getNames_GeneSymbol_RnaSeq2(theZipFile=zipFile)))
} else {
  message("No test data. Skip test.")
  TRUE
}

