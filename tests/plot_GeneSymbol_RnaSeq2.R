library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  #foo <- plot_GeneSymbol_RnaSeq2("TP53", theOutputDir=file.path(baseDir, "plotRnaSeq2"), theZipFile=zipFile)
  foo <- plot_GeneSymbol_RnaSeq2("PRLR", theOutputDir=file.path(baseDir, "plotRnaSeq2"), theZipFile=zipFile)

  ("PRLR_RNASeqV2_Stripchart.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
