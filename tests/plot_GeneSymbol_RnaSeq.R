library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- plot_GeneSymbol_RnaSeq("TP53", theOutputDir=file.path(baseDir, "plotRnaSeq"), theZipFile=zipFile)

  ("TP53_RNASeq_Stripchart.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
