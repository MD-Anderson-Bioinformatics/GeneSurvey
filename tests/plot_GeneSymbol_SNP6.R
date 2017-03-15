library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  #foo <- plot_GeneSymbol_SNP6("TP53", theOutputDir=file.path(baseDir, "plotSNP6"), theZipFile=zipFile)
  foo <- plot_GeneSymbol_SNP6("PRLR", theOutputDir=file.path(baseDir, "plotSNP6"), theZipFile=zipFile, theVerboseFlag=TRUE)

  ("PRLR_SNP6_Stripchart.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
