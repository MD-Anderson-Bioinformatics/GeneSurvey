library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- plot_GeneSymbol_Meth450("TP53", theOutputDir=file.path(baseDir, "plotMeth450"), theZipFile=zipFile)

  ("17-7572391-TP53-cg22949073_Meth450_Stripchart.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
