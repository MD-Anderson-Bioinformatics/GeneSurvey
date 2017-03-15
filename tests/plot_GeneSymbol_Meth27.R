library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- plot_GeneSymbol_Meth27("TP53", theOutputDir=file.path(baseDir, "plotMeth27"), theZipFile=zipFile)

  ("17-7590052-TP53-cg22175811_Meth27_Stripchart.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
