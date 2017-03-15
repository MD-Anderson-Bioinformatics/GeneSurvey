library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- plot_CombinedHsaMimat_miRNASeq("hsa-let-7a.MIMAT0000062", theOutputDir=file.path(baseDir, "plotmiRNASeq"), theZipFile=zipFile)

  ("hsa-let-7a.MIMAT0000062_miRNASeq_Stripchart.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
