library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- plotAssays(theGene1="TP53", theGene2="BRCA1",
                    theAssay1="RnaSeq2", theAssay2="RnaSeq2",
                    theOutputDir=file.path(baseDir, "plotAssays"), theZipFile=zipFile)

  ("assayplot_RnaSeq2_RnaSeq2_TP53_BRCA1_ALL.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
