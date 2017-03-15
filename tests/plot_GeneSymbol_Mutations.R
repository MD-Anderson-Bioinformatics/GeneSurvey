library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- plot_GeneSymbol_Mutations("FIBCD1", theOutputDir=file.path(baseDir, "plotMutations"), theZipFile=zipFile)

  ("Mutations_AminoAcid_FIBCD1_Disease_Type-Variant_Classification.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
