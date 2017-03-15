library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  probes <-  as.vector(unlist(getNames_ProbeFromGeneSymbol_Meth450("TP53", theZipFile=zipFile, FALSE)))
  # note transpose here
  mydata <- t(getData_Probe_Meth450(probes, theZipFile=zipFile, FALSE, TRUE, FALSE))
  barDis <- getMetadataPop_BarcodeDisease(theZipFile=zipFile)
  barSam <- getMetadataPop_BarcodeSamplecode(theZipFile=zipFile)
  foo <- plotHeatmapOutput("TP53",
                           theOutputDir=file.path(baseDir, "plotHeatmapOutput"),
                           theProbeData=mydata,
                           theBarcodeDiseases=barDis,
                           theBarcodeSampleType=barSam,
                           theDataType="Meth450",
                           theDataTypeLabel="Count",
                           theZipFile=zipFile,
                           theVerboseFlag=FALSE,
                           theReadProbeFunction=getMetadata_Probe_Meth450)

  ("TP53_Meth450_Heatmap.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
