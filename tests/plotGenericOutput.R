library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  mydata <- getData_GeneSymbol_RnaSeq2("TP53", theZipFile=zipFile)
  barDis <- getMetadataPop_BarcodeDisease(theZipFile=zipFile)
  barSam <- getMetadataPop_BarcodeSamplecode(theZipFile=zipFile)
  barTyp <- getMetadataPop_BarcodeSamplecode(theZipFile=zipFile)
  foo <- plotGenericOutput("TP53",
                           theOutputDir=file.path(baseDir, "plotGenericOutput"),
                           myGeneData=mydata,
                           myBarcodeDiseases=barDis,
                           myBarcodeSampleType=barTyp,
                           theDataType="RnaSeq2",
                           theDataTypeLabel="Log Normalized Count",
                           theVerboseFlag=FALSE,
                           theTag="")

  ("TP53_RnaSeq2_Stripchart.PNG"==basename(foo[1]))
} else {
  message("No test data. Skip test.")
  TRUE
}
