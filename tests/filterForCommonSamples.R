library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport()
  foo<-filterForCommonSamples(getData_GeneSymbol_RnaSeq2("TP53", theZipFile=zipFile), getData_GeneSymbol_RnaSeq("TP53", theZipFile=zipFile))
  ("10.348507"==sprintf("%f", foo[[1]][1]))&&
  ("11.158732"==sprintf("%f", foo[[1]][2]))&&
  ("10.924103"==sprintf("%f", foo[[1]][3]))&&
  ( "9.502478"==sprintf("%f", foo[[1]][4]))&&
  ("12.141395"==sprintf("%f", foo[[1]][5]))&&
  ("5.232450"==sprintf("%f", foo[[2]][1]))&&
  ("6.230378"==sprintf("%f", foo[[2]][2]))&&
  ("5.846312"==sprintf("%f", foo[[2]][3]))&&
  ("4.281304"==sprintf("%f", foo[[2]][4]))&&
  ("6.938178"==sprintf("%f", foo[[2]][5]))
} else {
  message("No test data. Skip test.")
  TRUE
}
