library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getNames_Mutation_Details(theZipFile=zipFile)

  (28744==length(foo))&&
  ("."==foo[1])&&
  ("AC009945.4"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
