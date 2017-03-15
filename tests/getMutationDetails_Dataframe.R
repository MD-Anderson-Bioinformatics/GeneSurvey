library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMutationDetails_Dataframe("TP53", theZipFile=zipFile)

  (2036==dim(foo)[1])&&
  (44==dim(foo)[2])
} else {
  message("No test data. Skip test.")
  TRUE
}
