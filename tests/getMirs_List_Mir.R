library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMirs_List_Mir(theZipFile=zipFile)

  (4446==length(foo))&&
  ("hsa-let-7a-1"==foo[1])
} else {
  message("No test data. Skip test.")
  TRUE
}
