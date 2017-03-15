library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getMirs_List_Mimat(theZipFile=zipFile)

  (4665==length(foo))&&
  ("MI0000060"==foo[1])
} else {
  message("No test data. Skip test.")
  TRUE
}
