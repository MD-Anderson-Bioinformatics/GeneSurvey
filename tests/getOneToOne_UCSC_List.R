library(GeneSurvey)

#################################################################
#################################################################

baseDir <- getBaseDir()
zipFile <- getZipDir()
if ((!is.null(baseDir))&&(!is.null(zipFile)))
{
  initGeneReport("-Xmx4800m")
  foo <- getOneToOne_UCSC_List(theZipFile=zipFile)

  (31692==length(foo))&&
  ("uc001aaa.3"==foo[1])&&
  ("uc001axj.2"==foo[200])
} else {
  message("No test data. Skip test.")
  TRUE
}
