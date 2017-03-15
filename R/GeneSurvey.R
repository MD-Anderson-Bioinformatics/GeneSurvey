#GeneSurvey Copyright 2014, 2015, 2016 University of Texas MD Anderson Cancer Center
#
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.
#
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(rJava)
library(Cairo)
library(pheatmap)
library(methods)

compressIntoFilename<-function(theString)
{
	### listing whole list of characters out looks wrong, but is locale independent
	theString <- gsub("[^ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789|_/\\/.-]", "", theString)
	theString <- gsub("\\", "_", theString, fixed=TRUE)
	theString <- gsub("/", "_", theString, fixed=TRUE)
	theString <- gsub("|", "-", theString, fixed=TRUE)
	return(theString)
}

padChromosomeName <- function(theChr)
{
	if(1==nchar(theChr))
	{
		if (("x"!=theChr)&&("X"!=theChr)&&("y"!=theChr)&&("Y"!=theChr))
		{
			theChr <- paste("0", theChr, sep="")
		}
	}
	theChr
}

geneReportVersion <- function()
{
	"GeneSurvey 2017-03-13-1625"
}

initGeneReport <- function(theParameters="-Xmx2400m")
{
	stopifnot(is.character(theParameters))
	message(geneReportVersion())
	message("note: parameters must be in terms of 'm' or .jinit will fail on Linux")
	message("In testing, -Xmx42000m worked, while larger values caused core exceptions")
	myJavaJars <- file.path(
		system.file("GSAccess", "commons-codec-1.9.jar", package="GeneSurvey"),
		system.file("GSAccess", "GSAccess.jar", package="GeneSurvey"),
		fsep=.Platform$path.sep)
	message("Calling .jinit theParameters=", theParameters)
	message(myJavaJars)
	.jinit(classpath=myJavaJars, force.init = TRUE, parameters=theParameters)
	message("After .jinit")
	message("Java version")
	message(J("java.lang.System")$getProperty("java.version"))
	message("Print version")
	cfr <- .jnew("org/mda/bcb/gsaccess/CallFromR", "")
	.jcall(cfr, returnSig = "S", method="printVersion")
	# call to pdf(NULL) is included to prevent palette and other functions
	# from creating useless un-asked for PDF files
	#pdf(NULL)
}

verboseMessage <- function(..., theVerboseFlag=FALSE)
{
	if(isTRUE(theVerboseFlag))
	{
		message(...)
	}
}

setJavaVerboseFlag <- function(theVerboseFlag)
{
	objFlag <- .jnew("java/lang/Boolean", theVerboseFlag)
	.jcall("org/mda/bcb/gsaccess/GSAccess", returnSig = "V", method="setVerboseFlag", objFlag)
}

matrixWithIssues<-function(...)
{
	warnLevel<-getOption("warn")
	on.exit(options(warn=warnLevel))
	options(warn=3) # warnings are errors
	return(matrix(...))
}

isValidDirectoryPath <- function(thePath)
{
	marker <- file.info(thePath)[1,"isdir"]
	if (is.na(marker))
	{
		dir.create(thePath, recursive = TRUE, showWarnings=FALSE)
		marker <- file.info(thePath)[1,"isdir"]
		unlink(thePath)
	}
	marker
}

#################################################################
#################################################################
#################################################################
#################################################################

getDataVersion <- function(theZipFile="/geneSurveyData/GeneSurvey.zip")
{
	stopifnot(file.exists(theZipFile))
	jReadGeneObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	results <- .jcall(jReadGeneObj, returnSig = "Ljava/lang/String;", method="getValue_Time")
	results
}

#################################################################
#################################################################
#################################################################
#################################################################

geneSymbolsForDataset <- function(theRequestVector, theGeneSymbolVector)
{
	matches <- NULL
	unmatched <- NULL
	for(theGene in theRequestVector)
	{
		##message("check ", theGene)
		if (theGene %in% theGeneSymbolVector)
		{
			message("matches 1-1 ", theGene)
			matches <- c(matches, theGene)
		}
		else
		{
			matchCheck <- grepl(paste("^", theGene, "\\|.*$", sep=""), theGeneSymbolVector, perl=TRUE)
			if (sum(matchCheck)>0)
			{
				##message("matches 1-1 ", theGene, " and ", packageList[matchCheck])
				matches <- c(matches, theGeneSymbolVector[matchCheck])
			}
			else
			{
				##message("unmatched ", theGene)
				unmatched <- c(unmatched, theGene)
			}
		}
	}
	results <- list(matches, unmatched)
	names(results) <- c("matched", "unmatched")
	results
}

geneSymbolsMatchDataset <- function(theRequestVector, theGeneSymbolVector)
{
	matches <- (0==sum((theRequestVector %in% theGeneSymbolVector)==FALSE))
	matches
}

getSynonyms <- function(theId, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	setJavaVerboseFlag(theVerboseFlag)
	verboseMessage("getSynonyms theId=", theId, theVerboseFlag=theVerboseFlag)
	results <- NULL
	jReadGeneObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	results <- .jcall(jReadGeneObj, returnSig = "[S", method="getList_GeneSymbol_Synonym",
										.jnew("java/lang/String",theId))
	results
}

#################################################################
#################################################################

getBaseDir <- function()
{
  baseDir <- Sys.getenv("GS_TEST_OUTPUT")
  if(!file.exists(baseDir))
  {
    baseDir <- "/temp/test_package"
    if(!file.exists(baseDir))
    {
      baseDir <- "/geneSurveyData/TESTING/test_package"
      if(!file.exists(baseDir))
      {
        if(!file.exists(baseDir))
        {
          baseDir <- NULL
        }
      }
    }
  }
  baseDir
}


getZipDir <- function()
{
  zipFile <- Sys.getenv("GS_ZIP_ARCHIVE")
  if(!file.exists(zipFile))
  {
    zipFile <- "/temp/GeneSurvey.zip"
    if(!file.exists(zipFile))
    {
      zipFile <- "/geneSurveyData/TESTING/GeneSurvey.zip"
      if(!file.exists(zipFile))
      {
        zipFile <- NULL
      }
    }
  }
  zipFile
}

#################################################################
#################################################################
#################################################################
#################################################################
