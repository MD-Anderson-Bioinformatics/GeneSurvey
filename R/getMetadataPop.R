#GeneSurvey Copyright 2014, 2015, 2016 University of Texas MD Anderson Cancer Center
#
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.
#
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

#################################################################
#################################################################
# internal
#################################################################
#################################################################

getMetadataPop_internal <- function(theZipFile, theMethodString, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	results <- NULL
	jReadGeneObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	result <- .jcall(jReadGeneObj, returnSig = "Lorg/mda/bcb/gsaccess/retrieve/MetadataPop;", method=theMethodString)
	if(FALSE==is.jnull(result))
	{
		results <- result$mValues
		names(results) <- result$mIds
	}
	results
}

#################################################################
#################################################################
# exported
#################################################################
#################################################################

getMetadataPop_BarcodeDisease <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	getMetadataPop_internal(theZipFile, 'getMetadataPop_BarcodeDisease', theVerboseFlag=theVerboseFlag)
}

getMetadataPop_BarcodeSamplecode <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	getMetadataPop_internal(theZipFile, 'getMetadataPop_BarcodeSamplecode', theVerboseFlag=theVerboseFlag)
}

getMetadataPop_PatientDisease <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	getMetadataPop_internal(theZipFile, 'getMetadataPop_PatientDisease', theVerboseFlag=theVerboseFlag)
}

### used outside this file, but not exported

getMetadataPop_BarcodeDisease_forList <- function(theList, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	results <- getMetadataPop_BarcodeDisease(theZipFile, theVerboseFlag=theVerboseFlag)
	results <- results[theList]
	names(results) <- theList
	results <- as.vector(unlist(lapply(results, function(theVal)
	{
		if (is.na(theVal))
		{
			theVal <- "UNK"
		}
		theVal
	})))
	names(results) <- theList
	results
}

getMetadataPop_BarcodeSamplecode_forList <- function(theList, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	results <- getMetadataPop_BarcodeSamplecode(theZipFile, theVerboseFlag=theVerboseFlag)
	results <- results[theList]
	names(results) <- theList
	results <- as.vector(unlist(lapply(results, function(theVal)
	{
		if (is.na(theVal))
		{
			theVal <- "UNK"
		}
		theVal
	})))
	names(results) <- theList
	results
}

getMetadataPop_PatientDisease_forList <- function(theList, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	results <- getMetadataPop_PatientDisease(theZipFile, theVerboseFlag=theVerboseFlag)
	results <- results[theList]
	names(results) <- theList
	results <- as.vector(unlist(lapply(results, function(theVal)
	{
		if (is.na(theVal))
		{
			theVal <- "UNK"
		}
		theVal
	})))
	names(results) <- theList
	results
}

#################################################################
#################################################################
#################################################################
#################################################################
