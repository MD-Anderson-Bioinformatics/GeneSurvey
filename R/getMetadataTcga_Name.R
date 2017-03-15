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

getMetadataTcga_Name_internal <- function(theId, theZipFile, theMethodString, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	verboseMessage("getMetadataTcga_Name_internal theId=", theId, theVerboseFlag=theVerboseFlag)
	verboseMessage("getMetadataTcga_Name_internal theMethodString=", theMethodString, theVerboseFlag=theVerboseFlag)
	results <- NULL
	jReadGeneObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	results <- .jcall(jReadGeneObj, returnSig = "Ljava/lang/String;", method=theMethodString,
											.jnew("java/lang/String",theId))
	results
}

#################################################################
#################################################################
# used outside file but not exported
#################################################################
#################################################################

getMetadataTcga_Name_Dataset <- function(theId, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getMetadataTcga_Name_internal(theId, theZipFile, 'getMetadataTcga_DatasetName', theVerboseFlag=theVerboseFlag)
}

getMetadataTcga_Name_Disease <- function(theId, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getMetadataTcga_Name_internal(theId, theZipFile, 'getMetadataTcga_DiseaseName', theVerboseFlag=theVerboseFlag)
}

getMetadataTcga_Name_SampleType <- function(theId, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getMetadataTcga_Name_internal(theId, theZipFile, 'getMetadataTcga_SampleTypeName', theVerboseFlag=theVerboseFlag)
}

#################################################################
#################################################################
#################################################################
#################################################################
