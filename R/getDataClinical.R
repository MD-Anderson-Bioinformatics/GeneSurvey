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

getDataClinical_internal <- function(theZipFile, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	results <- NULL
	jObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	result <- .jcall(jObj, returnSig = "Lorg/mda/bcb/gsaccess/retrieve/GetDataClinical;",
									 method="getDataClinical")
	if(FALSE==is.jnull(result))
	{
		results <- matrixWithIssues(result$mGenesBySamplesValues, ncol=length(result$mColumnLabels))
		rownames(results) <- result$mPatientIds
		colnames(results) <- result$mColumnLabels
	}
	results
}

#################################################################
#################################################################
# exported
#################################################################
#################################################################

getDataClinical <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	getDataClinical_internal(theZipFile, theVerboseFlag=theVerboseFlag)
}

#################################################################
#################################################################
#################################################################
#################################################################
