#GeneSurvey Copyright 2014, 2015, 2016 University of Texas MD Anderson Cancer Center
#
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.
#
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

#################################################################
#################################################################
#################################################################
#################################################################

readAsGenericDataframe_internal <- function(theGene, theZipFile, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	results <- NULL
	jObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	result <- .jcall(jObj, returnSig = "Lorg/mda/bcb/gsaccess/retrieve/Dataframe;",
									 method="getMutationDetails_Dataframe",
									 .jnew("java/lang/String",theGene))
	if(FALSE==is.jnull(result))
	{
		results <- as.data.frame(result$mDataframe, stringsAsFactors=FALSE)
		names(results) <- result$mColumnHeaders
		results$Position_End <- as.numeric(results$Position_End)
		results$Position_Start <- as.numeric(results$Position_Start)
	}
	results
}

getMutationDetails_Dataframe <- function(theGene, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	#getDataPlatform_internal(theZipFile, 'getDataMatrix_miRNASeqPlatform', theVerboseFlag=theVerboseFlag)
	readAsGenericDataframe_internal(theGene, theZipFile, theVerboseFlag)
}
