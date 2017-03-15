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

getNames_internal <- function(theZipFile, theMethodString, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	jListGenesObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	results <- .jcall(jListGenesObj, returnSig = "[S", method=theMethodString)
	if(TRUE==is.jnull(results))
	{
		results <- NULL;
	}
	results
}

getNamesFromMapping_internal <- function(theZipFile, theMethodString, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	jReadGeneObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	result <- .jcall(jReadGeneObj, returnSig = "[S", method=theMethodString)
	if(TRUE==is.jnull(result))
	{
		result <- NULL
	}
	else
	{
		result <- unique(sort(result))
	}
	result
}

#################################################################
#################################################################
# exported
#################################################################
#################################################################

####
#### uses gene equivalent from data file
####

getNames_GeneSymbol_Mutations <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNames_internal(theZipFile, 'getNames_Mutations', theVerboseFlag=theVerboseFlag)
}

getNames_GeneSymbol_RnaSeq2 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNames_internal(theZipFile, 'getNames_RnaSeq2', theVerboseFlag=theVerboseFlag)
}

getNames_GeneSymbol_RnaSeq <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNames_internal(theZipFile, 'getNames_RnaSeq', theVerboseFlag=theVerboseFlag)
}

getNames_GeneSymbol_SNP6 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNames_internal(theZipFile, 'getNames_SNP6', theVerboseFlag=theVerboseFlag)
}

getNames_Probe_Meth450 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNames_internal(theZipFile, 'getNames_Meth450', theVerboseFlag=theVerboseFlag)
}

getNames_Probe_Meth27 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNames_internal(theZipFile, 'getNames_Meth27', theVerboseFlag=theVerboseFlag)
}

getNames_CombinedHsaMimat_miRNASeq <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNames_internal(theZipFile, 'getNames_miRNASeq', theVerboseFlag=theVerboseFlag)
}

getNames_Mutation_Details <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNames_internal(theZipFile, 'getMutationDetails_GeneList', theVerboseFlag=theVerboseFlag)
}

####
#### uses mapping functions
####

getNames_GeneSymbol_Meth450 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNamesFromMapping_internal(theZipFile, 'getMappingGeneSymbols_Meth450', theVerboseFlag=theVerboseFlag)
}

getNames_GeneSymbol_Meth27 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getNamesFromMapping_internal(theZipFile, 'getMappingGeneSymbols_Meth27', theVerboseFlag=theVerboseFlag)
}

#################################################################
#################################################################
#################################################################
#################################################################
