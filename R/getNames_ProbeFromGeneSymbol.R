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

getNames_ProbeFromGeneSymbol_internal <- function(theGene, theZipFile, theMethodString, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	listResults <- lapply(theGene, function(myGene)
	{
		results <- NULL
		jReadGeneObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
		result <- .jcall(jReadGeneObj, returnSig = "[S", method=theMethodString,
										 .jnew("java/lang/String",myGene))
		if(is.jnull(result))
		{
			results <- NULL
		}
		if (0==length(results))
		{
			results <- result
		}
		results
	})
	names(listResults) <- theGene
	listResults
}

#################################################################
#################################################################
# exported
#################################################################
#################################################################

getNames_ProbeFromGeneSymbol_Meth450 <- function(theGene, theZipFile="/geneSurveyData/GeneSurvey.zip",
																		theVerboseFlag=FALSE)
{
	getNames_ProbeFromGeneSymbol_internal(theGene, theZipFile, 'getMapping_Meth450', theVerboseFlag=theVerboseFlag)
}

getNames_ProbeFromGeneSymbol_Meth27 <- function(theGene, theZipFile="/geneSurveyData/GeneSurvey.zip",
																	 theVerboseFlag=FALSE)
{
	getNames_ProbeFromGeneSymbol_internal(theGene, theZipFile, 'getMapping_Meth27', theVerboseFlag=theVerboseFlag)
}

#################################################################
#################################################################
#################################################################
#################################################################
