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

getMirs_List_internal <- function(theZipFile, theMethodString, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	results <- NULL
	jListGenesObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	results <- .jcall(jListGenesObj, returnSig = "[S", method=theMethodString)
	results
}

getMirs_Metadata_internal <- function(theId, theZipFile, theMethodString, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	result <- NULL
	jReadGeneObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	jObj <- .jcall(jReadGeneObj,
								 returnSig = "[Lorg/mda/bcb/gsaccess/retrieve/MetadataMir;",
								 method=theMethodString,
								 .jnew("java/lang/String",theId))
	if(FALSE==is.jnull(jObj))
	{
		{
			result <- as.vector(unlist(lapply(jObj, function(theObj)
			{
				new("Metadata_Mir",
										theMirId=theObj$mMirId,
										theMimatId=theObj$mMimatId,
										theMirType=theObj$mMirType,
										theChromosome=theObj$mChromosome,
										theLocationStart=theObj$mLocationStart,
										theLocationEnd=theObj$mLocationEnd,
										theStrand=theObj$mStrand,
										theDerivedFrom=theObj$mDerivedFrom)
			})))
		}
	}
	result
}

#################################################################
#################################################################
# exported
#################################################################
#################################################################

####
#### uses mirs from data file
####

getMirs_List_Mir <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getMirs_List_internal(theZipFile, 'getMirList', theVerboseFlag=theVerboseFlag)
}

getMirs_List_Mimat <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getMirs_List_internal(theZipFile, 'getMimatList', theVerboseFlag=theVerboseFlag)
}

getMirs_Metadata_Mir <- function(theMirId, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getMirs_Metadata_internal(theMirId, theZipFile, 'getMetadata_miRNA_mir', theVerboseFlag=theVerboseFlag)
}

getMirs_Metadata_Mimat <- function(theMimatId, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	getMirs_Metadata_internal(theMimatId, theZipFile, 'getMetadata_miRNA_mimat', theVerboseFlag=theVerboseFlag)
}


#################################################################
#################################################################
#################################################################
#################################################################
