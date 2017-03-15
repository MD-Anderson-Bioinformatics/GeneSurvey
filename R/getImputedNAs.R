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

getImputedNAs_GeneEq_internal <- function(theGeneEqList, theZipFile, theRemoveDupFlag, theMethodString,
																		theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	results <- NULL
	jObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	result <- .jcall(jObj, returnSig = "Lorg/mda/bcb/gsaccess/retrieve/GetImputedNAsMatrix;", method=theMethodString,
									 .jarray(as.vector(as.character(theGeneEqList))))
	if(FALSE==is.jnull(result))
	{
		results <- matrixWithIssues(result$mGenesBySamplesValues, nrow=length(result$mGenes))
		colnames(results) <- result$mSamples
		rownames(results) <- result$mGenes
		if (theRemoveDupFlag==TRUE)
		{
			rNames <- rownames(results)
			cNames <- colnames(results)[!duplicated(colnames(results))]
			results <- results[,!duplicated(colnames(results))]
			# have to do this as above line removes "matrixness" from matrix with single row
			results <- matrixWithIssues(as.vector(unlist(results)), nrow=length(rNames))
			colnames(results) <- cNames
			rownames(results) <- rNames
		}
	}
	results
}

#################################################################
#################################################################
# exported
#################################################################
#################################################################

####
#### uses gene equivalent from data file
####

getImputedNAs_GeneSymbol_RnaSeq2 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																			 theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	# getImputedNAs_RnaSeq2 -> getImputedNAsMatrix_RnaSeq2
	getImputedNAs_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getImputedNAsMatrix_RnaSeq2',
													theVerboseFlag=theVerboseFlag)
}

getImputedNAs_GeneSymbol_RnaSeq <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																			theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	# getImputedNAs_RnaSeq -> getImputedNAsMatrix_RnaSeq
	getImputedNAs_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getImputedNAsMatrix_RnaSeq',
													theVerboseFlag=theVerboseFlag)
}

getImputedNAs_GeneSymbol_SNP6 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																		theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	# getImputedNAs_SNP6 -> getImputedNAsMatrix_SNP6
	getImputedNAs_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getImputedNAsMatrix_SNP6',
													theVerboseFlag=theVerboseFlag)
}

getImputedNAs_Probe_Meth450 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																	theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	# getImputedNAs_Meth450 -> getImputedNAsMatrix_Meth450
	getImputedNAs_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getImputedNAsMatrix_Meth450',
													theVerboseFlag=theVerboseFlag)
}

getImputedNAs_Probe_Meth27 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																 theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	# getImputedNAs_Meth27 -> getImputedNAsMatrix_Meth27
	getImputedNAs_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getImputedNAsMatrix_Meth27',
													theVerboseFlag=theVerboseFlag)
}

getImputedNAs_CombinedHsaMimat_miRNASeq <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																							theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	# getImputedNAs_miRNASeq -> getImputedNAsMatrix_miRNASeq
	getImputedNAs_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getImputedNAsMatrix_miRNASeq',
													theVerboseFlag=theVerboseFlag)
}

#################################################################
#################################################################
#################################################################
#################################################################
