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

getData_GeneEq_internal <- function(theGeneEq, theZipFile, theRemoveDupFlag, theMethodString,
																		theDeltaFunction, theVerboseFlag)
{
	readMatrix_internal(theGeneEq, theZipFile, theRemoveDupFlag, theMethodString, theVerboseFlag, theDeltaFunction)
}

#################################################################
#################################################################
# exported
#################################################################
#################################################################

####
#### uses gene equivalent from data file
####

getData_GeneSymbol_RnaSeq2 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																			 theUseDeltaFlag=FALSE, theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theUseDeltaFlag)||(FALSE==theUseDeltaFlag))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	deltaFunction <- NULL
	if(TRUE==theUseDeltaFlag)
	{
		deltaFunction <- getDelta_RnaSeq2
	}
	# getData_RnaSeq2 -> getDataMatrix_RnaSeq2
	getData_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getDataMatrix_RnaSeq2',
													deltaFunction, theVerboseFlag=theVerboseFlag)
}

getData_GeneSymbol_RnaSeq <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																			theUseDeltaFlag=FALSE, theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theUseDeltaFlag)||(FALSE==theUseDeltaFlag))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	deltaFunction <- NULL
	if(TRUE==theUseDeltaFlag)
	{
		deltaFunction <- getDelta_RnaSeq
	}
	# getData_RnaSeq -> getDataMatrix_RnaSeq
	getData_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getDataMatrix_RnaSeq',
													deltaFunction, theVerboseFlag=theVerboseFlag)
}

getData_GeneSymbol_Mutations <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																				 theUseDeltaFlag=FALSE, theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	deltaFunction <- NULL
	if(TRUE==theUseDeltaFlag)
	{
		deltaFunction <- getDelta_Mutations
	}
	getData_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getDataMatrix_Mutations',
													deltaFunction, theVerboseFlag=FALSE)
}

getData_GeneSymbol_MutationDetails <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																				 theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	loadGeneMutationDetails(theGeneEq, theZipFile, theVerboseFlag=FALSE)
}

getData_GeneSymbol_SNP6 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
															 theUseDeltaFlag=FALSE, theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theUseDeltaFlag)||(FALSE==theUseDeltaFlag))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	deltaFunction <- NULL
	if(TRUE==theUseDeltaFlag)
	{
		deltaFunction <- getDelta_SNP6
	}
	getData_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getDataMatrix_SNP6',
													deltaFunction, theVerboseFlag=theVerboseFlag)
}

getData_Probe_Meth450 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																	theUseDeltaFlag=FALSE, theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theUseDeltaFlag)||(FALSE==theUseDeltaFlag))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	deltaFunction <- NULL
	if(TRUE==theUseDeltaFlag)
	{
		deltaFunction <- getDelta_Meth450
	}
	getData_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getDataMatrix_Meth450',
													deltaFunction, theVerboseFlag=theVerboseFlag)
}

getData_Probe_Meth27 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																 theUseDeltaFlag=FALSE, theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theUseDeltaFlag)||(FALSE==theUseDeltaFlag))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	deltaFunction <- NULL
	if(TRUE==theUseDeltaFlag)
	{
		deltaFunction <- getDelta_Meth27
	}
	# getData_Meth27 -> getDataMatrix_Meth27
	getData_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getDataMatrix_Meth27',
													deltaFunction, theVerboseFlag=theVerboseFlag)
}

getData_CombinedHsaMimat_miRNASeq <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip",
																							theUseDeltaFlag=FALSE, theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(is.character(theGeneEq))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theUseDeltaFlag)||(FALSE==theUseDeltaFlag))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	deltaFunction <- NULL
	if(TRUE==theUseDeltaFlag)
	{
		deltaFunction <- getDelta_miRNASeq
	}
	# getData_miRNASeq -> getDataMatrix_miRNASeq
	getData_GeneEq_internal(theGeneEq, theZipFile, theRemoveDupFlag, 'getDataMatrix_miRNASeq',
													deltaFunction, theVerboseFlag=theVerboseFlag)
}

####
#### uses mapping functions
####

getDataMatrix_GeneSymbol_Meth450 <- function(theGene, theZipFile="/geneSurveyData/GeneSurvey.zip",
																						 theRemoveDupFlag=TRUE, theVerboseFlag=FALSE, theUseDeltaFlag=FALSE)
{
	stopifnot(is.character(theGene))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theUseDeltaFlag)||(FALSE==theUseDeltaFlag))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	dataObj <- getDataObject_GeneSymbol_Meth450(theGene, theZipFile, theRemoveDupFlag, theVerboseFlag, NULL, theUseDeltaFlag=theUseDeltaFlag)
	get.Data(dataObj)
}

getDataMatrix_GeneSymbol_Meth27 <- function(theGene, theZipFile="/geneSurveyData/GeneSurvey.zip",
																						theRemoveDupFlag=TRUE, theVerboseFlag=FALSE, theUseDeltaFlag=FALSE)
{
	stopifnot(is.character(theGene))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theUseDeltaFlag)||(FALSE==theUseDeltaFlag))
	stopifnot((TRUE==theRemoveDupFlag)||(FALSE==theRemoveDupFlag))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	dataObj <- getDataObject_GeneSymbol_Meth27(theGene, theZipFile, theRemoveDupFlag, theVerboseFlag, NULL, theUseDeltaFlag=theUseDeltaFlag)
	get.Data(dataObj)
}

#################################################################
#################################################################
#################################################################
#################################################################
