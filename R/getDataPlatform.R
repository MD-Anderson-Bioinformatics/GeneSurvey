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

#readAsGenericMatrix <- function(theZipFile, theMethodString, theVerboseFlag)
#{
#	setJavaVerboseFlag(theVerboseFlag)
#	results <- NULL
#	jObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
#	result <- .jcall(jObj, returnSig = "Lorg/mda/bcb/gsaccess/retrieve/GetMatrixPlatform;", method=theMethodString)
#	if(FALSE==is.jnull(result))
#	{
#		results <- matrixWithIssues(result$mGenesBySamplesValues, nrow=length(result$mGenes))
#		colnames(results) <- result$mSamples
#		rownames(results) <- result$mGenes
#	}
#	results
#}

getDataPlatform_internal <- function(theZipFile, theInternalFile)
{
#	platformData <- readAsGenericMatrix(theZipFile, theMethodString, theVerboseFlag)
#	platformData
	results <- NULL
	if(require(ReadMatrixRcpp, warn.conflicts=FALSE))
	{
		results <- ReadMatrixRcpp::readMatrixFromZip(theZipFile, theInternalFile)
	}
	else
	{
		message("Package ReadMatrixRcpp is not available, so NULL is being returned for the data request")
	}
	results
}

getDataPlatform_GeneSymbol_Mutations <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	#getDataPlatform_internal(theZipFile, 'getDataMatrix_MutationsPlatform', theVerboseFlag=theVerboseFlag)
	getDataPlatform_internal(theZipFile, "combined/mutations/platform.tsv")
}

getDataPlatform_GeneSymbol_RnaSeq2 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	#getDataPlatform_internal(theZipFile, 'getDataMatrix_RnaSeq2Platform', theVerboseFlag=theVerboseFlag)
	getDataPlatform_internal(theZipFile, "combined/illuminahiseq_rnaseqv2_gene/platform.tsv")
}

getDataPlatform_GeneSymbol_RnaSeq <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	#getDataPlatform_internal(theZipFile, 'getDataMatrix_RnaSeqPlatform', theVerboseFlag=theVerboseFlag)
	getDataPlatform_internal(theZipFile, "combined/illuminahiseq_rnaseq_uncGeneRPKM/platform.tsv")
}

getDataPlatform_GeneSymbol_SNP6 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	#getDataPlatform_internal(theZipFile, 'getDataMatrix_SNP6Platform', theVerboseFlag=theVerboseFlag)
	getDataPlatform_internal(theZipFile, "combined/genome_wide_snp_6_hg19nocnvWxy/platform.tsv")
}

getDataPlatform_Probe_Meth450 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	#getDataPlatform_internal(theZipFile, 'getDataMatrix_Meth450Platform', theVerboseFlag=theVerboseFlag)
	getDataPlatform_internal(theZipFile, "combined/humanmethylation450_level3/platform.tsv")
}

getDataPlatform_Probe_Meth27 <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	#getDataPlatform_internal(theZipFile, 'getDataMatrix_Meth27Platform', theVerboseFlag=theVerboseFlag)
	getDataPlatform_internal(theZipFile, "combined/humanmethylation27_hg19Wxy/platform.tsv")
}

getDataPlatform_CombinedHsaMimat_miRNASeq <- function(theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	#getDataPlatform_internal(theZipFile, 'getDataMatrix_miRNASeqPlatform', theVerboseFlag=theVerboseFlag)
	getDataPlatform_internal(theZipFile, "combined/illuminahiseq_mirnaseq_isoform/platform.tsv")
}
