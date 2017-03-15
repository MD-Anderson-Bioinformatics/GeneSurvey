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

getMetadata_Probe_JavaToR <- function(theJavaObj)
{
	geneStruct <- theJavaObj$mStructure
	names(geneStruct) <- theJavaObj$mGene
	strandVal <- "unknown"
	if(FALSE==is.jnull(theJavaObj$mStrand))
	{
		strandVal <- theJavaObj$mStrand
	}
	return(new("Metadata_Probe",
						 theName=theJavaObj$mName,
						 theChromosome=theJavaObj$mChromosome,
						 theProbeLocation=as.numeric(theJavaObj$mProbeLocation),
						 theStrand=strandVal,
						 theGeneStructure=geneStruct))
}

getMetadata_Probe_internal <- function(theProbe, theZipFile, theMethodString, theVerboseFlag)
{
	setJavaVerboseFlag(theVerboseFlag)
	# class objects in a vector are always a list, not a vector
	listResults <- lapply(theProbe, function(myProbe)
	{
		results <- NULL
		jObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
		jReadGeneObj <- .jcall(jObj, returnSig = "Lorg/mda/bcb/gsaccess/retrieve/MetadataProbe;", method=theMethodString,
												.jnew("java/lang/String",myProbe))
		if(FALSE==is.jnull(jReadGeneObj))
		{
			results <- getMetadata_Probe_JavaToR(jReadGeneObj)
		}
		results
	})
	names(listResults) <- theProbe
	listResults
}

#################################################################
#################################################################
# exported
#################################################################
#################################################################

####
#### specific probes
####

# TODO: does this take a vector?
getMetadata_Probe_Meth450 <- function(theProbe, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(is.character(theProbe))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	getMetadata_Probe_internal(theProbe, theZipFile, 'getMetadata_Meth450', theVerboseFlag=theVerboseFlag)
}

getMetadata_Probe_Meth27 <- function(theProbe, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	stopifnot(is.character(theProbe))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	getMetadata_Probe_internal(theProbe, theZipFile, 'getMetadata_Meth27', theVerboseFlag=theVerboseFlag)
}

####
#### probes by location
####

getMetadata_ProbeByNeighbor_Meth450 <- function(theStartPosition, theStopPosition, theChromosome,
																	theZipFile="/geneSurveyData/GeneSurvey.zip",
																	theVerboseFlag=FALSE)
{
	stopifnot(is.numeric(theStartPosition))
	stopifnot(is.numeric(theStopPosition))
	stopifnot(is.character(theChromosome))
	stopifnot(file.exists(theZipFile))
	setJavaVerboseFlag(theVerboseFlag)
	results <- NULL
	verboseMessage("getMetadata_ProbeByNeighbor_Meth450 started", theVerboseFlag=theVerboseFlag)
	jFindNeighborObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	resultArray <- .jcall(jFindNeighborObj,
												returnSig = "[Lorg/mda/bcb/gsaccess/retrieve/MetadataProbe;",
												method="findNeighbors_Meth450",
												.jlong(theStartPosition),
												.jlong(theStopPosition),
												.jnew("java/lang/String",theChromosome)
												)
	verboseMessage("getMetadata_ProbeByNeighbor_Meth450 after call", theVerboseFlag=theVerboseFlag)
	if(length(resultArray)>0)
	{
		verboseMessage("getMetadata_ProbeByNeighbor_Meth450 length(resultArray)=", length(resultArray), theVerboseFlag=theVerboseFlag)
		for(jReadGeneObj in resultArray)
		{
			results <- c(results, getMetadata_Probe_JavaToR(jReadGeneObj))
		}
	}
	probeNames <- as.vector(unlist(lapply(results, get.Name)))
	names(results) <- probeNames
	verboseMessage("getMetadata_ProbeByNeighbor_Meth450 finished", theVerboseFlag=theVerboseFlag)
	results
}

getMetadata_ProbeByNeighbor_Meth27 <- function(theStartPosition, theStopPosition, theChromosome,
																 theZipFile="/geneSurveyData/GeneSurvey.zip",
																 theVerboseFlag=FALSE)
{
	stopifnot(is.numeric(theStartPosition))
	stopifnot(is.numeric(theStopPosition))
	stopifnot(is.character(theChromosome))
	stopifnot(file.exists(theZipFile))
	setJavaVerboseFlag(theVerboseFlag)
	results <- NULL
	verboseMessage("getMetadata_ProbeByNeighbor_Meth27 started", theVerboseFlag=theVerboseFlag)
	jFindNeighborObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
	resultArray <- .jcall(jFindNeighborObj,
												returnSig = "[Lorg/mda/bcb/gsaccess/retrieve/MetadataProbe;",
												method="findNeighbors_Meth27",
												.jlong(theStartPosition),
												.jlong(theStopPosition),
												.jnew("java/lang/String",theChromosome))
	verboseMessage("getMetadata_ProbeByNeighbor_Meth27 after call", theVerboseFlag=theVerboseFlag)
	if(length(resultArray)>0)
	{
		verboseMessage("getMetadata_ProbeByNeighbor_Meth27 length(resultArray)=", length(resultArray), theVerboseFlag=theVerboseFlag)
		for(jReadGeneObj in resultArray)
		{
			results <- c(results, getMetadata_Probe_JavaToR(jReadGeneObj))
		}
	}
	probeNames <- as.vector(unlist(lapply(results, get.Name)))
	names(results) <- probeNames
	verboseMessage("getMetadata_ProbeByNeighbor_Meth27 finished", theVerboseFlag=theVerboseFlag)
	results
}
#################################################################
#################################################################
#################################################################
#################################################################
