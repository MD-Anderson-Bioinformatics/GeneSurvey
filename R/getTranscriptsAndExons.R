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

getExons_GeneEq_internal <- function(theGeneEq, theZipFile, theMethodString, theVerboseFlag)
{
  message(theGeneEq)
  message(theZipFile)
  message(theMethodString)
  setJavaVerboseFlag(theVerboseFlag)
  results <- NULL
  jObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
  result <- .jcall(jObj, returnSig = "[Lorg/mda/bcb/gsaccess/retrieve/Exon;", method=theMethodString,
                   .jnew("java/lang/String",theGeneEq), TRUE)
  if(FALSE==is.jnull(result))
  {
    results <- data.frame(
      gene=sapply(result, function(theObj)
      {
        theObj$mGene
      }, simplify=TRUE, USE.NAMES=FALSE),
      chromosome=sapply(result, function(theObj)
      {
        theObj$mChromosome
      }, simplify=TRUE, USE.NAMES=FALSE),
      start=as.numeric(sapply(result, function(theObj)
      {
        theObj$mStart
      }, simplify=TRUE, USE.NAMES=FALSE)),
      end=as.numeric(sapply(result, function(theObj)
      {
        theObj$mEnd
      }, simplify=TRUE, USE.NAMES=FALSE)),
      strand=sapply(result, function(theObj)
      {
        theObj$mStrand
      }, simplify=TRUE, USE.NAMES=FALSE),
      exon_type=sapply(result, function(theObj)
      {
        theObj$mExonType
      }, simplify=TRUE, USE.NAMES=FALSE),
      exon_id=sapply(result, function(theObj)
      {
        theObj$mExonId
      }, simplify=TRUE, USE.NAMES=FALSE),
      exon_number=sapply(result, function(theObj)
      {
        theObj$mExonNumber
      }, simplify=TRUE, USE.NAMES=FALSE),
      transcript_id=sapply(result, function(theObj)
      {
        theObj$mTranscriptId
      }, simplify=TRUE, USE.NAMES=FALSE),
      transcript_symbol=sapply(result, function(theObj)
      {
        theObj$mTranscriptSymbol
      }, simplify=TRUE, USE.NAMES=FALSE),
      stringsAsFactors=FALSE)
  }
  results
}

getTranscripts_GeneEq_internal <- function(theGeneEq, theZipFile, theMethodString, theVerboseFlag)
{
  setJavaVerboseFlag(theVerboseFlag)
  results <- NULL
  jObj <- .jnew("org/mda/bcb/gsaccess/CallFromR", theZipFile)
  result <- .jcall(jObj, returnSig = "[Lorg/mda/bcb/gsaccess/retrieve/Transcript;", method=theMethodString,
                   .jnew("java/lang/String",theGeneEq), TRUE)
  if(FALSE==is.jnull(result))
  {
    results <- data.frame(
      gene=sapply(result, function(theObj)
      {
        theObj$mGene
      }, simplify=TRUE, USE.NAMES=FALSE),
      chromosome=sapply(result, function(theObj)
      {
        theObj$mChromosome
      }, simplify=TRUE, USE.NAMES=FALSE),
      start=as.numeric(sapply(result, function(theObj)
      {
        theObj$mStart
      }, simplify=TRUE, USE.NAMES=FALSE)),
      end=as.numeric(sapply(result, function(theObj)
      {
        theObj$mEnd
      }, simplify=TRUE, USE.NAMES=FALSE)),
      strand=sapply(result, function(theObj)
      {
        theObj$mStrand
      }, simplify=TRUE, USE.NAMES=FALSE),
      transcript_type=sapply(result, function(theObj)
      {
        theObj$mTranscriptType
      }, simplify=TRUE, USE.NAMES=FALSE),
      transcript_id=sapply(result, function(theObj)
      {
        theObj$mTranscriptId
      }, simplify=TRUE, USE.NAMES=FALSE),
      transcript_symbol=sapply(result, function(theObj)
      {
        theObj$mTranscriptSymbol
      }, simplify=TRUE, USE.NAMES=FALSE),
      stringsAsFactors=FALSE)
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

getExons_GeneSymbol_HG19 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
  stopifnot(is.character(theGeneEq))
  stopifnot(file.exists(theZipFile))
  stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
  getExons_GeneEq_internal(theGeneEq, theZipFile, 'getExons_HG19', theVerboseFlag=theVerboseFlag)
}

getTranscripts_GeneSymbol_HG19 <- function(theGeneEq, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
  stopifnot(is.character(theGeneEq))
  stopifnot(file.exists(theZipFile))
  stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
  getTranscripts_GeneEq_internal(theGeneEq, theZipFile, 'getTranscripts_HG19', theVerboseFlag=theVerboseFlag)
}
