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

.onAttach <- function(libname, pkgname)
{
	packageStartupMessage(paste(geneReportVersion()))
}

#################################################################
#################################################################
#################################################################
#################################################################

setClass("Metadata_Mir", representation(
	mMirId="character",
	mMimatId="character",
	mMirType="character",
	mChromosome="character",
	mLocationStart="numeric",
	mLocationEnd="numeric",
	mStrand="character",
	mDerivedFrom="character"))

setMethod("initialize", "Metadata_Mir",
					function(.Object,
									 theMirId,
									 theMimatId,
									 theMirType,
									 theChromosome,
									 theLocationStart,
									 theLocationEnd,
									 theStrand,
									 theDerivedFrom)
					{
						.Object@mMirId <- theMirId
						.Object@mMimatId <- theMimatId
						.Object@mMirType <- theMirType
						.Object@mChromosome <- theChromosome
						.Object@mLocationStart <- theLocationStart
						.Object@mLocationEnd <- theLocationEnd
						.Object@mStrand <- theStrand
						.Object@mDerivedFrom <- theDerivedFrom
						.Object
					})

setGeneric(name="get.MirId", def=function(theObject){standardGeneric("get.MirId")})
setMethod(f="get.MirId",
					definition=function(theObject)
					{
						theObject@mMirId
					})

setGeneric(name="get.MimatId", def=function(theObject){standardGeneric("get.MimatId")})
setMethod(f="get.MimatId",
					definition=function(theObject)
					{
						theObject@mMimatId
					})

setGeneric(name="get.MirType", def=function(theObject){standardGeneric("get.MirType")})
setMethod(f="get.MirType",
					definition=function(theObject)
					{
						theObject@mMirType
					})

setGeneric(name="get.LocationStart", def=function(theObject){standardGeneric("get.LocationStart")})
setMethod(f="get.LocationStart",
					definition=function(theObject)
					{
						theObject@mLocationStart
					})

setGeneric(name="get.LocationEnd", def=function(theObject){standardGeneric("get.LocationEnd")})
setMethod(f="get.LocationEnd",
					definition=function(theObject)
					{
						theObject@mLocationEnd
					})

setGeneric(name="get.Chromosome", def=function(theObject){standardGeneric("get.Chromosome")})
setMethod(f="get.Chromosome",
					definition=function(theObject)
					{
						theObject@mChromosome
					})

setGeneric(name="get.Strand", def=function(theObject){standardGeneric("get.Strand")})
setMethod(f="get.Strand",
					definition=function(theObject)
					{
						theObject@mStrand
					})

setGeneric(name="get.DerivedFrom", def=function(theObject){standardGeneric("get.DerivedFrom")})
setMethod(f="get.DerivedFrom",
					definition=function(theObject)
					{
						theObject@mDerivedFrom
					})

#################################################################
#################################################################
#################################################################
#################################################################

setClass("Metadata_Gene", representation(
	mGeneSymbol="character",
	mGeneId="character",
	mVersionIndex="character",
	mChromosome="character",
	mLocationStart="numeric",
	mLocationEnd="numeric",
	mStrand="character"))

setMethod("initialize", "Metadata_Gene",
					function(.Object,
									 theGeneSymbol,
									 theGeneId,
									 theVersionIndex,
									 theChromosome,
									 theLocationStart,
									 theLocationEnd,
									 theStrand)
					{
						.Object@mGeneSymbol <- theGeneSymbol
						.Object@mGeneId <- theGeneId
						.Object@mVersionIndex <- theVersionIndex
						.Object@mChromosome <- theChromosome
						.Object@mLocationStart <- theLocationStart
						.Object@mLocationEnd <- theLocationEnd
						.Object@mStrand <- theStrand
						.Object
					})

setGeneric(name="get.GeneSymbol", def=function(theObject){standardGeneric("get.GeneSymbol")})
setMethod(f="get.GeneSymbol",
					definition=function(theObject)
					{
						theObject@mGeneSymbol
					})

setGeneric(name="get.GeneId", def=function(theObject){standardGeneric("get.GeneId")})
setMethod(f="get.GeneId",
					definition=function(theObject)
					{
						theObject@mGeneId
					})

setGeneric(name="get.VersionIndex", def=function(theObject){standardGeneric("get.VersionIndex")})
setMethod(f="get.VersionIndex",
					definition=function(theObject)
					{
						theObject@mVersionIndex
					})

setGeneric(name="get.LocationStart", def=function(theObject){standardGeneric("get.LocationStart")})
setMethod(f="get.LocationStart",
					definition=function(theObject)
					{
						theObject@mLocationStart
					})

setGeneric(name="get.LocationEnd", def=function(theObject){standardGeneric("get.LocationEnd")})
setMethod(f="get.LocationEnd",
					definition=function(theObject)
					{
						theObject@mLocationEnd
					})

setGeneric(name="get.Chromosome", def=function(theObject){standardGeneric("get.Chromosome")})
setMethod(f="get.Chromosome",
					definition=function(theObject)
					{
						theObject@mChromosome
					})

setGeneric(name="get.Strand", def=function(theObject){standardGeneric("get.Strand")})
setMethod(f="get.Strand",
					definition=function(theObject)
					{
						theObject@mStrand
					})

#################################################################
#################################################################
#################################################################
#################################################################

setClass("Metadata_Probe", representation(
	mName="character",
	mChromosome="character",
	mProbeLocation="numeric",
	mStrand="character",
	mGeneStructure="vector"))

setMethod("initialize", "Metadata_Probe",
					function(.Object,
									 theName,
									 theChromosome,
									 theProbeLocation,
									 theStrand,
									 theGeneStructure)
					{
						.Object@mName <- theName
						.Object@mChromosome <- theChromosome
						.Object@mProbeLocation <- theProbeLocation
						.Object@mStrand <- theStrand
						.Object@mGeneStructure <- theGeneStructure
						.Object
					})

setGeneric(name="get.Name", def=function(theObject){standardGeneric("get.Name")})
setMethod(f="get.Name",
					definition=function(theObject)
					{
						theObject@mName
					})

setGeneric(name="get.Chromosome", def=function(theObject){standardGeneric("get.Chromosome")})
setMethod(f="get.Chromosome",
					definition=function(theObject)
					{
						theObject@mChromosome
					})

setGeneric(name="get.ProbeLocation", def=function(theObject){standardGeneric("get.ProbeLocation")})
setMethod(f="get.ProbeLocation",
					definition=function(theObject)
					{
						theObject@mProbeLocation
					})

setGeneric(name="get.Strand", def=function(theObject){standardGeneric("get.Strand")})
setMethod(f="get.Strand",
					definition=function(theObject)
					{
						theObject@mStrand
					})

setGeneric(name="get.GeneStructure", def=function(theObject){standardGeneric("get.GeneStructure")})
setMethod(f="get.GeneStructure",
					definition=function(theObject)
					{
						theObject@mGeneStructure
					})

#################################################################
#################################################################
#################################################################
#################################################################

# myMatrix <- matrix(data=c(0,0,0,0,20,0,0,0,30,10,0,0,10,10,20,0), byrow=TRUE, ncol=4, nrow=4, dimnames=list(c("Bob", "Ted", "Carol", "Alice"),c("Bob", "Ted", "Carol", "Alice")))
# new("ReadGene_Item", NULL, "foo", myMatrix, NULL)

#setClassUnion("VectorOrNULL", c("vector", "NULL"))
#setClassUnion("CharacterOrNULL", c("character", "NULL"))
#setClassUnion("ReadProbeOrNULL", c("ReadProbe", "NULL"))
#setClassUnion("ReadGenomeOrNULL", c("ReadGenome", "NULL"))
#setClassUnion("ReadGenomeOrNULL", c("NULL"))

# mData -- matrix samples in rows, probes in columns, with row and column names
setClass("GeneReportDataObject", representation(
	mData="matrix",
	mSampleAnnotations="data.frame",
	mGenomeAnnotations="data.frame",
	mTissueAnnotations="data.frame"))

setMethod("initialize", "GeneReportDataObject",
					function(.Object,
									 theData,
									 theSampleAnnotations,
									 theGenomeAnnotations,
									 theTissueAnnotations)
					{
						.Object@mData <- theData
						.Object@mSampleAnnotations <- theSampleAnnotations
						.Object@mGenomeAnnotations <- theGenomeAnnotations
						.Object@mTissueAnnotations <- theTissueAnnotations
						.Object
					})


setGeneric(name="get.Data", def=function(theObject){standardGeneric("get.Data")})
setMethod(f="get.Data",
					definition=function(theObject)
					{
						theObject@mData
					})

setGeneric(name="get.SampleAnnotations", def=function(theObject){standardGeneric("get.SampleAnnotations")})
setMethod(f="get.SampleAnnotations",
					definition=function(theObject)
					{
						theObject@mSampleAnnotations
					})

setGeneric(name="get.GenomeAnnotations", def=function(theObject){standardGeneric("get.GenomeAnnotations")})
setMethod(f="get.GenomeAnnotations",
					definition=function(theObject)
					{
						theObject@mGenomeAnnotations
					})

setGeneric(name="get.TissueAnnotations", def=function(theObject){standardGeneric("get.TissueAnnotations")})
setMethod(f="get.TissueAnnotations",
					definition=function(theObject)
					{
						theObject@mTissueAnnotations
					})

#################################################################
#################################################################
#################################################################
#################################################################
