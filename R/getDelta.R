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

computeDelta_internal <- function(thePatientList, theNormalsMatrix, theTumorsMatrix)
{
	# tumor - normal
	myNRowNames <- rownames(theNormalsMatrix)
	myTRowNames <- rownames(theTumorsMatrix)
	##################################################################
	tMatrix <- theTumorsMatrix[,thePatientList]
	tMatrix <- matrixWithIssues(as.vector(unlist(tMatrix)), nrow=length(myNRowNames))
	colnames(tMatrix) <- thePatientList
	rownames(tMatrix) <- myNRowNames
	##################################################################
	nMatrix <- theNormalsMatrix[,thePatientList]
	# have to do this as above line removes "matrixness" from matrix with single row
	nMatrix <- matrixWithIssues(as.vector(unlist(nMatrix)), nrow=length(myTRowNames))
	colnames(nMatrix) <- thePatientList
	rownames(nMatrix) <- myTRowNames
	##################################################################
	newMatrix <- tMatrix - nMatrix
	# have to do this as above line removes "matrixness" from matrix with single row
	newMatrix <- matrixWithIssues(as.vector(unlist(newMatrix)), nrow=length(myNRowNames))
	colnames(newMatrix) <- thePatientList
	rownames(newMatrix) <- myNRowNames
	newMatrix
}

getDataDelta_internal <- function(data, barcodeSampleTypes, theNormalList, theTumorList)
{
	#
	listOfNormalBarcodes <- names(barcodeSampleTypes[barcodeSampleTypes %in% theNormalList])
	listOfTumorBarcodes <- names(barcodeSampleTypes[barcodeSampleTypes %in% theTumorList])
	listOfNormalBarcodes <- intersect(colnames(data), listOfNormalBarcodes)
	listOfTumorBarcodes <- intersect(colnames(data), listOfTumorBarcodes)
	# rownames
	myRowNames <- rownames(data)
	######################################################
	######################################################
	# get values
	matrixOfNormalValues <- data[,listOfNormalBarcodes]
	# have to do this as above line removes "matrixness" from matrix with single row
	matrixOfNormalValues <- matrixWithIssues(as.vector(unlist(matrixOfNormalValues)), nrow=length(myRowNames))
	colnames(matrixOfNormalValues) <- listOfNormalBarcodes
	rownames(matrixOfNormalValues) <- myRowNames
	######################################################
	matrixOfTumorValues <- data[,listOfTumorBarcodes]
	# have to do this as above line removes "matrixness" from matrix with single row
	matrixOfTumorValues <- matrixWithIssues(as.vector(unlist(matrixOfTumorValues)), nrow=length(myRowNames))
	colnames(matrixOfTumorValues) <- listOfTumorBarcodes
	rownames(matrixOfTumorValues) <- myRowNames
	######################################################
	######################################################
	# shorten to patient only
	listOfNormalBarcodes <- substring(colnames(matrixOfNormalValues), 1, 12)
	listOfTumorBarcodes <- substring(colnames(matrixOfTumorValues), 1, 12)
	colnames(matrixOfNormalValues) <- listOfNormalBarcodes
	colnames(matrixOfTumorValues) <- listOfTumorBarcodes
	# remove duplicates
	listOfNormalBarcodes <- listOfNormalBarcodes[!duplicated(listOfNormalBarcodes)]
	listOfTumorBarcodes <- listOfTumorBarcodes[!duplicated(listOfTumorBarcodes)]
	# find matches between lists
	sharedPatients <- intersect(listOfNormalBarcodes, listOfTumorBarcodes)

	# delta
	deltaValueList <- computeDelta_internal(sharedPatients, matrixOfNormalValues, matrixOfTumorValues)
	deltaValueList
}

#################################################################
#################################################################
# external to function, NOT exported
#################################################################
#################################################################

getDelta_RnaSeq2 <- function(theData, theZipFile="/geneSurveyData/GeneSurvey.zip",
														 theTumorList = c("01", "03", "05", "06", "09"),
														 theNormalList = c("10", "11", "12", "13", "14"),
														 theVerboseFlag=FALSE)
{
	barcodeSampleTypes <- getMetadataPop_BarcodeSamplecode(theZipFile, theVerboseFlag=theVerboseFlag)
	getDataDelta_internal(theData, barcodeSampleTypes, theNormalList, theTumorList)
}

getDelta_RnaSeq <- function(theData, theZipFile="/geneSurveyData/GeneSurvey.zip",
														 theTumorList = c("01", "03", "05", "06", "09"),
														 theNormalList = c("10", "11", "12", "13", "14"),
														 theVerboseFlag=FALSE)
{
	barcodeSampleTypes <- getMetadataPop_BarcodeSamplecode(theZipFile, theVerboseFlag=theVerboseFlag)
	getDataDelta_internal(theData, barcodeSampleTypes, theNormalList, theTumorList)
}

getDelta_Mutations <- function(theData, theZipFile="/geneSurveyData/GeneSurvey.zip",
															 theTumorList = c("01", "03", "05", "06", "09"),
															 theNormalList = c("10", "11", "12", "13", "14"),
															 theVerboseFlag=FALSE)
{
	barcodeSampleTypes <- getMetadataPop_BarcodeSamplecode(theZipFile, theVerboseFlag=theVerboseFlag)
	getDataDelta_internal(theData, barcodeSampleTypes, theNormalList, theTumorList)
}

getDelta_SNP6 <- function(theData, theZipFile="/geneSurveyData/GeneSurvey.zip",
													theTumorList = c("01", "03", "05", "06", "09"),
													theNormalList = c("10", "11", "12", "13", "14"),
													theVerboseFlag=FALSE)
{
	barcodeSampleTypes <- getMetadataPop_BarcodeSamplecode(theZipFile, theVerboseFlag=theVerboseFlag)
	getDataDelta_internal(theData, barcodeSampleTypes, theNormalList, theTumorList)
}

getDelta_Meth450 <- function(theData, theZipFile="/geneSurveyData/GeneSurvey.zip",
														 theTumorList = c("01", "03", "05", "06", "09"),
														 theNormalList = c("10", "11", "12", "13", "14"),
														 theVerboseFlag=FALSE)
{
	barcodeSampleTypes <- getMetadataPop_BarcodeSamplecode(theZipFile, theVerboseFlag=theVerboseFlag)
	getDataDelta_internal(theData, barcodeSampleTypes, theNormalList, theTumorList)
}

getDelta_Meth27 <- function(theData, theZipFile="/geneSurveyData/GeneSurvey.zip",
														theTumorList = c("01", "03", "05", "06", "09"),
														theNormalList = c("10", "11", "12", "13", "14"),
														theVerboseFlag=FALSE)
{
	barcodeSampleTypes <- getMetadataPop_BarcodeSamplecode(theZipFile, theVerboseFlag=theVerboseFlag)
	getDataDelta_internal(theData, barcodeSampleTypes, theNormalList, theTumorList)
}

getDelta_miRNASeq <- function(theData, theZipFile="/geneSurveyData/GeneSurvey.zip",
															theTumorList = c("01", "03", "05", "06", "09"),
															theNormalList = c("10", "11", "12", "13", "14"),
															theVerboseFlag=FALSE)
{
	barcodeSampleTypes <- getMetadataPop_BarcodeSamplecode(theZipFile, theVerboseFlag=theVerboseFlag)
	getDataDelta_internal(theData, barcodeSampleTypes, theNormalList, theTumorList)
}

#################################################################
#################################################################
#################################################################
#################################################################
