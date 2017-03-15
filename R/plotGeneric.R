#GeneSurvey Copyright 2014, 2015, 2016 University of Texas MD Anderson Cancer Center
#
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.
#
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

plotGenericOutput <- function(theGeneEq, theOutputDir, myGeneData, myBarcodeDiseases, myBarcodeSampleType,
															theDataType, theDataTypeLabel, theVerboseFlag, theTag, theGeneEqPre="",
															theReplicateFlag=FALSE, theExtraTitle="")
{
	dir.create(theOutputDir, recursive = TRUE, showWarnings=FALSE)
	returnFilename <- c()
	verboseMessage("plotGenericOutput start", theVerboseFlag=theVerboseFlag)
	if (is.matrix(myGeneData))
	{
		verboseMessage("plotGenericOutput convert matrix to vector", theVerboseFlag=theVerboseFlag)
		myColNames <- colnames(myGeneData)
		myGeneData <- as.vector(myGeneData)
		names(myGeneData) <- myColNames
	}
	verboseMessage("theOutputDir = ", theOutputDir, theVerboseFlag=theVerboseFlag)
	verboseMessage("theDataType = ", theDataType, theVerboseFlag=theVerboseFlag)
	verboseMessage("theDataTypeLabel = ", theDataTypeLabel, theVerboseFlag=theVerboseFlag)
	verboseMessage("original data points = ", length(myGeneData), theVerboseFlag=theVerboseFlag)
	verboseMessage("original disease points = ", length(myBarcodeDiseases), theVerboseFlag=theVerboseFlag)
	verboseMessage("original sample type points = ", length(myBarcodeSampleType), theVerboseFlag=theVerboseFlag)
	verboseMessage("original number of diseases (myBarcodeDiseases) = ", length(unique(sort(myBarcodeDiseases))), theVerboseFlag=theVerboseFlag)
	verboseMessage(paste(unique(sort(myBarcodeDiseases)), sep="", coll=", "), theVerboseFlag=theVerboseFlag)
	verboseMessage("original number of sample types = ", length(unique(sort(myBarcodeSampleType))), theVerboseFlag=theVerboseFlag)
	verboseMessage(paste(unique(sort(myBarcodeSampleType)), sep="", coll=", "), theVerboseFlag=theVerboseFlag)
	####
	myGeneData <- myGeneData[names(myGeneData) %in% names(myBarcodeDiseases)]
	myGeneData <- myGeneData[names(myGeneData) %in% names(myBarcodeSampleType)]
	myGeneData <- myGeneData[order(names(myGeneData))]
	myBarcodeDiseases <- myBarcodeDiseases[names(myBarcodeDiseases) %in% names(myGeneData)]
	myBarcodeDiseases <- myBarcodeDiseases[order(names(myBarcodeDiseases))]
	myBarcodeSampleType <- myBarcodeSampleType[names(myBarcodeSampleType) %in% names(myGeneData)]
	myBarcodeSampleType <- myBarcodeSampleType[order(names(myBarcodeSampleType))]
	verboseMessage("filtered data", theVerboseFlag=theVerboseFlag)
	verboseMessage("filtered data points = ", length(myGeneData), theVerboseFlag=theVerboseFlag)
	verboseMessage("filtered disease points = ", length(myBarcodeDiseases), theVerboseFlag=theVerboseFlag)
	verboseMessage("filtered sample type points = ", length(myGeneData), theVerboseFlag=theVerboseFlag)
	verboseMessage("filtered number of diseases = ", length(unique(sort(myBarcodeDiseases))), theVerboseFlag=theVerboseFlag)
	verboseMessage(paste(unique(sort(myBarcodeDiseases)), sep="", coll=", "), theVerboseFlag=theVerboseFlag)
	verboseMessage("filtered number of sample types = ", length(unique(sort(myBarcodeSampleType))), theVerboseFlag=theVerboseFlag)
	verboseMessage(paste(unique(sort(myBarcodeSampleType)), sep="", coll=", "), theVerboseFlag=theVerboseFlag)
	####
	verboseMessage("get colors", theVerboseFlag=theVerboseFlag)
	tcgaColors <- getColorsForDiseases(unique(sort(myBarcodeDiseases)))
	####
	ylab <- theDataTypeLabel
	main <- paste(theGeneEqPre, theGeneEq," : ", theDataType,sep="")
	filesub <- paste(theGeneEqPre, theGeneEq,"_",theDataType,sep="")
	verboseMessage("filesub = ", filesub, theVerboseFlag=theVerboseFlag)
	if (""!=theTag)
	{
		main <- paste(main, theExtraTitle, " : ", theTag,sep="")
		filesub <- paste(filesub, "_", theTag,sep="")
	}
	####
	verboseMessage("stripchartAcrossDiseases", theVerboseFlag=theVerboseFlag)
	returnFilename <- c(returnFilename,
											stripchartAcrossDiseases(myGeneData, myBarcodeDiseases, myBarcodeSampleType,
																							 tcgaColors, ylab, main, theOutputDir, filesub, theVerboseFlag=theVerboseFlag))
	####
	if(TRUE==theReplicateFlag)
	{
		verboseMessage("replicateValues", theVerboseFlag=theVerboseFlag)
		returnFilename <- c(returnFilename,
												replicateValues(myGeneData, myBarcodeDiseases, myBarcodeSampleType, tcgaColors,
																				ylab, main, theOutputDir, filesub, theVerboseFlag=theVerboseFlag))
	}
	verboseMessage("plotGenericOutput finished", theVerboseFlag=theVerboseFlag)
	returnFilename
}


replicateValues <- function(theGeneData, theBarcodeDiseases, theBarcodeSampleType, theDiseaseColors,
														theLabel, theMainLabel, theOutputDir, theFilenameBase, theVerboseFlag=theVerboseFlag)
{
	verboseMessage("replicateValues start", theVerboseFlag=theVerboseFlag)
	returnFilename <- c()
	if (length(theGeneData) > 0)
	{
		verboseMessage("replicateValues get barcodeSamples", theVerboseFlag=theVerboseFlag)
		barcodeSamples <- unlist(lapply(names(theBarcodeDiseases), function(theValue)
		{
			substr(theValue,1,15)
		}))
		verboseMessage("replicateValues get duplicatedBarcodeSamples", theVerboseFlag=theVerboseFlag)
		duplicatedBarcodeSamples <- barcodeSamples[duplicated(barcodeSamples)]
		if (length(duplicatedBarcodeSamples)>0)
		{
			minVals <- c()
			maxVals <- c()
			for(duplicateValue in duplicatedBarcodeSamples)
			{
				valueList <- theGeneData[which(barcodeSamples==duplicateValue)]
				if (length(valueList)>1)
				{
					minVals <- c(minVals, min(valueList))
					maxVals <- c(maxVals, max(valueList))
				}
			}
			if( (sum(!is.nan(minVals))>0) && (sum(!is.nan(maxVals))>0))
			{
				filename <- file.path(theOutputDir, compressIntoFilename(paste(theFilenameBase, "_Replicate.PNG", sep="")))
				returnFilename <- filename
				verboseMessage("internalDrawReplicateValues start", theVerboseFlag=theVerboseFlag)
				CairoPNG(filename=filename, width = 400, height = 400, pointsize=12)
				on.exit(dev.off(), add = TRUE)
				#op <- par(no.readonly = TRUE)
				#on.exit(par(op), add = TRUE)
				#par(mar=c(15,5,5,5))
				plot(minVals,
						 maxVals,
						 xlab="min values",
						 ylab="max values",
						 main=paste(theMainLabel, "\n", theLabel, " (N=", length(minVals), ")", sep=""))
				abline(0,1,lty=2)
				#stripchart(theValues ~ theSamples, vertical=TRUE, srt=90, las=2, ylab=theLabel, main=theMainLabel)
			}
			else
			{
				message("internalDrawReplicateValues all NaN skipping ", compressIntoFilename(paste(theFilenameBase, "_Replicate.PNG", sep="")))
			}
			verboseMessage("internalDrawReplicateValues finish", theVerboseFlag=theVerboseFlag)
		}
	}
	verboseMessage("replicateValues finished", theVerboseFlag=theVerboseFlag)
	returnFilename
}

stripchartAcrossDiseases <- function(theGeneData, theBarcodeDiseases, theBarcodeSampleType, theDiseaseColors,
																		 theLabel, theMainLabel, theOutputDir, theFilenameBase, theVerboseFlag)
{
	returnFilename <- c()
	verboseMessage("stripchartAcrossDiseases started", theVerboseFlag=theVerboseFlag)
	if ((length(theGeneData) > 0) && (sum(!is.nan(theGeneData))>0))
	{
		verboseMessage("stripchartAcrossDiseases has data", theVerboseFlag=theVerboseFlag)
		filename <- file.path(theOutputDir, compressIntoFilename(paste(theFilenameBase, "_Stripchart.PNG", sep="")))
		returnFilename <- filename
		verboseMessage("filename = ", filename, theVerboseFlag=theVerboseFlag)
		CairoPNG(filename=filename, width = 2000, height = 1000, pointsize=24)
		verboseMessage("stripchartAcrossDiseases filename=", filename, theVerboseFlag=theVerboseFlag)
		###defaultPar <- par(no.readonly = TRUE)
		on.exit(dev.off(), add = TRUE)
		groups <- as.factor(as.vector(theBarcodeDiseases))
		groupTable <- table(groups)
		groupTable <- groupTable[order(names(groupTable))]
		groupNames <- unlist(lapply(names(groupTable), function(theEntry, theBarcodeDiseases, theGeneData)
		{
			barcodeList <- names(which(theBarcodeDiseases==theEntry))
			values <- theGeneData[barcodeList]
			value <- paste(theEntry, " (", sum(!is.na(values)), "/", length(values), ")", sep="")
			value
		}, theBarcodeDiseases=theBarcodeDiseases, theGeneData=theGeneData))
		# initial strip chart
		theMainLabel <- paste(theMainLabel, " (N=", sum(!is.na(theGeneData)), "/", length(theGeneData), ")", sep="", collapse="")
		stripchart(theGeneData ~ groups, col=theDiseaseColors, pch=1,
							 method="jitter", jitter=0.3, vertical=TRUE, srt=90, las=2, ylab=theLabel, main=theMainLabel,
							 cex.axis=0.6,
							 group.names=groupNames)
		# add others
		for( sampleType in c("02", "04", "40"))
		{
			stripchart(theGeneData[which(theBarcodeSampleType==sampleType)] ~ groups[which(theBarcodeSampleType==sampleType)],
								 col="black", pch="+",
								 method="jitter", jitter=0.3, add=TRUE, vertical=TRUE, srt=90, las=2, ylab=theLabel)
		}
		for( sampleType in c("06", "07"))
		{
			stripchart(theGeneData[which(theBarcodeSampleType==sampleType)] ~ groups[which(theBarcodeSampleType==sampleType)],
								 col="black", pch="*",
								 method="jitter", jitter=0.3, add=TRUE, vertical=TRUE, srt=90, las=2, ylab=theLabel)
		}
		for( sampleType in c("10", "11", "12", "14"))
		{
			stripchart(theGeneData[which(theBarcodeSampleType==sampleType)] ~ groups[which(theBarcodeSampleType==sampleType)],
								 col="black", pch=".",
								 method="jitter", jitter=0.3, add=TRUE, vertical=TRUE, srt=90, las=2, ylab=theLabel)
		}
	}
	else
	{
		message("stripchartAcrossDiseases skipping zero length or NaN ", compressIntoFilename(paste(theFilenameBase, "_Stripchart.PNG", sep="")))
	}
	verboseMessage("stripchartAcrossDiseases finished", theVerboseFlag=theVerboseFlag)
	returnFilename
}
