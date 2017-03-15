library(Cairo)

loadGeneMutationDetails <- function(theGene, theZipFile, theVerboseFlag)
{
	getMutationDetails_Dataframe(theGene, theZipFile, theVerboseFlag)
}

rowLabelNumbersFromLabels <- function(theRowLabels)
{
	currentIndex <- 0
	matchedLabels <- c()
	rowNumbers <- c()
	for (label in theRowLabels)
	{
		if (label %in% matchedLabels)
		{
			# use currentIndex
			rowNumbers <- c(rowNumbers, currentIndex)
		}
		else
		{
			# increment currentIndex
			currentIndex <- currentIndex + 1
			# add to matchedLabels
			matchedLabels <- c(matchedLabels, label)
			# add row number with updated currentIndex
			rowNumbers <- c(rowNumbers, currentIndex)
		}
	}
	rowNumbers
}

dividerRowLabels <- function(theRowLabels)
{
	theRowLabels <- as.vector(unlist(lapply(theRowLabels, function(theVal)
	{
		strsplit(theVal, "-", fixed=TRUE)[[1]][1]
	})))
	newRowLabels <- c()
	for(rowLabel in theRowLabels)
	{
		if (rowLabel %in% newRowLabels)
		{
			newRowLabels <- c(newRowLabels, "")
		}
		else
		{
			newRowLabels <- c(newRowLabels, rowLabel)
		}
	}
	newRowLabels
}

simplifyRowLabels <- function(theRowLabels, theRowNumbers)
{
	theRowLabels <- as.vector(unlist(lapply(theRowLabels, function(theVal)
	{
		strsplit(theVal, "-", fixed=TRUE)[[1]][1]
	})))
	combinedLabels <- c()
	for(index in 1:length(theRowNumbers))
	{
		combinedLabels[theRowNumbers[index]] <- theRowLabels[index]
	}
	theRowLabels <- combinedLabels
	newRowLabels <- c()
	firstIndex <- 0
	indexedLabel <- ""
	for(index in 1:length(theRowLabels))
	{
		rowLabel <- theRowLabels[index]
		newRowLabels <- c(newRowLabels, "")
		if (indexedLabel!=rowLabel)
		{
			# first take care of old label
			if (0!=firstIndex)
			{
				newRowLabels[floor((firstIndex+index)/2)] <- indexedLabel
			}
			# then setup for new label
			indexedLabel <- rowLabel
			firstIndex <- index
		}
	}
	newRowLabels[floor((firstIndex+length(theRowLabels))/2)] <- indexedLabel
	newRowLabels
}

makeSilentNonsilent <- function(theTypes)
{
	theTypes[which(theTypes!="Silent")] <- "Nonsilent"
	theTypes
}

averageWithin <- function(theValues, theClosest)
{
	newVector <- c(theValues[1])
	for(nextIndex in 2:length(theValues))
	{
		testValue <- theValues[nextIndex]
		check <- abs(newVector[length(newVector)]-testValue)
		if (check>=theClosest)
		{
			newVector <- c(newVector, testValue)
		}
	}
	newVector
}

combineWithin <- function(theValues, theClosest)
{
	newVector <- c(theValues[1])
	for(nextIndex in 2:length(theValues))
	{
		testValue <- theValues[nextIndex]
		check <- abs(newVector[length(newVector)]-testValue)
		if (check<=theClosest)
		{
			testValue <- newVector[length(newVector)]
		}
		newVector <- c(newVector, testValue)
	}
	newVector
}

# ******************************************************************
# ******************************************************************
# ******************************************************************
# ******************************************************************

getPlotSymbol <- function(thePlotCount)
{
	plotSymbol <- 1
	if ((thePlotCount>0)&&(thePlotCount<=2))
	{
		plotSymbol <- 1
	}
	if (thePlotCount>2)
	{
		plotSymbol <- 16
	}
	plotSymbol
}

plotMutations_dna <- function(theGene, theOutputDirectory, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	sortColumn1 <- "Disease_Type"
	sortColumn2 <- "Variant_Classification"
	geneData <- getMetadata_Gene_Mutations(theGene, theZipFile, theVerboseFlag)[[1]][[1]]
	plotFile <- c()
	if(!is.null(geneData))
	{
		geneStart <- get.LocationStart(geneData)
		geneEnd <- get.LocationEnd(geneData)
		mutationTypeStandards <-c("Frame_Shift_Del", "Frame_Shift_Ins", "In_Frame_Del", "In_Frame_Ins", "Intron", "Missense_Mutation", "Nonsense_Mutation", "RNA", "Silent", "Splice_Site");
		colorSelection <- c("red", "darkgoldenrod", "darkred", "darkorange", "blue", "magenta", "plum", "purple", "green", "darkgreen")
		names(colorSelection) <- mutationTypeStandards

		#sortColumn <- "Position_Start"
		#sortColumn <- "Disease_Type"
		plotTypeColumn <- "Variant_Classification"
		startLocationColumn <- "Position_Start"
		endLocationColumn <- "Position_End"
		labelColumn <- "Tumor_Sample_Barcode"
		geneColumn <- "Gene"
		diseaseColumn <- "Disease_Type"
		annotations <- loadGeneMutationDetails(theGene, theZipFile, theVerboseFlag)
		annotations <- annotations[order(annotations[sortColumn2]),]
		annotations <- annotations[annotations$Position_Start>=geneStart,]
		annotations <- annotations[annotations$Position_End<=geneEnd,]
		annotations <- annotations[order(annotations[sortColumn1], decreasing=TRUE),]
		plotFile <- file.path(theOutputDirectory, paste("Mutations_DNA_", theGene, "_", sortColumn1, "-", sortColumn2, ".PNG", sep=""))
		## TODO: Do I need to take into account mixed plus and minus strands?
		labels <- as.vector(unlist(annotations[labelColumn]))
		starts <- as.numeric(as.vector(unlist(annotations[startLocationColumn])))
		#message("plotMutations_dna starts=", paste(starts, sep = ",", collapse = ","))
		types <- as.vector(unlist(annotations[plotTypeColumn]))
		diseases <- toupper(as.vector(unlist(annotations[diseaseColumn])))
		title <- paste("Gene Mutations", theGene, "width =", abs(geneStart - geneEnd), "N=", sum(!is.na(starts)))
		rowNumbers <- c(1:length(labels))
		rowLabels <- paste(diseases, types, sep="-")
		rowLabelNumbers <- rowLabelNumbersFromLabels(rowLabels)
		rowLabelsSimplified <- simplifyRowLabels(rowLabels, rowLabelNumbers)
		rowLabelDividers <- dividerRowLabels(rowLabels)
		#
		plotDiseases <- unique(sort(diseases))
		plotTypes <- unique(sort(as.vector(unlist(annotations[plotTypeColumn]))))

		myWidth <- (12*length(unique(sort(rowLabels))))
		#myHeight <- (12*length(unique(sort(rowLabels))))
		myHeight <- (24*length(unique(sort(rowLabels))))
		if (myWidth < 1000)
		{
			myWidth <- 1000
		}
		if (myHeight < 1000)
		{
			myHeight <- 1000
		}
		#CairoPNG(filename=plotFile, width=1000, height=)
		verboseMessage("plotMutations_dna plotFile=", plotFile)
		CairoPNG(filename=plotFile, width=myWidth, height=myHeight)
		on.exit(dev.off(), add = TRUE)
		#plot.new()
		op <- par(no.readonly = TRUE)
		on.exit(par(op), add = TRUE)
		# c(bottom, left, top, right)
		#par(mar=c(17,17,5,5))
		# c(11,8,5,5)
		####par(xpd=TRUE, mar=c(12,11,5,15))
		par(xpd=TRUE, mar=c(0,11,5,15))
		# the par family does not work with Cairo
		par(family="mono")
		##############################################################################
		##############################################################################
		# mgp=c(6, 1, 0)
		par(fig=c(0,1,.3,1), new=TRUE)
		plot(1, type="n", las=2, yaxt="n", xaxt="n", xlim=c(geneStart, geneEnd), ylim=c(0, max(rowLabelNumbers)),
				 xlab=startLocationColumn, ylab=paste("Samples sorted by", sortColumn1, "and", sortColumn2), mgp=c(10, 1, 0), main=title)
		##plot(x=starts, y=rowLabelNumbers, col=colorList, las=2, yaxt="n", xaxt="n", xlim=c(geneStart, geneEnd), ylim=c(0, max(rowLabelNumbers)),
		##		 xlab=startLocationColumn, ylab=paste("Samples sorted by", sortColumn1, "and", sortColumn2), mgp=c(6, 1, 0), main=title)
		#xlab=startLocationColumn, ylab=labelColumn, mgp=c(15, 1, 0), main=title)
		for(myDisease in plotDiseases)
		{
			startsSelected <- starts[which(diseases==myDisease)]
			typesSelected <- types[which(diseases==myDisease)]
			rowsSelected <- rowLabelNumbers[which(diseases==myDisease)]
			typeIndex <- rowsSelected
			names(typeIndex) <- typesSelected
			typeIndex <- typeIndex[!duplicated(typeIndex)]
			myTable <- table(typesSelected, startsSelected)
			for(myType in names(typeIndex))
			{
				for(myStart in colnames(myTable))
				{
					myPlotCount <- as.numeric(myTable[myType, myStart])
					if (myPlotCount>0)
					{
						myRow <- as.vector(unlist(typeIndex[myType]))
						myColor <- as.vector(unlist(colorSelection[myType]))
						points(x=myStart, y=myRow, col=myColor, pch=getPlotSymbol(myPlotCount))
					}
				}
			}
		}
		verboseMessage("lines")
		for(index in 2:length(rowLabelNumbers))
		{
			if(""!=rowLabelDividers[index])
			{
				myRowNum <- rowLabelNumbers[index]-0.5
				#print(myRowNum)
				lines(x=c(geneStart, geneEnd), y=c(myRowNum, myRowNum), lwd=1)
			}
		}
####		myRowNum <- rowLabelNumbers[length(rowLabelNumbers)]+0.5
####		lines(x=c(geneStart, geneEnd), y=c(myRowNum, myRowNum), lwd=1)
		#
		axis(2, at=1:length(rowLabelsSimplified), labels=rowLabelsSimplified, las=2, tick=FALSE)
		# width of diagram is myWidth
		# 20 is a magic number based on general height of text
		divisor <- 100*(abs(geneStart - geneEnd)/myWidth)
		xAxisLabels <- averageWithin(c(geneStart, sort(starts), geneEnd), divisor)
		if (geneStart!=xAxisLabels[1])
		{
			xAxisLabels[1] <- geneStart
		}
		if (geneEnd!=xAxisLabels[length(xAxisLabels)])
		{
			xAxisLabels[length(xAxisLabels)] <- geneEnd
		}
		#axis(1, at=c(geneStart, geneEnd), labels=c(geneStart, geneEnd), las=2)
		#message("plotMutations_dna xAxisLabels=", paste(xAxisLabels, sep = ",", collapse = ","))
		#message("plotMutations_dna geneStart=", geneStart)
		#message("plotMutations_dna geneEnd=", geneEnd)
		#axis(1, at=xAxisLabels, labels=xAxisLabels, las=2, cex.axis=2)
		legend(geneEnd+divisor, median(rowLabelNumbers), legend=mutationTypeStandards, fill=colorSelection, title="Mutation Types")
		legend(geneEnd+divisor, max(rowLabelNumbers), legend=c("1-2 Mutations", ">=3 Mutations"), pch=c(1, 16), title="Mutation Counts")
		##############################################################################
		##############################################################################
		par(fig=c(0,1,0,.3), new=TRUE)
		#par(xpd=TRUE, mar=c(12,11,5,15))
##		par(xpd=TRUE, mar=c(12,11,0.05,15))
		par(xpd=TRUE, mar=c(12,12.5,0.05,16.5))
		plotGeneArchitecture(theGene, geneStart, geneEnd, xAxisLabels, divisor, theZipFile, theVerboseFlag)
		##############################################################################
		##############################################################################
	}
	plotFile
}

# ******************************************************************
# ******************************************************************
# ******************************************************************
# ******************************************************************

plotMutations_aminoacid <- function(theGene, theOutputDirectory, theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
	sortColumn1 <- "Disease_Type"
	sortColumn2 <- "Variant_Classification"
	# no protein size data available
	#geneData <- getMetadata_Gene_Mutations(theGene, theZipFile, theVerboseFlag)
	#geneStart <- get.LocationStart(geneData)
	#geneEnd <- get.LocationEnd(geneData)
	mutationTypeStandards <-c("Frame_Shift_Del", "Frame_Shift_Ins", "In_Frame_Del", "In_Frame_Ins", "Intron", "Missense_Mutation", "Nonsense_Mutation", "RNA", "Silent", "Splice_Site");
	colorSelection <- c("red", "darkgoldenrod", "darkred", "darkorange", "blue", "magenta", "plum", "purple", "green", "darkgreen")
	names(colorSelection) <- mutationTypeStandards
	plotTypeColumn <- "Variant_Classification"
	locationColumn <- "amino_acid_position"
	labelColumn <- "Tumor_Sample_Barcode"
	geneColumn <- "Gene"
	diseaseColumn <- "Disease_Type"
	annotations <- loadGeneMutationDetails(theGene, theZipFile, theVerboseFlag)
	annotations <- annotations[order(annotations[sortColumn2]),]
	annotations <- annotations[order(annotations[sortColumn1], decreasing=TRUE),]
	plotFile <- file.path(theOutputDirectory, paste("Mutations_AminoAcid_", theGene, "_", sortColumn1, "-", sortColumn2, ".PNG", sep=""))
	## TODO: Do I need to take into account mixed plus and minus strands?
	starts <- as.vector(unlist(annotations[locationColumn]))
	starts <- as.numeric(starts)
	geneStart <- min(starts, na.rm = TRUE)
	geneEnd <- max(starts, na.rm = TRUE)
	labels <- as.vector(unlist(annotations[labelColumn]))
	title <- paste("Amino Acid Mutations", theGene, "width =", abs(geneStart - geneEnd), "N=", sum(!is.na(starts)))
	types <- as.vector(unlist(annotations[plotTypeColumn]))
	diseases <- toupper(as.vector(unlist(annotations[diseaseColumn])))
	rowNumbers <- c(1:length(labels))
	rowLabels <- paste(diseases, types, sep="-")
	rowLabelNumbers <- rowLabelNumbersFromLabels(rowLabels)
	rowLabelsSimplified <- simplifyRowLabels(rowLabels, rowLabelNumbers)
	rowLabelDividers <- dividerRowLabels(rowLabels)
	#
	plotDiseases <- unique(sort(diseases))
	plotTypes <- unique(sort(as.vector(unlist(annotations[plotTypeColumn]))))

	myWidth <- (12*length(unique(sort(rowLabels))))
	myHeight <- (12*length(unique(sort(rowLabels))))
	if (myWidth < 1000)
	{
		myWidth <- 1000
	}
	if (myHeight < 1000)
	{
		myHeight <- 1000
	}
	#CairoPNG(filename=plotFile, width=1000, height=)
	verboseMessage("plotMutations_aminoacid plotFile=", plotFile)
	CairoPNG(filename=plotFile, width=myWidth, height=myHeight)
	on.exit(dev.off(), add = TRUE)
	#plot.new()
	op <- par(no.readonly = TRUE)
	on.exit(par(op), add = TRUE)
	# c(bottom, left, top, right)
	#par(mar=c(17,17,5,5))
	# c(11,8,5,5)
	par(xpd=TRUE, mar=c(10,9,5,15))
	# the par family does not work with Cairo
	par(family="mono")
	#print("geneStart")
	#print(geneStart)
	#print("geneEnd")
	#print(geneEnd)
	# mgp=c(6, 1, 0) axis title, axis labels and axis line
	# mar=c(5, 4, 4, 2) + 0.1 c(bottom, left, top, right)
	plot(1, type="n", las=2, yaxt="n", xaxt="n", xlim=c(geneStart, geneEnd), ylim=c(0, max(rowLabelNumbers)),
			 xlab=locationColumn, ylab=paste("Samples sorted by", sortColumn1, "and", sortColumn2),
			 mgp=c(8, 1, 0), main=title)
	##plot(x=starts, y=rowLabelNumbers, col=colorList, las=2, yaxt="n", xaxt="n", xlim=c(geneStart, geneEnd), ylim=c(0, max(rowLabelNumbers)),
	##		 xlab=startLocationColumn, ylab=paste("Samples sorted by", sortColumn1, "and", sortColumn2), mgp=c(6, 1, 0), main=title)
	#xlab=startLocationColumn, ylab=labelColumn, mgp=c(15, 1, 0), main=title)
	for(myDisease in plotDiseases)
	{
		startsSelected <- starts[which(diseases==myDisease)]
		typesSelected <- types[which(diseases==myDisease)]
		rowsSelected <- rowLabelNumbers[which(diseases==myDisease)]
		typeIndex <- rowsSelected
		names(typeIndex) <- typesSelected
		typeIndex <- typeIndex[!duplicated(typeIndex)]
		myTable <- table(typesSelected, startsSelected)
		for(myType in names(typeIndex))
		{
			for(myStart in colnames(myTable))
			{
				myPlotCount <- as.numeric(myTable[myType, myStart])
				if (myPlotCount>0)
				{
					myRow <- as.vector(unlist(typeIndex[myType]))
					myColor <- as.vector(unlist(colorSelection[myType]))
					points(x=myStart, y=myRow, col=myColor, pch=getPlotSymbol(myPlotCount))
				}
			}
		}
	}
	for(index in 2:length(rowLabelNumbers))
	{
		if(""!=rowLabelDividers[index])
		{
			myRowNum <- rowLabelNumbers[index]-0.5
			lines(x=c(geneStart, geneEnd), y=c(myRowNum, myRowNum), lwd=1)
		}
	}
####	myRowNum <- rowLabelNumbers[length(rowLabelNumbers)]+0.5
####	lines(x=c(geneStart, geneEnd), y=c(myRowNum, myRowNum), lwd=1)
	#
	axis(2, at=1:length(rowLabelsSimplified), labels=rowLabelsSimplified, las=2, tick=FALSE)
	# width of diagram is myWidth
	divisor <- 100*(abs(geneStart - geneEnd)/myWidth)
	xAxisLabels <- averageWithin(c(geneStart, sort(starts), geneEnd), divisor)
	#print("xAxisLabels1")
	#print(xAxisLabels)
	if (geneStart!=xAxisLabels[1])
	{
			xAxisLabels[1] <- geneStart
	}
	#print("xAxisLabels2")
	#print(xAxisLabels)
	if (geneEnd!=xAxisLabels[length(xAxisLabels)])
	{
			xAxisLabels[length(xAxisLabels)] <- geneEnd
	}
	#print("xAxisLabels3")
	#print(xAxisLabels)
	axis(1, at=xAxisLabels, labels=xAxisLabels, las=2, cex.axis=2)
	legend(geneEnd+divisor, median(rowLabelNumbers), legend=mutationTypeStandards, fill=colorSelection, title="Mutation Types")
	legend(geneEnd+divisor, max(rowLabelNumbers), legend=c("1-2 Mutations", ">=3 Mutations"), pch=c(1, 16), title="Mutation Counts")
	plotFile
}

# ******************************************************************
# ******************************************************************
# ******************************************************************
# ******************************************************************
