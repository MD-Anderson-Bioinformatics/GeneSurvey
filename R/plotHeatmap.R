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

plotHeatmapOutput <- function(theGene, theOutputDir, theProbeData, theBarcodeDiseases, theBarcodeSampleType,
															theDataType, theDataTypeLabel, theZipFile, theVerboseFlag, theReadProbeFunction,
															theTag="")
{
  verboseMessage("plotHeatmapOutput start", theVerboseFlag=theVerboseFlag)
  stopifnot(length(theProbeData)>0)
	dir.create(theOutputDir, recursive = TRUE, showWarnings=FALSE)
	returnFilename <- c()
	filesub <- paste(theGene,"_",theDataType,sep="")
	if (""!=theTag)
	{
		filesub <- paste(filesub, "_", theTag,sep="")
	}
	filename <- file.path(theOutputDir, compressIntoFilename(paste(filesub, "_Heatmap.PNG", sep="")))
	returnFilename <- filename
	namesToUse <- rownames(theProbeData)[rownames(theProbeData) %in% names(theBarcodeDiseases)]
	namesToUse <- sort(namesToUse)
	plotData <- theProbeData[namesToUse, , drop=FALSE]
	plotBarcodeDiseases <- theBarcodeDiseases[namesToUse, drop=FALSE]
	diseaseLabels <- c()
	diseaseRows <- c()
	tmpLabelList <- c()
	tempDis <- sort(as.vector(unlist(plotBarcodeDiseases)))
	verboseMessage("plotHeatmapOutput diseases", theVerboseFlag=theVerboseFlag)
	for (diseaseIndex in 1:length(tempDis))
	{
		disease <- tempDis[diseaseIndex]
		diseasePlace <- floor(median(which(tempDis==disease)))
		if (FALSE==disease %in% tmpLabelList)
		{
			diseaseRows <- c(diseaseRows, diseaseIndex)
			tmpLabelList <- c(tmpLabelList, disease)
		}
		if (diseaseIndex==diseasePlace)
		{
			diseaseLabels <- c(diseaseLabels, paste(disease, " (", length(which(tempDis==disease)), ")", sep=""))
		}
		else
		{
			diseaseLabels <- c(diseaseLabels, "")
		}
	}
	verboseMessage("plotHeatmapOutput labels", theVerboseFlag=theVerboseFlag)
	mainText <- paste(theGene, " : ", theDataType, "\n", theDataTypeLabel, " (N=", dim(plotData)[1], ")", sep="")
	if (""!=theTag)
	{
		mainText <- paste(theGene, " : ", theDataType, " : ", theTag, "\n", theDataTypeLabel, " (N=", dim(plotData)[1], ")", sep="")
	}
	myrowsep <- diseaseRows
	rownames(plotData) <- diseaseLabels
	verboseMessage("plotHeatmapOutput locations", theVerboseFlag=theVerboseFlag)
	locations <- unlist(lapply(colnames(plotData), function(theProbe, theZipFile, theVerboseFlag)
	{
		probeData <- theReadProbeFunction(theProbe, theZipFile=theZipFile)
		# stuck as a list :-(
		paste(theProbe, " @ ", probeData[[1]]@mProbeLocation, sep="")
	},theZipFile=theZipFile,theVerboseFlag=theVerboseFlag))
	verboseMessage("plotHeatmapOutput probe data", theVerboseFlag=theVerboseFlag)
	locOnly <- unlist(lapply(colnames(plotData), function(theProbe, theZipFile, theVerboseFlag)
	{
		probeData <- theReadProbeFunction(theProbe, theZipFile=theZipFile, theVerboseFlag=theVerboseFlag)
		# stuck as a list :-(
		probeData[[1]]@mProbeLocation
	},theZipFile=theZipFile,theVerboseFlag=theVerboseFlag))
	verboseMessage("plotHeatmapOutput plot", theVerboseFlag=theVerboseFlag)

	colnames(plotData) <- locations
	plotData <- plotData[,order(locOnly), drop=FALSE]

	CairoPNG(filename=filename, width = 2400, height = 2400, pointsize=36)
	on.exit(dev.off(), add = TRUE)

	if ((length(locOnly)>1) && (length(as.vector(plotData))!=(sum(is.nan(as.vector(plotData))))))
	{
	  pheatmap(plotData, cluster_cols=FALSE, cluster_rows=FALSE,
	           gaps_row=myrowsep-1, main=mainText,
	           color=heat.colors(12), fontsize=log10((length(diseaseLabels)))*5)
	}
	else
	{
		# code modified from that suggested by Ying Zhao!
		image(t(plotData),xaxt="n",yaxt="n",main=mainText, xlab = colnames(plotData))
		mtext(text=rownames(plotData),
		      side=2,line=0.3,
		      at=seq(0,1,length.out=nrow(plotData)),
		      las=2,
		      cex = (0.1 + 1/log10((length(diseaseLabels)/2.0))))
	}
	verboseMessage("plotHeatmapOutput finished", theVerboseFlag=theVerboseFlag)
	returnFilename
}
