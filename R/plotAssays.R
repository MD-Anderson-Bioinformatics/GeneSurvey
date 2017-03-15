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

plotAssayForDisease <- function(theCorVal, theData1, theData2, theDisease, theCorrelation,
																theGene1, theGene2, theAssay1, theAssay2, theOutputDir,
																theColors,
																theVerboseFlag)
{
	title <- paste(theDisease, " for ",
								 theGene1, " (", theAssay1, ") versus ",
								 theGene2, " (", theAssay2, ") ",
								 "(N=", ncol(theData1), ")",
								 "\r\nSpearman=", round(theCorrelation, digits=4), sep="")
	filename <- paste(paste("assayplot", theAssay1, theAssay2, theGene1, theGene2, theDisease,  sep="_"), "PNG", sep=".")
	filepath <- file.path(theOutputDir, filename)
	verboseMessage("plotAssayForDisease filepath=", filepath, theVerboseFlag=theVerboseFlag)
	CairoPNG(filename=filepath, width = 800, height = 800, pointsize=12)
	on.exit(dev.off(), add = TRUE)
	#op <- par(no.readonly = TRUE)
	#on.exit(par(op), add = TRUE)
	#par(mar=c(15,5,5,5))
	plot(as.vector(unlist(theData1)),
			 as.vector(unlist(theData2)),
			 xlab=paste(theGene1, " (", theAssay1, ")", sep=""),
			 ylab=paste(theGene2, " (", theAssay2, ")", sep=""),
			 main=title,
			 col=theColors)
	filepath
}

plotAssayByDisease <- function(theData1, theData2, theDisease1, theDisease2, theMethod, theUse,
															 theGene1, theGene2, theAssay1, theAssay2, theOutputDir,
															 theVerboseFlag)
{
	diseaselist <- unique(sort(c(theDisease1, theDisease2)))
	verboseMessage("get colors", theVerboseFlag=theVerboseFlag)
	tcgaColors <- getColorsForDiseases(diseaselist)
	results <- lapply(diseaselist, function(theDisease)
	{
		disData1 <- getDiseaseData(theData1, theDisease1, theDisease)
		disData2 <- getDiseaseData(theData2, theDisease2, theDisease)
		corVal <- cor(t(disData1), t(disData2), method=theMethod, use=theUse)
		plotAssayForDisease(corVal, disData1, disData2, theDisease, corVal,
												theGene1, theGene2, theAssay1, theAssay2, theOutputDir,
												tcgaColors[theDisease],
												theVerboseFlag)
	})
	results <- as.vector(unlist(results))
	corVal <- cor(t(theData1), t(theData2), method=theMethod, use=theUse)
	results <- c(plotAssayForDisease(corVal, theData1, theData2, "ALL", corVal,
																	 theGene1, theGene2, theAssay1, theAssay2, theOutputDir,
																	 tcgaColors[theDisease1],
																	 theVerboseFlag),
							 results)
	results
}

#################################################################
#################################################################
# exported
#################################################################
#################################################################

plotAssays <- function(theGene1, theGene2, theAssay1, theAssay2, theOutputDir,
											 theZipFile="/geneSurveyData/GeneSurvey.zip",
											 theUseDeltaFlag=FALSE, theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	# much if this is taken from the getContract file. These might need to share more at some point
	stopifnot(is.character(theGene1))
	stopifnot(1==length(theGene1))
	stopifnot(is.character(theGene2))
	stopifnot(1==length(theGene2))
	stopifnot(is.character(theAssay1))
	stopifnot(1==length(theAssay1))
	stopifnot(is.character(theAssay2))
	stopifnot(1==length(theAssay2))
	stopifnot(theAssay1 %in% c("Mutations", "RnaSeq2", "RnaSeq", "SNP6", "Meth450", "Meth27", "miRNASeq"))
	stopifnot(theAssay2 %in% c("Mutations", "RnaSeq2", "RnaSeq", "SNP6", "Meth450", "Meth27", "miRNASeq"))
	stopifnot(isValidDirectoryPath(theOutputDir))
	stopifnot(file.exists(theZipFile))
	stopifnot((TRUE==theVerboseFlag)||(FALSE==theVerboseFlag))
	stopifnot((TRUE==theUseDeltaFlag)||(FALSE==theUseDeltaFlag))
	#
	dir.create(theOutputDir, recursive = TRUE, showWarnings=FALSE)
	#######################################
	# get the data
	data1 <- getContrastData(theGene1, theAssay1, theZipFile, theUseDeltaFlag, theRemoveDupFlag, theVerboseFlag)
	data2 <- getContrastData(theGene2, theAssay2, theZipFile, theUseDeltaFlag, theRemoveDupFlag, theVerboseFlag)
	#######################################
	# get the disease values for samples (or patient ids)
	disease1 <- NULL
	disease2 <- NULL
	if (TRUE==theUseDeltaFlag)
	{
		# patient ids
		disease1 <- getMetadataPop_PatientDisease_forList(colnames(data1), theZipFile=theZipFile, theVerboseFlag=theVerboseFlag)
		disease2 <- getMetadataPop_PatientDisease_forList(colnames(data2), theZipFile=theZipFile, theVerboseFlag=theVerboseFlag)
	} else {
		# barcodes (samples)
		disease1 <- getMetadataPop_BarcodeDisease_forList(colnames(data1), theZipFile=theZipFile, theVerboseFlag=theVerboseFlag)
		disease2 <- getMetadataPop_BarcodeDisease_forList(colnames(data2), theZipFile=theZipFile, theVerboseFlag=theVerboseFlag)
	}
	#######################################
	# remove UNK data
	disease1 <- disease1[disease1!="UNK"]
	disease2 <- disease2[disease2!="UNK"]
	#######################################
	# match up data by barcode through portion without replicates
	# this gives us one to one matches only
	#           1111111111222222222
	#  1234567890123456789012345678
	# "TCGA-AR-A1AV-01A-21D-A12R-05"
	# shorten list of names
	barStop <- 19
	list1 <- substring(colnames(data1), 1, barStop)
	list2 <- substring(colnames(data2), 1, barStop)
	# remove duplicates from original data
	# need drop=FALSE to prevent R from turning 1 row matrix into vector
	data1 <- data1[,!duplicated(list1), drop=FALSE]
	data2 <- data2[,!duplicated(list2), drop=FALSE]
	# remove duplicates from list
	list1 <- list1[!duplicated(list1)]
	list2 <- list2[!duplicated(list2)]
	# set original colnames to shortened versions
	colnames(data1) <- list1
	colnames(data2) <- list2
	# get list of portions in common
	commonPortions <- list1[list1 %in% list2]
	# limit data to common portions
	# need drop=FALSE to prevent R from turning 1 row matrix into vector
	data1 <- data1[,commonPortions, drop=FALSE]
	data2 <- data2[,commonPortions, drop=FALSE]
	#######################################
	# update disease types via the same method
	list1 <- substring(names(disease1), 1, barStop)
	list2 <- substring(names(disease2), 1, barStop)
	# remove duplicates from original data
	# need drop=FALSE to prevent R from turning 1 row matrix into vector
	disease1 <- disease1[!duplicated(list1), drop=FALSE]
	disease2 <- disease2[!duplicated(list2), drop=FALSE]
	# remove duplicates from list
	list1 <- list1[!duplicated(list1)]
	list2 <- list2[!duplicated(list2)]
	# set original colnames to shortened versions
	names(disease1) <- list1
	names(disease2) <- list2
	# limit disease and gene data to common portions
	# need drop=FALSE to prevent R from turning 1 row matrix into vector
	commonPortions <- commonPortions[commonPortions %in% names(disease1)]
	commonPortions <- commonPortions[commonPortions %in% names(disease2)]
	disease1 <- disease1[commonPortions, drop=FALSE]
	disease2 <- disease2[commonPortions, drop=FALSE]
	data1 <- data1[,commonPortions, drop=FALSE]
	data2 <- data2[,commonPortions, drop=FALSE]
	#######################################
	# get correlations
	# stratified by disease type
	filenames <- plotAssayByDisease(data1, data2, disease1, disease2,
																	"spearman", "pairwise.complete.obs",
																	theGene1, theGene2, theAssay1, theAssay2, theOutputDir,
																	theVerboseFlag)
	filenames
}

#################################################################
#################################################################
#################################################################
#################################################################
