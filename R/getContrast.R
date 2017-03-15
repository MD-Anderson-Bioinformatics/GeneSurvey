#GeneSurvey Copyright 2014, 2015, 2016 University of Texas MD Anderson Cancer Center
#
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.
#
#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.


#getContrast("/output", "TP53", "SNP6", "SRC", "RnaSeq2")

getContrast <- function(theOutdir, theGene1, theAssay1, theGene2, theAssay2,
												theZipFile="/geneSurveyData/GeneSurvey.zip",
												theMethod="spearman", theUse="pairwise.complete.obs",
												theUseDeltaFlag=FALSE, theRemoveDupFlag=TRUE, theVerboseFlag=FALSE)
{
	stopifnot(theAssay1 %in% c("Meth450", "Meth27", "miRNASeq", "SNP6", "RnaSeq", "RnaSeq2", "Mutations"))
	stopifnot(theAssay2 %in% c("Meth450", "Meth27", "miRNASeq", "SNP6", "RnaSeq", "RnaSeq2", "Mutations"))
	stopifnot(is.character(theGene1))
	stopifnot(is.character(theGene2))
	dir.create(theOutdir, recursive = TRUE, showWarnings=FALSE)
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
	# between the two genes overall
	mycorGenesOverall <- cor(t(data1), t(data2), method=theMethod, use=theUse)
	plotCorByDisease(theOutdir, data1, data2, "Overall",
									 theMethod, theUse, theGene1, theGene2,
									 theAssay1, theAssay2, theVerboseFlag, mycorGenesOverall)
	#######################################
	# get correlations
	# between samples overall
	#TDC-very long-useful? mycorSamplesOverall <- cor(data1, data2, method="spearman", use="pairwise.complete.obs")
	#######################################
	# get correlations
	# stratified by disease type
	mycorByDiseaseList <- getCorByDisease(theOutdir, data1, data2, disease1, disease2,
																				theMethod, theUse, theGene1, theGene2,
																				theAssay1, theAssay2, theVerboseFlag)
	#######################################
	# corList is a list, since mycorByDiseaseList is a list (not a vector)
	corList <- mycorByDiseaseList
	newEle <- list(mycorGenesOverall, data1, data2)
	corList[[length(corList)+1]] <- newEle
	names(corList) <- c(names(mycorByDiseaseList), "Overall")
	# move overall to front
	corList <- corList[c(length(corList),2:length(corList)-1)]
	corList
}

plotCorByDisease <- function(theOutdir, theData1, theData2, theDisease,
														 theMethod, theUse, theGene1, theGene2,
														 theAssay1, theAssay2, theVerboseFlag,
														 theCorrelation)
{
	title <- paste(theGene1, "(", theAssay1, ") versus ", theGene2, "(", theAssay2, ") for ",
								 theDisease, "(N=", ncol(theData1), ") using ", theUse, " with ", theMethod,
								 "\r\nCorrelation=", round(theCorrelation, digits=4), sep="")
	filename <- paste(paste("correlation", theDisease, theGene1, theGene2, sep="_"), "PNG", sep=".")
	filepath <- file.path(theOutdir, filename)
	verboseMessage("plotCorByDisease filepath=", filepath, theVerboseFlag=theVerboseFlag)
	CairoPNG(filename=filepath, width = 800, height = 800, pointsize=12)
	on.exit(dev.off(), add = TRUE)
	#op <- par(no.readonly = TRUE)
	#on.exit(par(op), add = TRUE)
	#par(mar=c(15,5,5,5))
	plot(as.vector(unlist(theData1)),
			 as.vector(unlist(theData2)),
			 xlab=paste(theGene1, "(", theAssay1, ")"),
			 ylab=paste(theGene2, "(", theAssay2, ")"),
			 main=title)
}

getCorByDisease <- function(theOutdir, theData1, theData2, theDisease1, theDisease2,
														theMethod, theUse, theGene1, theGene2,
														theAssay1, theAssay2, theVerboseFlag)
{
	diseaselist <- unique(sort(c(theDisease1, theDisease2)))
	results <- lapply(diseaselist, function(theDisease)
	{
		disData1 <- getDiseaseData(theData1, theDisease1, theDisease)
		disData2 <- getDiseaseData(theData2, theDisease2, theDisease)
		corVal <- cor(t(disData1), t(disData2), method=theMethod, use=theUse)
		plotCorByDisease(theOutdir, disData1, disData2, theDisease,
										 theMethod, theUse, theGene1, theGene2,
										 theAssay1, theAssay2, theVerboseFlag, corVal)
		list(
			corVal,
			disData1,
			disData2)
	})
	names(results) <- diseaselist
	results
}

getDiseaseData <- function(theData, theDiseaseList, theDisease)
{
	myids <- names(theDiseaseList[theDiseaseList==theDisease])
	# need drop=FALSE to prevent R from turning 1 row matrix into vector
	theData[,myids, drop=FALSE]
}

getContrastData <- function(theGene, theAssay, theZipFile, theUseDeltaFlag, theRemoveDupFlag, theVerboseFlag)
{
	results <- NULL
	if ("Meth450"==theAssay)
	{
		results <- getData_Probe_Meth450(theGene, theZipFile, theUseDeltaFlag=theUseDeltaFlag, theRemoveDupFlag=theRemoveDupFlag, theVerboseFlag=theVerboseFlag)
	}
	else if ("miRNASeq"==theAssay)
	{
		results <- getData_CombinedHsaMimat_miRNASeq(theGene, theZipFile, theUseDeltaFlag=theUseDeltaFlag, theRemoveDupFlag=theRemoveDupFlag, theVerboseFlag=theVerboseFlag)
	}
	else if ("Meth27"==theAssay)
	{
		results <- getData_Probe_Meth27(theGene, theZipFile, theUseDeltaFlag=theUseDeltaFlag, theRemoveDupFlag=theRemoveDupFlag, theVerboseFlag=theVerboseFlag)
	}
	else if ("RnaSeq"==theAssay)
	{
		results <- getData_GeneSymbol_RnaSeq(theGene, theZipFile, theUseDeltaFlag=theUseDeltaFlag, theRemoveDupFlag=theRemoveDupFlag, theVerboseFlag=theVerboseFlag)
	}
	else if ("RnaSeq2"==theAssay)
	{
		results <- getData_GeneSymbol_RnaSeq2(theGene, theZipFile, theUseDeltaFlag=theUseDeltaFlag, theRemoveDupFlag=theRemoveDupFlag, theVerboseFlag=theVerboseFlag)
	}
	else if ("SNP6"==theAssay)
	{
		results <- getData_GeneSymbol_SNP6(theGene, theZipFile, theUseDeltaFlag=theUseDeltaFlag, theRemoveDupFlag=theRemoveDupFlag, theVerboseFlag=theVerboseFlag)
	}
	else if ("Mutations"==theAssay)
	{
		results <- getData_GeneSymbol_Mutations(theGene, theZipFile, theUseDeltaFlag=theUseDeltaFlag, theRemoveDupFlag=theRemoveDupFlag, theVerboseFlag=theVerboseFlag)
	}
	results
}