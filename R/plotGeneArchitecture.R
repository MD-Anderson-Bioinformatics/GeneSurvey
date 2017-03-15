
getTranscriptSymbols <- function(theExons, theTranscripts)
{
  exonSymbols <- theExons$transcript_symbol
  transcriptSymbols <- theTranscripts$transcript_symbol
  symbols <- unique( sort( c(exonSymbols, transcriptSymbols)))
  symbols
}

getMinStart <- function(theExons, theTranscripts)
{
  min(theTranscripts$start)
}

getMaxEnd <- function(theExons, theTranscripts)
{
  max(theTranscripts$end)
}

getTranscriptStart <- function(theTranscripts, theTranscriptSymbol)
{
  start <- -1
  for(index in 1:nrow(theTranscripts))
  {
    if (theTranscriptSymbol==theTranscripts[index,]$transcript_symbol)
    {
      start <- theTranscripts[index,]$start
    }
  }
  start
}

getTranscriptEnd <- function(theTranscripts, theTranscriptSymbol)
{
  end <- -1
  for(index in 1:nrow(theTranscripts))
  {
    if (theTranscriptSymbol==theTranscripts[index,]$transcript_symbol)
    {
      end <- theTranscripts[index,]$end
    }
  }
  end
}

plotGeneArchitecture <- function(theGeneEq, theGeneStart, theGeneEnd, theXAxisLabels, theDivisor,
                                 theZipFile="/geneSurveyData/GeneSurvey.zip", theVerboseFlag=FALSE)
{
  verboseMessage("plotGeneArchitecture")
  #print(theGeneStart)
  #print(theGeneEnd)
  # Get Exons and Transcripts
  myExons <- getExons_GeneSymbol_HG19(theGeneEq, theZipFile, theVerboseFlag)
  myTranscripts <- getTranscripts_GeneSymbol_HG19(theGeneEq, theZipFile, theVerboseFlag)
  # get transcript symbols
  myTranscriptSymbols <- getTranscriptSymbols(myExons, myTranscripts)
  #print(myTranscriptSymbols)
  # get min transcript start
  minStart <- getMinStart(myExons, myTranscripts)
  #print(minStart)
  # get max transcript end
  maxEnd <- getMaxEnd(myExons, myTranscripts)
  #print(maxEnd)
  # make matrix of data
  dataMatrix <- matrix(0,nrow=length(myTranscriptSymbols), ncol=(maxEnd-minStart+1))
  #print(dim(dataMatrix))
  # populate transcript ranges
  verboseMessage("populate transcript ranges")
  for(myrow in 1:length(myTranscriptSymbols))
  {
    mysymbol <- myTranscriptSymbols[myrow]
    #print(mysymbol)
    mystart <- getTranscriptStart(myTranscripts,mysymbol)
    myend <- getTranscriptEnd(myTranscripts,mysymbol)
    mystart <- mystart - minStart + 1
    myend <- myend - minStart + 1
    #print(mystart)
    #print(myend)
    dataMatrix[myrow, c(mystart:myend)] <- 1
  }
  # populate exon ranges
  verboseMessage("populate exon ranges")
  for(myrow in 1:length(myTranscriptSymbols))
  {
    mysymbol <- myTranscriptSymbols[myrow]
    #print(mysymbol)
    #print(myrow)
    for(index in 1:nrow(myExons))
    {
      myexon <- myExons[index,]
      if (mysymbol==myexon$transcript_symbol)
      {
        mystart <- myexon$start
        myend <- myexon$end
        mystart <- mystart - minStart + 1
        myend <- myend - minStart + 1
        #print(mystart)
        #print(myend)
        dataMatrix[myrow, c(mystart:myend)] <- 2
      }
    }
  }
  # setup labels and plot
  rownames(dataMatrix) <- myTranscriptSymbols
  colnames(dataMatrix) <- minStart:maxEnd
  image((1:ncol(dataMatrix)), (1:nrow(dataMatrix)), t(apply(dataMatrix, 2, rev)), main=NULL, axes=FALSE, xlab="", ylab="",
        col=c("white", "lightblue", "blue"),
        breaks=c(-1, 0.5, 1.5, 2.5))
        #, xlim=c(theGeneStart, theGeneEnd)
#  axis(3, at=1:ncol(dataMatrix), labels=colnames(dataMatrix), tick=FALSE)
#  axis(2, at=1:nrow(dataMatrix), labels=rev(rownames(dataMatrix)), las=2, tick=FALSE, cex.axis=0.5)
  axis(1, at=theXAxisLabels-minStart+1, labels=theXAxisLabels, las=2, cex.axis=2)
  legend(ncol(dataMatrix)+(theDivisor/2), nrow(dataMatrix), legend=c("outside of gene", "transcript", "exon"), fill=c("white", "lightblue", "blue"), title="Gene Components")
}