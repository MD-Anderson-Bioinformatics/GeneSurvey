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

filterForCommonSamples <- function(...)
{
  commonSamples <- NULL
  allNames <- lapply(list(...), function(theData)
  {
    stopifnot(is.matrix(theData))
    substring(colnames(theData), 1, 15)
  })
  for (index in 1:length(allNames))
  {
    if (is.null(commonSamples))
    {
      commonSamples <- allNames[[index]]
    }
    else
    {
      commonSamples <- intersect(commonSamples, allNames[[index]])
    }
  }
  results <- lapply(list(...), function(theDataSet, theCommonList)
  {
    colnames(theDataSet) <- substring(colnames(theDataSet), 1, 15)
    theDataSet[,theCommonList]
  }, theCommonList=commonSamples)
  results
}

#################################################################
#################################################################
#################################################################
#################################################################
