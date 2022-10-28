#'@title Get Total Score Column
#'
#'@description "getTotalScoreColumn" takes a dataframe with three lists of scores (math, reading, writing) and adds them together, then returns a copy of the dataframe with "Total Score" column appended on. It first inputs the totals into a separate list, turns the values into numeric, and cbinds that list to the dataframe to avoid the values being input as type list.
#'
#'@param mathScoreList a list of math scores (numeric), in dataframe$scoreList form
#'@param readingScoreList a list of reading scores (numeric), in dataframe$scoreList form
#'@param writingScoreList a list of writing scores (numeric), in dataframe$scoreList form
#'@param dataframe a dataframe with at least three columns of numeric data (math/reading/writing) and a student associated with each
#'
#'@keywords exam collegeboard total SAT calculate grading
#'
#'@export
#'
#'@examples
#'getTotalScoreColumn(exams$math.score, exams$reading.score, exams$writing.score, exams)

getTotalScoreColumn <- function(mathScoreList, readingScoreList, writingScoreList, dataframe){
  listOfTotalScores = list()
  for (i in 1:(length(mathScoreList))){
    totalScore = mathScoreList[i] + readingScoreList[i] + writingScoreList[i]
    listOfTotalScores[[length(listOfTotalScores) + 1]] <- totalScore
  }
  total.score <- sapply(listOfTotalScores, function(x){as.numeric(x)})
  editedData <- cbind(dataframe, total.score)
  return(editedData)
}
