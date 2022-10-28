#'@title Get Average Score For a Group
#'
#'@description "getAverage" takes a dataframe, a column within that dataframe, a group within that column, and a subject with scores attached. It filters the dataframe for rows containing only that group, then finds the average of whatever subject was chosen and returns that. Note: To find the average total score, one must use a returned dataframe from getTotalScoreColumn() or have manually added up the three scores.
#'
#'@param column a column of a dataframe (character), in "name of column" form. NOT a score list
#'@param group a value in the aforementioned column (character)
#'@param dataframe a dataframe with at least a list of scores for some subject and students associated with the scores who can be divided into at least two groups (Ex: male/female)
#'@param subject any subject that has a list of scores attached, this example uses math/reading/writing/total, but it can be any subject as long as it's written properly and shows up in the dataframe. It must be in character form without the name of the dataframe and the $.
#'
#'@keywords exam collegeboard SAT calculate grading average
#'
#'@export
#'
#'@examples
#'getAverage(exams$gender, "male", exams, "math.score")

getAverage <- function(column, choice, dataframe, subject){
  if (choice == "male"){
    column <- sub("female", "N/A", column)
    filteredData <- dataframe[grepl(choice, column),]
    column <- sub("N/A", "female", column)
  } else if (choice == "high school") {
    column <- sub("some high school", "N/A", column)
    filteredData <- dataframe[grepl(choice, column),]
    column <- sub("N/A", "some high school", column)
  } else {
    filteredData <- dataframe[grepl(choice, column),]
  }

  filteredScoreList <- filteredData[, subject]

  average <- mean(filteredScoreList)
  return(average)
}
