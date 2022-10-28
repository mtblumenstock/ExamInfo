#'@title Plot Two Different Groups' Scores
#'
#'@description "plot" takes a dataframe, a column within that dataframe, two groups within that column, a subject with scores attached, and two colors of the user's choice. It filters the dataframe for rows containing only those groups (filtering each one separately and then combining the filtered data), then plots a dotplot. Note: To plot the total score, one must use a returned dataframe from getTotalScoreColumn() or have manually added up the three scores.
#'
#'@param choice1 a value in the column of choice (character)
#'@param choice2 another value in the column of choice (character)
#'@param columnName a column of a dataframe (character), in "name of column" form. NOT a score list
#'@param dataframe a dataframe with at least a list of scores for some subject and students associated with the scores who can be divided into at least two groups (Ex: male/female)
#'@param subject any subject that has a list of scores attached, this example uses math/reading/writing/total, but it can be any subject as long as it's written properly and shows up in the dataframe. It must be in character form without the name of the dataframe and the $.
#'
#'@keywords exam collegeboard SAT calculate grading dotplot
#'
#'@export
#'
#'@examples
#'plot("group A", "group B", "race.ethnicity", exams, "math.score", "blue", "red")

plot <- function(choice1, choice2, columnName, dataframe, subject, color1, color2){
  column <- dataframe[, columnName]
  library(stringr)
  if (choice1 == "male") {
    column <- sub("female", "N/A", column)
    filteredData1 <- dataframe[grepl(choice1, column),]
    column <- sub("N/A", "female", column)
  } else if (choice1 == "high school") {
    column <- sub("some high school", "N/A", column)
    filteredData1 <- dataframe[grepl(choice1, column),]
    column <- sub("N/A", "some high school", column)
  } else {
    filteredData1 <- dataframe[grepl(choice1, column),]
  }
  if (choice2 == "male"){
    column <- sub("female", "N/A", column)
    filteredData2 <- dataframe[grepl(choice2, column),]
    column <- sub("N/A", "female", column)
  } else if (choice2 == "high school") {
    column <- sub("some high school", "N/A", column)
    filteredData2 <- dataframe[grepl(choice2, column),]
    column <- sub("N/A", "some high school", column)
  } else {
    filteredData2 <- dataframe[grepl(choice2, column),]
  }

  combinedData <- rbind(filteredData1, filteredData2)

  scoreToPlot <- combinedData[, subject]
  if (grepl("math", tolower(subject)) == TRUE) {
    xlabel <- "Math Score"
  } else if (grepl("reading", tolower(subject)) == TRUE) {
    xlabel <- "Reading Score"
  } else if (grepl("writing", tolower(subject)) == TRUE) {
    xlabel <- "Writing Score"
  } else if (grepl("total", tolower(subject)) == TRUE) {
    xlabel <- "Total Score"
  }

  colors <- numeric(2)
  colors[column == choice1] <- color1
  colors[column == choice2] <- color2

  if (grepl("gender", tolower(columnName)) == TRUE) {
    columnName <- "Gender"
  } else if ((grepl("race", tolower(columnName)) == TRUE) | (grepl("ethni", tolower(columnName)) == TRUE)){
    columnName <- "Race/Ethnicity"
  } else if (grepl("education", tolower(columnName)) == TRUE) {
    columnName <- "Parent's Level of Education"
  } else if (grepl("lunch", tolower(columnName)) == TRUE) {
    columnName <- "Lunch Status"
  } else if (grepl("course", tolower(columnName)) == TRUE) {
    columnName <- "Completion of Test Prep Course"
  }

  myTitle <- paste("Grades Based On", columnName)
  dotchart(scoreToPlot, labels = NULL, pch = 19, pt.cex = 1, groups = NULL, main = myTitle,
           color = colors, xlab = xlabel, ylab = "Students")
  legend("bottomleft", inset=.02, title="Groups",
         c(str_to_title(choice1), str_to_title(choice2)), fill = c(color1, color2), horiz = FALSE, cex = 0.4)
}
