% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot.R
\name{plot}
\alias{plot}
\title{Plot Two Different Groups' Scores}
\usage{
plot(choice1, choice2, columnName, dataframe, subject, color1, color2)
}
\arguments{
\item{choice1}{a value in the column of choice (character)}

\item{choice2}{another value in the column of choice (character)}

\item{columnName}{a column of a dataframe (character), in "name of column" form. NOT a score list}

\item{dataframe}{a dataframe with at least a list of scores for some subject and students associated with the scores who can be divided into at least two groups (Ex: male/female)}

\item{subject}{any subject that has a list of scores attached, this example uses math/reading/writing/total, but it can be any subject as long as it's written properly and shows up in the dataframe. It must be in character form without the name of the dataframe and the $.}
}
\description{
"plot" takes a dataframe, a column within that dataframe, two groups within that column, a subject with scores attached, and two colors of the user's choice. It filters the dataframe for rows containing only those groups (filtering each one separately and then combining the filtered data), then plots a dotplot. Note: To plot the total score, one must use a returned dataframe from getTotalScoreColumn() or have manually added up the three scores.
}
\examples{
plot("group A", "group B", "race.ethnicity", exams, "math.score", "blue", "red")
}
\keyword{SAT}
\keyword{calculate}
\keyword{collegeboard}
\keyword{dotplot}
\keyword{exam}
\keyword{grading}
