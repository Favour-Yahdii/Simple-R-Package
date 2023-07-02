#' An R function that produces regression statistics as well as a line plot illustration of zipf's law
#'
#' This function produces a plot showing the relationship between a word's rank in a document and its term frequency, in this case, the Joe Biden's dataset.
#' First, the dialogue is tokenized, then the term frequency (tf) of the words are computed alongside their ranking.
#' It produces a plot of the word rank and term frequency with a linear regression line using the \code{ggplot2} package.
#'
#' @param datafile The dataset passed into the function containing text to be used.
#'
#' @return A plot/plots of the percentage change of words in Biden's speeches over time:
#' \describe{
#' \item{datafile}{The dataframe to be used.}
#' \item{df_tokenized}{A variable denoting the speech part number that also tokenizes speech.}
#' \item{word_group}{Groups tokenized speech by location and computes tf-idf index.}
#' \item{model}{The linear regression model of the independent and dependent variables.}
#' }
#' @author 10784424, 10782401, 10782689, 10757306.
#' @import ggplot2
#' @import readr
#' @import dplyr
#' @import tidytext
#' @import tidyverse
#' @export
#' @examples
#' zipf(datafile=bidens_speeches_data)
#'
#'

zipf <- function(datafile){
  df_tokenized <- datafile %>%
  mutate(speechnumber = datafile$part) %>% # create new variable denoting the speech part number
  unnest_tokens(word, speech)

  word_group <- df_tokenized %>%
  group_by(location) %>%
  count(word, sort=TRUE) %>%
  bind_tf_idf(word, location, n) %>%
  arrange(desc(tf))
  #tf is done using bind_tf_idf
  #rank is row number after sorting in decreasing tf and grouping by location,
  word_group <- word_group %>% mutate(rank = row_number())
  #Fit the model
  model <- lm(tf ~ rank, data = word_group)

  # Summary of the model
  print(summary(model))

  #plot different colours for each location.
  ggplot(word_group,
         aes(x = rank, y=tf, colour=location)) +
    geom_line() +
    geom_smooth(method="lm", col="black", se=FALSE, linewidth=0.5)+
    scale_x_log10("Word Rank") + # logarithmic scale on the x-axis
    scale_y_log10("Term Frequency (tf)") +
    labs(title="Zipf's Law for Joe Biden's Speeches in 2020") +
    theme(axis.text = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 10, color = "black"),
          title = element_text(size = 10))

}
