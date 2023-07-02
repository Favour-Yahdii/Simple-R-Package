#' An R function that produces a plot showing how the proportion of given words in Joe Biden's 2020 Speeches changed over time
#'
#' This function produces a plot showing the proportional change of words in Joe Biden's speeches over time.
#' First, the dialogue is tokenized and stop words are removed, then the proportion of the word or words is computed.
#' It also plots a scatter plot with a linear regression line using the \code{ggplot2} package.
#'
#' @param words A character vector containing the word or words to be computed.
#' @param datafile The Dataset to be used in the function
#' @return A plot/plots of the percentage change of words in Biden's speeches over time:
#' \describe{
#' \item{data_file}{The dataframe.}
#' \item{new}{A variable denoting the speech part number.}
#' \item{tokens}{Tokenizes speech and remove stop words.}
#' \item{tokens_group}{Groups tokenized words by speech number, computes proportion.}
#' \item{word_plot}{Plots word propoprtions over time with Linear Regression line.}
#' }
#' @author 10784424, 10782401, 10782689, 10757306.
#' @import ggplot2
#' @import readr
#' @import dplyr
#' @import tidytext
#' @import tidyverse
#' @export
#' @examples
#' plot_biden_word_proportion(words= c("schools", "justice"), datafile=bidens_speeches_data)
#'
#'
plot_biden_word_proportion <- function(words, datafile){


  # create new variable denoting the speech part number
  new <- datafile %>%
    mutate(speechnumber = datafile$part)

  # create tokens variable that unnests tokens and removes stop words
  tokens <- new %>% unnest_tokens(word, speech) %>%
    anti_join(tidytext::stop_words)

  # create variable that groups words based on speech number,creates a value 'p' and filters for the desired word
  tokens_group <- tokens %>%
    count(speechnumber, word, date) %>% group_by(speechnumber) %>%
    mutate(p = n / sum(n)) %>%
    filter(word%in%words)

  #plot the words
  word_plot <- ggplot(tokens_group, aes(x = date, y = p, color=word)) +
    geom_point() +
    geom_smooth(method="loess",se=TRUE) +
    labs(y = "Percentage of words") +
    labs(x = "Speech Date") +
    labs(title="Change of Word Frequency over time in Joe Biden's 2020 Speeches Dataset") +
    facet_wrap(~word, scales='free')+
    theme(legend.position = "none") +
    theme(axis.text = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 10, color = "black"),
          title = element_text(size = 10)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_smooth(method='lm', col='black', se=FALSE)
  word_plot

}


