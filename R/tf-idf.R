#' An R function that computes the frequency of a term depending on how rarely it is used.
#' This function calculates the tf, idf and tf-idf indexes of words in the bidens_speeches_data dataset and extracts and plots the top 10 tf-idf words for each speech event.
#' First, the dataset is tokenized and stop words are removed, then the tf, idf and tf-idf indexes are computed.
#' It also produces a bar plot of the top ten  tf-idf words for each event in ascending order using the \code{ggplot2} package.
#'
#' @param datafile The dataset containing words to be indexed.
#'
#' @return Plots of the tf-idf index of top 10 tf-idf words from each speech event:
#' \describe{
#' \item{data_file}{The dataframe.}
#' \item{df_tokenized}{A variable denoting the speech part number as well as tokenize speech.}
#' \item{df_token_cleaned}{Removes stop words.}
#' \item{df_final}{Groups tokenized words by location, computes tf-idf index.}
#' \item{top_words}{top 10 tf-idf words for each event.}
#' }
#' @author 10784424, 10782401, 10782689, 10757306.
#' @import ggplot2
#' @import readr
#' @import dplyr
#' @import tidytext
#' @import tidyverse
#' @export
#' @examples
#' tf_idf(datafile = bidens_speeches_data)
#'
#'
tf_idf <- function(datafile){
    df_tokenized <- datafile %>%
    mutate(speechnumber = datafile$part) %>% # create new variable denoting the speech part number
    unnest_tokens(word, speech)

    df_token_cleaned <- df_tokenized %>%
    anti_join(tidytext::stop_words)

    df_final <- df_token_cleaned %>%  count(location, word, event) %>%
    bind_tf_idf(word, location, n) %>% group_by(location) %>% arrange(desc(tf_idf), .by_group = TRUE)

    #getting the top 10 tf_idf words for each event
    top_words <- df_final %>% slice(1:10)
    ggplot(top_words,
           aes(x = reorder(word, -tf_idf),
           y=tf_idf, fill=event)) +
      geom_col() +
      coord_flip() +
      labs(x='Top Words at each event', y = "tf_idf index", title="Highest tf-idf Word Frequency for Joe Biden's Speeches in 2020") +
      facet_wrap(~ event, scales='free', strip.position = 'left') +
      theme(axis.text = element_text(size = 8, color = "black"),
            axis.title = element_text(size = 10, color = "black"),
            title = element_text(size = 10)) +
      theme(legend.position = "none")
}


