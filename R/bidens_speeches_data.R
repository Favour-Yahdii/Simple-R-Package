#' The Bidens Speeches data
#'
#' The Bidens Speeches Dataset contains textual data as well as other information from Joe Biden's 2020 Speeches.
#' It contains speeches delivered at 6 National events in the United States from August to November, 2020.
#' @docType data
#' @name bidens_speeches_data
#' @format A data.frame with 128 rows and 5 columns:
#' \describe{
#'  \item{Speech}{Words contained in the Speech}
#'  \item{Part}{The section of the speech the words belong to}
#'  \item{Location}{Location in the United States where the Speech was delivered}
#'  \item{Event}{Event at which the speech was delivered}
#'  \item{Date}{Date the Speech was delivered}
#'  }
#' @examples
#' words <- c("schools", "justice", "president")
#' datafile <- bidens_speeches_data
#' plot_biden_word_proportion(words,datafile)
#' tf_idf(datafile)
#' zipf(datafile)
#' @source Data provided by MATH513 Coursework.
'bidens_speeches_data'
