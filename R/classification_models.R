#' Classification of flood impacts based on keywords
#'This is a dummy-function which classifies articles based on whether
#'the keyword agriculture is mentioned or not. Depending on your case-study
#' different models can be employed here. This code can be easily
#' adjusted to classify multiple impact types. 
#' For example, a trained lasso logistic regression model for classification
#' can be inserted here (see Sodoge et al. 2023)
#' 
#' @param text_corpus Corpus which is to be classified
#'
#' @return Binary classification for each impact category
#' @export
#'
#' @examples
predict_impacts_keywords <- function(text_corpus){
  result_data <- text_corpus %>%
    mutate(agriculture_impacts = ifelse(grepl("landwirtschaft", 
                                              text, 
                                              ignore.case = TRUE), 
                                        1, 0)) %>%
    select(ID, agriculture_impacts)
  
  return(result_data)
}