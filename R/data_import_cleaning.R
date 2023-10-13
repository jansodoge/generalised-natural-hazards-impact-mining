#' CAlculating similarities between documents
#'This function calculates the jaccard similarity between the newspaper articles.
#'The similarities are calculated for groups of newspaper articles (per year)
#'in order to reduce computational cost. We use the textreuse package,
#'see this for details on the hashing procedure
#' @param full_corpus Dataframe of newspaper articles and years
#'
#' @return List of document pairs and their similarity, only similiar documents
#' are listed
#' @export
#'
#' @examples
hashing_duplicates <- function(full_corpus){
  
  results <- purrr::map(unique(full_corpus$year), function(year_selected){
    
  full_corpus_tmp <- full_corpus %>% 
      dplyr::filter(year == year_selected)
    
    hashing_values <- setNames(full_corpus_tmp$text, 
                                full_corpus_tmp$ID)
    
    minhash <- minhash_generator(n = 480, seed = 3552)
    corpus <- TextReuseCorpus(text=hashing_values,
                              tokenizer = tokenize_ngrams, n = 5, 
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = TRUE)
    
    buckets <- lsh(corpus, bands = 240, progress = TRUE)
    candidates <- lsh_candidates(buckets) 
    lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE)
  })
  return(results)
}


#' Remove duplicate articles based on calculated jaccard similarities
#'
#' @param hashing_results calculated jaccard-similarities i.e. the output from
#' hashing_duplicates
#' @param full_corpus Dataframe of newspaper articles and years
#' @param cutoff_threshold Cutoff value for similarity of documents, articles 
#' below this cutoff threshold will be kept while articles more similar than the
#' cutoff threshold will be removed
#'
#' @return Text corpus with duplicate articles being removed
#' @export
#'
#' @examples
remove_hashing_duplicates <- function(hashing_results, 
                                      full_corpus,
                                      cutoff_threshold = .7){
  results <- purrr::map2_dfr(hashing_results, unique(full_corpus$year), 
                               function(hashing_data, year_selected){
                                 
                                 
                                 full_corpus_tmp <- full_corpus %>%
                                   filter(year == year_selected)
                                 
                                 to_remove <- hashing_data %>%
                                   filter(score >= cutoff_threshold) %>%
                                   distinct(b) %>%
                                   pull(b) %>%
                                   as.numeric() %>%
                                   sort()
                                 
                                 filtered_corpus <- full_corpus_tmp %>%
                                   filter(!ID %in% to_remove)
                               }) 
  
  return(results)
}
                         