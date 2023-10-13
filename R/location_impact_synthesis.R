






#' Linking reported impacts and extracted locations
#' In this function we link the text corpus with information on reported impacts
#' that are classified and the identified geographic scope of the extracted 
#' location. Thereby, this function provides the resulting spatio-temporal
#' perspective on reported impact
#'
#' @param predict_impact_classes Dataframe of classified impacts for each
#' newspaper article
#' @param predicted_cluster_locations Determimed cluster of locations that
#' desctibes the spatila extent of the reported impact
#' @param clustered_impact_locations Names of locations and their cluster
#' assocations
#' @param clean_text_articles_removed_duplicates Dataframe of text articles
#' @param nuts_geo_data Shape file data on the districts to which the geographic
#' scope of newspaper articles is linked
#' @param geonames_data Dataframe of locations in case-study area
#'
#' @return Dataset describing the newspaper articles, the identified impacts
#' and their spatio-temporal scope
#' @export
#'
#' @examples
merge_locations_impacts_function_b <- function(predict_impact_classes, 
                                               predicted_cluster_locations , 
                                               clustered_impact_locations,
                                               clean_text_articles_removed_duplicates,
                                               nuts_geo_data,
                                               geonames_data){
  


  #predicted_cluster locations contains identified impact clusters 
  predicted_cluster_locations <- predicted_cluster_locations %>% 
    group_by(doc_id) %>% 
    count() %>% 
    dplyr::filter(n == 1) %>% #we only select those documents where one cluster could specifically be identified
    #here we need to continue implementing more advanced heuristics at some point
    ungroup() %>% 
    dplyr::left_join(predicted_cluster_locations %>% 
                       dplyr::select(doc_id, clusters), by = c("doc_id" = "doc_id")) %>% 
    dplyr::select(doc_id, clusters) %>% 
    dplyr::left_join(as_tibble(clustered_impact_locations) %>% dplyr::select(!geometry), 
                     by = c("doc_id" = "doc_id", 
                            "clusters" = "clusters")) %>%
    dplyr::left_join(geonames_data, by = c("object_id" = "object_id")) %>% 
    dplyr::select(doc_id, object_id, token, geometry, nuts_id)
  

  #merge locations, impacts and text corpora 
  predict_impact_classes %>% 
    dplyr::left_join(clean_text_articles_removed_duplicates, by = c("ID" ="ID")) %>% 
    dplyr::left_join(predicted_cluster_locations, by = c("ID" = "doc_id")) %>% 
    dplyr::filter(!is.na(nuts_id))
    
    

}

