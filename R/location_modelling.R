


#' Raead and prepare shape files for location the reported impacts
#' This shape files are used to link the reported impact locations into
#' districts. Here, we use the NUTS-system which is used for European countries
#' @param nut_shape_files 
#'
#' @return sf shape file to be processed within R code
#' @export
#'
#' @examples
read_merge_nuts_data <- function(nut_shape_files){
  relevant_files <- list.files(nut_shape_files, pattern = "\\.shp$", 
                               full.names = TRUE)
  all_nuts_shape <- map_dfr(relevant_files, ~st_read(.x)) %>%
    st_as_sf()
  return(all_nuts_shape)
}




#' Import names of locations to be identified within newspaper articles
#' for identifying geographic scope of reported impacts
#'
#' @param pre_procssed_file File containing German locations with 
#' geo-coordinates
#'
#' @return Dataframe containing German locations with geo-coordinates
#' @export
#'
#' @examples
process_geonames_data <- function(pre_procssed_file){
  load(pre_procssed_file)
  return(geonames_data_excl_tokens)
}






#' Link mentioned locations to database of geo-locations
#' The locations mentioned in the text (using spacy) are linked to the geonames
#' data in order to add coordinates to the locations and locate them for
#' further identification of the geographic scope of the reported impacts
#'
#' @param ner_annotations_spacy Locations identified by spacy from the text
#' @param geonames_data Data provided by process_geonames_data function
#' @param clean_text_articles Dataframe with text articles

#'
#' @return Dataframe with all matching locations between spacy and the geonames
#' dataset
#' @export
#'
#' @examples
model_impact_locations_b <- function(ner_annotations_spacy,
                                     geonames_data,
                                     clean_text_articles
                                     ){

  spacy_annotations <-  ner_annotations_spacy %>% 
    filter(ent_type == "LOC") %>%   
    dplyr::left_join(clean_text_articles, by = c("doc_id" = "ID")) %>% 
    dplyr::select(doc_id, token, lemma, ent_type, text, date) %>% 
    mutate(token = tolower(token))
  
  places <- geonames_data %>% 
    sf::st_as_sf() %>% 
    mutate(name = tolower(name)) 
  
  osm_dictionary <- places %>% 
    drop_na(name)
  

  #directly merge found tokens w/osm dictionary
  spacy_annotations_osm <- spacy_annotations %>% 
    dplyr::semi_join(osm_dictionary, by = c("token" = "name")) %>% 
    dplyr::select(doc_id, token)
  
  #check two-component names
  spacy_annotations_two_components <-  ner_annotations_spacy %>% 
    filter(ent_type == "LOC") %>% 
    mutate(connected = ifelse(lag(token_order) == token_order -1, 
                              TRUE, FALSE)) %>% 
    mutate(new_places  = ifelse(connected == TRUE, paste(lag(token), token), 
                                NA)) %>% 
    filter(!is.na(new_places)) %>% 
    mutate(token = new_places) %>% 
    dplyr::select(doc_id, token)
  
  
  multi_component_locations <- tibble()
  #if we detect locations composed of two words we will merge them here
  if(nrow(spacy_annotations_two_components) > 0){
    
    
    multi_component_locations <- as_tibble(spacy_annotations_two_components) %>% 
      dplyr::semi_join(osm_dictionary, by = c("token" = "name"))
    }
  
  locations_mapped_osm <- dplyr::bind_rows(multi_component_locations, 
                                           spacy_annotations_osm)
  locations_mapped_osm %>% 
    dplyr::left_join(places, by = c("token" = "name")) %>%  
      distinct(doc_id, token, nuts_id, .keep_all = TRUE)
  
}



cluster_article_locations <- function(located_osm){ 
  
  clusters <-  purrr::map(unique(located_osm$doc_id), 
                          ~try(create_location_clusters_b(.x, 
                                                          located_osm)))
  return(clusters)
}

set_cluster_cutoff_threshold <- function(cluster_impact_locations, 
                                         cutoff_threshold = 10000,
                                         geonames_data){
  
  cutoff <- purrr::map(cluster_impact_locations, 
                       ~create_cutoff(.x, cutoff_threshold)) 
  
  merged_df <- data.frame()
  for(index in seq(1,length(cutoff))){

    if(nrow(cutoff[[index]]) >0){
    tmp <- cutoff[[index]] %>% 
      as_tibble() %>% 
      dplyr::select(doc_id:token) 
    merged_df <- dplyr::bind_rows(merged_df, as.data.frame(tmp))
    }
  }
  merged_df <- merged_df %>% 
    dplyr::left_join(geonames_data %>% dplyr::select(object_id, geometry),
                     by = c("object_id" = "object_id"))
  return(merged_df)
}



create_cutoff <- function(.x, cutoff_threshold){

  if(is.null(.x)){
    return(tibble())
  }
  .x[[2]]$clusters <- cutree(.x[[1]], h=cutoff_threshold)
  return_df <- .x[[2]] %>% 
    dplyr::select(doc_id, clusters, object_id, token)
  
  return(return_df)
}



create_location_clusters <- function(doc, located_osm){
  
  locations_article <- located_osm %>% 
    dplyr::filter(doc_id == doc) %>% 
    sf::st_as_sf(.)
  
  if(nrow(locations_article) <  2){
    return(locations_article)
    }
  
  xy <- SpatialPointsDataFrame(
    matrix(st_coordinates(locations_article$geometry), ncol=2), 
    data.frame(ID=seq(1:nrow(locations_article))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  distances <- distm(xy)
  hc <- hclust(as.dist(distances), method="complete")
  locations_article$clusters <- cutree(hc, h=100)
  return(locations_article)
}


create_location_clusters_b <- function(doc, located_osm){

  locations_article <- located_osm %>% 
    dplyr::filter(doc_id == doc) %>% 
    sf::st_as_sf(.)
  
  if(nrow(locations_article) <  2){
    return(NULL)
  }
  
  xy <- SpatialPointsDataFrame(
    matrix(st_coordinates(locations_article$geometry), ncol=2), 
    data.frame(ID=seq(1:nrow(locations_article))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  distances <- distm(xy)
  hc <- hclust(as.dist(distances), method="complete")
  locations_article$clusters <- cutree(hc, h=100)
  return(list(hc, locations_article))
}



predict_cluster_location <- function(clustered_impact_locations, level  = "NUTS3"){
  
  clustered_impact_locations <- clustered_impact_locations %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() 
  
  article_locations <- purrr::map_df(unique(clustered_impact_locations$doc_id), 
                                     ~singl_predict_cluster_location(dplyr::filter(clustered_impact_locations, doc_id == .x), .x))
  
}


singl_predict_cluster_location <- function(clustered_impact_locations, doc_id_selected){
  
  clustered_impact_locations_tmp <- clustered_impact_locations 
  
  clustered_impact_locations_tmp <- clustered_impact_locations_tmp %>% 
    dplyr::left_join( clustered_impact_locations_tmp %>%
                        as_tibble() %>% 
                        count(token, name = "frequency_of_occurece"),
                      by = c("token" = "token")) %>% 
    dplyr::filter(frequency_of_occurece < 5)
  
  if(nrow(clustered_impact_locations_tmp) == 0){
    return(data.frame())
  }
  
  impact_locations_b_filt_prediction <- clustered_impact_locations_tmp %>% 
    group_by(clusters) %>% 
    count(sort = TRUE) %>%  
    ungroup() %>% 
    slice_max(n = 1, order_by =   n) %>% 
    mutate(doc_id = doc_id_selected)
  
  return(impact_locations_b_filt_prediction)
  
}