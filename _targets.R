################################################################################
### MAIN FILE ##################################################################
# This is the main file in the targets-package to execute the entire pipeline.
# In this file each individual module can be inspected and is documented.
# Each module is attached to functions which are stored in the R folder
# see https://books.ropensci.org/targets/ for  details on the targets package
################################################################################



#load required packages for the pipeline
library(targets)
library(tarchetypes)
library(dbplyr)
library(RPostgreSQL) 
library(DBI)
library(httr)
library(geosphere)
library(rlist)
library(stringr)
library(themis)
library(textreuse)
library(lubridate)
library(janitor)
library(sf)
library(magrittr)
library(readr)
library(readtext)
library(tidytext)
library(arrow)
library(stringdist)
library(reticulate)
library(rgdal)
library(tidyverse)
library(discrim)
library(readxl)
library(furrr)


#import files with relevant functions implemented
source("R/data_import_cleaning.R")
source("R/classification_models.R")
source("R/location_modelling.R")
source("R/location_impact_synthesis.R")
reticulate::source_python('R/ner_module.py')




list(
 
  ##############################################################################
  #### READING AND EDITING DATASET #############################################
  # Two modules to read data from the database (db_file) and import/pre-process
  #the data for 
  #further processing
  # db_file: reads data from database of all newspaper articles. the keywords to
  #query 
  # the database can be edited inside the function 'import_from_database'
  # clean_text_articles: reads a file with all selected articles created 
  #in the db_file
  # module, adds an ID (permalink) and date variable
  ##############################################################################
  targets::tar_target(text_file, "R/data/sample_articles.csv",
                      format = "file"),
  targets::tar_target(clean_text_articles, 
                      read_csv(text_file) %>% 
                        dplyr::mutate(date = lubridate::ymd(date)) %>% 
                        dplyr::mutate(year = year(date))),
                       
  
  
  ##############################################################################
  ### Load misc. files ###
  ## nut_shapes_files: shape files for case-study region, impacts will be 
  ## mapped into these districts
  ## nuts_geo_data: load and pre-process shape files for case-study
  ##############################################################################
  targets::tar_target(nut_shape_files, "R/data/geographic_areas", format = "file"),
  targets::tar_target(nuts_geo_data, read_merge_nuts_data(nut_shape_files)),
  
  
  
  
 
  
  ##############################################################################
  ### Removing duplicates by hashing ###########################################
  ## hashing_results: calculates similarity between articles
  ## clean_text_articles_removed_duplicates: remove  articles that are similar
  ## based on selected threshold
  ##############################################################################
  targets::tar_target(hashing_results, hashing_duplicates(clean_text_articles)),
  targets::tar_target(
     clean_text_articles_removed_duplicates,
     remove_hashing_duplicates(hashing_results,
                               clean_text_articles,
                               cutoff_threshold = 0.7)),
  
  
  
  ##############################################################################
  ### Classify impacts reported in text ########################################
  ## predict_impact_classes: predict different types of impacts, here
  ## illustrated with keyword-based classification models
  ##############################################################################
  targets::tar_target(predict_impact_classes,
                      predict_impacts_keywords(clean_text_articles_removed_duplicates)),
  
  
  
  
   #############################################################################
   ### Identify locations of reported impacts ##################################
   ## geonames_data_file:  file with locations
   ## geonames_data: import and pre-process the geonames data
   ## ner_annotations_spacy: detect locations mentioned in text
   ## cluster_impact_locations: perform hierarchical clustering on the 
   ## geo-locations identified within the text
   ## clustered_impact_locations: based on the cluster, select a cluster
   ## using a specific cutoff value
   #############################################################################
   targets::tar_target(
     geonames_data_file,
     "R/data/geonames/geonames_data_w_cleaned_tokens.RData",
     format = "file"
   ),
  targets::tar_target(ner_annotations_spacy, as.data.frame(process_ner(
    clean_text_articles_removed_duplicates))),
  
  
  targets::tar_target(geonames_data, process_geonames_data(geonames_data_file)),
   
  
  
  targets::tar_target(impact_locations_b,
                      model_impact_locations_b(
                        ner_annotations_spacy = ner_annotations_spacy,
                        geonames_data = geonames_data,
                        clean_text_articles = clean_text_articles_removed_duplicates)),
   targets::tar_target(
     cluster_impact_locations,
     cluster_article_locations(impact_locations_b)),
   
  
  
  # For clustering the impact locations (see Sodoge et al. 2023), a cutoff
  #threshold for the hierarchical clustering is required in order to 
  # group together close-by locations and identify the geographical scope
  # of the reported drought impacts. Deoending on the case-study region
  # this cutoff_threshold needs to be adapted, be testing for desired 
  # granularity. For Germany, the cutoff threshold below has been found
  # to provide good results
  targets::tar_target(
    clustered_impact_locations,
     set_cluster_cutoff_threshold(cluster_impact_locations,
                                  cutoff_threshold = 61002, 
                                  geonames_data = geonames_data)),
  targets::tar_target(
     predicted_cluster_locations,
     predict_cluster_location(clustered_impact_locations, level  = "NUTS3")),
  
  
  ##############################################################################
  ### Combine locations and impacts into dataset ###############################
  ##############################################################################
   targets::tar_target(impacts_locations_dataset_b,
       merge_locations_impacts_function_b(
         predict_impact_classes = predict_impact_classes,
         predicted_cluster_locations = predicted_cluster_locations ,
         clustered_impact_locations = clustered_impact_locations,
         clean_text_articles_removed_duplicates = clean_text_articles_removed_duplicates,
         nuts_geo_data = nuts_geo_data,
         geonames_data = geonames_data))

  
  
)
