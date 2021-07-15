#READING CKAN json from API FOR transportdata.be
#Version: V1.0
#Date: 09.07.2021
#By: phe, ba
###################################################
library(pacman)
pacman::p_load(httr,jsonlite,dplyr,tidyverse,rio)

#1. pckg_df: build a dataframe of package informations

#Get CKAN package names by con_pckg_names 
con_pckg_names <- url("https://transportdata.be/api/3/action/package_list")  # Authorization: 1968ca3f-51cd-4a9c-9b38-3d5003d2688e NOT NECESSARY
pckg_names <- jsonlite::fromJSON(con_pckg_names)    #, simplifyDataFrame = TRUE
pckg_names <- pckg_names$result
pckg_nbr <- (length(pckg_names))

pckg_df <- NULL
obs <- NULL
pckg_field_names <- NULL
resource_data <- NULL
#Get every package_data by con_pckg 
for (i in 1:pckg_nbr) {     
  con_pckg <- url(paste("https://transportdata.be/api/3/action/package_show?id=",pckg_names[i], sep=""))
  pckg_data <- jsonlite::fromJSON(con_pckg, simplifyVector = TRUE)
  pckg_data <- pckg_data$result
  obs <- c(pckg_data$id,  
           pckg_data$name, 
           pckg_data$metadata_created, 
           pckg_data$metadata_modified,
           pckg_data$notes_translated$fr,
           pckg_data$notes_translated$de,
           pckg_data$notes_translated$nl,
           pckg_data$notes_translated$en,
           pckg_data$cont_res,
           pckg_data$contact_name,
           pckg_data$organization$name,
           pckg_data$organization$id,
           pckg_data$publisher_name,
           pckg_data$p_address,
           pckg_data$publisher_email,
           pckg_data$publisher_url,
           pckg_data$p_tel,
           pckg_data$temporal_start,
           pckg_data$license_id,
           pckg_data$license_title,
           pckg_data$contract_license,
           pckg_data$isopen,
           pckg_data$frequency,
           pckg_data$theme,
           pckg_data$revision_id,
           pckg_data$fluent_tags,
           pckg_data$num_tags,
           pckg_data$language,
           pckg_data$agreement_declaration)
  
  #Problem's to solve
  #pckg_data$temporal_end,  All empty!
  #pckg_data$num_resources, DONE!!
  #pckg_data$countries_covered, MISSING IN SOME RECORDS?
  #pckg_data$regions_covered, MISSING IN SOME RECORDS?
  #pckg_data$spatial MISSING IN MANY RECORDS BUT NOT MANDATORY?
  
  pckg_df <- rbind.data.frame(pckg_df,obs)
  
}

pckg_field_names <- c("pckg_id", 
                      "pckg_name", 
                      "metadata_created", 
                      "metadata_modified",
                      "notes_translatedfr",
                      "notes_translatedde",
                      "notes_translatednl",
                      "notes_translateden",
                      "cont_res",
                      "contact_name",
                      "organization_name",
                      "organization_id",
                      "publisher_name",
                      "p_address",
                      "publisher_email",
                      "publisher_url",
                      "p_tel",
                      "temporal_start",
                      "license_id",
                      "license_title",
                      "license_contract_license",
                      "license_isopen",
                      "frequency",
                      "theme",
                      "revision_id",
                      "fluent_tags",
                      "num_tags",
                      "language",
                      "agreement_declaration")

colnames(pckg_df) <- pckg_field_names