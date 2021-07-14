#READING CKAN json from API FOR transportdata.be
#Version: V1.0
#Date: 09.07.2021
#By: phe
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

#2. resource_df: build a dataframe of resource information's

#Get CKAN package names by con_pckg_names 
con_pckg_names <- url("https://transportdata.be/api/3/action/package_list")  # Authorization: 1968ca3f-51cd-4a9c-9b38-3d5003d2688e NOT NECESSARY
pckg_names <- jsonlite::fromJSON(con_pckg_names)    #, simplifyDataFrame = TRUE
pckg_names <- pckg_names$result
pckg_nbr <- (length(pckg_names))

#obs_resources <- NULL
#resource_field_names <- NULL
resource_data <- NULL
resource_df <- as.data.frame(NULL)
#Get every package_data by con_pckg 
for (ii in 1:pckg_nbr) {     #pckg_nbr
  con_resource <- url(paste("https://transportdata.be/api/3/action/package_show?id=",pckg_names[ii], sep=""))
  resource_data <- jsonlite::fromJSON(con_resource, simplifyVector = TRUE)
  resource_data <- resource_data$result$resources
  
  resource_df <- bind_rows(resource_df, resource_data)
  }

# replace colnames of resource_df with prefix "res_"
colnames(resource_df)[which(names(resource_df) == "package_id")] <- "pckg_id"
colnames(resource_df)[which(names(resource_df) == "id")] <- "res_id"
colnames(resource_df)[which(names(resource_df) == "name")] <- "res_name"
colnames(resource_df)[which(names(resource_df) == "size")] <- "size"
colnames(resource_df)[which(names(resource_df) == "url")] <- "res_url"
colnames(resource_df)[which(names(resource_df) == "size")] <- "res_size"
colnames(resource_df)[which(names(resource_df) == "state")] <- "res_state"
colnames(resource_df)[which(names(resource_df) == "description")] <- "res_description"
colnames(resource_df)[which(names(resource_df) == "format")] <- "res_format"
colnames(resource_df)[which(names(resource_df) == "url_type")] <- "res_url_type"
colnames(resource_df)[which(names(resource_df) == "mimetype")] <- "res_mimetype"
colnames(resource_df)[which(names(resource_df) == "created")] <- "res_created"
colnames(resource_df)[which(names(resource_df) == "last_modified")] <- "res_last_modified"
colnames(resource_df)[which(names(resource_df) == "revision_id")] <- "res_revision_id"

pckg_num_resources_df <- resource_df %>%
  select(pckg_id) %>%
  group_by(pckg_id) %>%
  summarise(n())
names(pckg_num_resources_df)[2] <- "num_resources"

pckg_df <- left_join(pckg_df, pckg_num_resources_df, by = "pckg_id")

pckg_X_resource_df <-  full_join(pckg_df, 
                                 resource_df %>%
                                   select(res_id, pckg_id, resource_language, res_size, acc_con, res_state, res_format, acc_gra,acc_enc, acc_int, 
                                          res_name, res_created, res_url, res_last_modified, res_revision_id),
                                 by = "pckg_id")

#EXPORT RESULTS
export(pckg_df,file = "pckg_df_20210709.csv", format = "csv")
#export(resource_df,file = "resource_df_20210709.csv", format = "csv")  #unexplained error!!!!!
export(pckg_X_resource_df,file = "pckg_X_resource_df_20210709.csv", format = "csv")


#ORGANIZATION_LIST
con_org_names <- url("https://transportdata.be/api/3/action/organization_list")  # Authorization: 1968ca3f-51cd-4a9c-9b38-3d5003d2688e NOT NECESSARY
org_names <- jsonlite::fromJSON(con_org_names)    #, simplifyDataFrame = TRUE
org_names <- org_names$result
org_nbr <- (length(org_names))

#Get every org_data by con_org 
org_data <- NULL
org_df <- as.data.frame(NULL)
obs <- NULL
for (i in 1:org_nbr) {     
  con_org <- url(paste("https://transportdata.be/api/3/action/organization_show?id=",org_names[i], sep=""))
  org_data <- jsonlite::fromJSON(con_org, simplifyVector = TRUE)
  org_data <- org_data$result
  
  obs <- c(org_data$name, org_data$id, org_data$display_name, org_data$title, org_data$description, org_data$type, org_data$state, org_data$approval_status, org_data$do_website, org_data$do_email, org_data$do_address, org_data$do_tel, org_data$created, org_data$is_organization, org_data$revision_id, org_data$package_count)
  names(obs) <- c("name", "id", "display_name", "title", "description", "type", "state", "approval_status", "do_website", "do_email", "do_address", "do_tel", "created", "is_organization", "revision_id","package_count")
  org_df <- bind_rows(org_df, obs)
}

export(org_df,file = "org_df_20210709.csv", format = "csv")

#STAKEHOLDERS_LIST
stakeholders_df <- read.csv("stakeholderslist v1.3_org.csv", encoding =  "UTF-8")

stakeholders_df$Organisation <- as.factor(stakeholders_df$Organisation)
stakeholders_df$User.type <- as.factor(stakeholders_df$User.type)
stakeholders_df$Data.supplier <- as.factor(stakeholders_df$Data.supplier)
stakeholders_df$Data <- as.factor(stakeholders_df$Data)
stakeholders_df$Regio <- as.factor(stakeholders_df$Regio)
stakeholders_df$website <- as.factor(stakeholders_df$website)
stakeholders_df$geregistreerd <- as.factor(stakeholders_df$geregistreerd)
stakeholders_df$DA.stakeholder <- as.factor(stakeholders_df$DA.stakeholder)

#ANALYZE DATA

plot(stakeholders_df$User.type)
plot(stakeholders_df$Data.supplier)
stat(stakeholders_df$Data.supplier)

stakeholders_df_link2 <- NULL
stakeholders_df_link1 <- left_join(stakeholders_df, org_df, by = c("Organisation" = "title"))
stakeholders_df_link2 <- left_join(stakeholders_df, org_df, by = c("website" = "do_website"))
