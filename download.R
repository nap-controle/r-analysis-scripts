#DOWNLOADING all CKAN json from API FOR transportdata.be
###################################################
library(RJSONIO)

# set the data path here.
data_path<-"data"

# make a directory with today's date.
today<-format(Sys.Date(), format="%Y-%m-%d")
today_data_path<-file.path(data_path, today)
dir.create(today_data_path, recursive = TRUE)

# get package_list.
package_list<-fromJSON("https://transportdata.be/api/3/action/package_list")
write(toJSON(package_list), file=file.path(today_data_path, "packages.json"))

# get all packages one-by-one.
packages_path<-file.path(today_data_path, "packages")
dir.create(packages_path, recursive = TRUE)
for (i in 1:length(package_list$result)) {     
  package_name<-package_list$result[i]
  package<-toJSON(fromJSON(paste("https://transportdata.be/api/3/action/package_show?id=",package_name, sep="")))
  
  write(package, file=file.path(packages_path, paste(package_name, ".json", sep="")))
}

# get organizations list
organizations<-fromJSON("https://transportdata.be/api/3/action/organization_list")
write(toJSON(organizations), file=file.path(today_data_path, "organizations.json"))

# get all organizations one-by-one.
organizations_path<-file.path(today_data_path, "organizations")
dir.create(organizations_path, recursive = TRUE)
for (i in 1:length(organizations$result)) {     
  organization_name<-organizations$result[i]
  organization<-toJSON(fromJSON(paste("https://transportdata.be/api/3/action/organization_show?id=",organization_name, sep="")))
  
  write(organization, file=file.path(organizations_path, paste(organization_name, ".json", sep="")))
}

