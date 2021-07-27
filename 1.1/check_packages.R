library("rjson")

# data path, by default based on today's date
# REMARK: assumes the working directory is the root of the repo
today<-format(Sys.Date(), format="%Y-%m-%d")
data_path<-file.path("data", today)

# load all packages
packages_path<-file.path(data_path, "packages")
package_files<-list.files(file.path(data_path, "packages"))

# parse all packages
for (i in 1:length(package_files)) {
  package <- fromJSON(file = file.path(packages_path, package_files[i]))
  
  # check cont_res
  cont_res<-package$result$cont_res
  if (cont_res != "Service" && cont_res != "Data set") {
    print("cont_res is invalid") 
  }
}


