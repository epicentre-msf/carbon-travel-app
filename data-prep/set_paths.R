
set_paths <- function() { 
  
  if(Sys.getenv("EPI_ONEDRIVE_PATH") == "") {
    
    stop("ERROR : the EPI_ONEDRIVE_PATH variable does not exist in .Renviron - please make sure it is created (see ?usethis::edit_r_environ() )")
    
  }
  #get onedrive path from .Renviron
  onedrive_path <- Sys.getenv("EPI_ONEDRIVE_PATH")
  
  names(onedrive_path) <- "onedrive_path"
  
  #list all sync projects in onedrive and name the item of list
  proj <- c(fs::dir_ls(onedrive_path))
  
  names(proj) <- stringr::str_remove(names(proj), "(.*/)" )
  
  proj_ls <- as.list(proj)
  
  #create the object to be returned
  paths <- append(
    
    onedrive_path, 
    
    proj_ls
    
    )
  
  return(paths)
  
}

get_sp_data_path <- function() {
  user <- Sys.info()[["user"]]
  if (user == "paul") {
    fs::path(
      Sys.getenv("SHAREPOINT_PATH"),
      "Maelle CHARRIER - Carbon-travel-App",
      "Data"
    )
  # add your user here
  } else {
    fs::path(
      Sys.getenv("SHAREPOINT_PATH"),
      "Maelle CHARRIER - TOOL",
      "Data"
    )
  }
}

#paths <- set_paths()

# set_paths <- function(info = Sys.info()) {
#   
#   # this script uses the .Renviron variable EPI_ONEDRIVE_PATH to get the onedrive paths
#   
#   
#   if (info["user"] == "ntncmch") {
#     # paths for Anton's laptop
#     paths <- list(
#       sharepoint = "~/Library/CloudStorage/OneDrive-SharedLibraries-MSF"
#     )
#     
#     # } else if (info["user"] == "ADD YOUR USERNAME HERE") {
#     #   paths <- list(
#     #     sharepoint = "ADD YOUR PATH HERE"
#     #   )
#     
#   } else {
#     
#     warning("User and computer not recognized, please edit R/set_paths.R.")
#     
#   }
#   
#   return(paths)
# }


