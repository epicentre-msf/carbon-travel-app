# Set paths functions 

set_paths <- function() { 
  
  if(Sys.getenv("SHAREPOINT_PATH") == "") {
    
    stop("ERROR : the SHAREPOINT_PATH variable does not exist in .Renviron - please make sure it is created (see ?usethis::edit_r_environ() )")
    
  }
  #get onedrive path from .Renviron
  sharepoint_path <- Sys.getenv("SHAREPOINT_PATH")
  
  names(sharepoint_path) <- "sharepoint_path"
  
  #list all sync projects in onedrive and name the item of list
  proj <- c(fs::dir_ls(sharepoint_path))
  
  names(proj) <- janitor::make_clean_names(stringr::str_remove(names(proj), "(.*/)" ))
  
  proj_ls <- as.list(proj)
  
  #create the object to be returned
  paths <- append(
    
    sharepoint_path, 
    
    proj_ls
    
  )
  
  return(paths)
  
}