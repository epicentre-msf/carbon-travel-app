# ---------------------------
# Script name: network-analysis.R
#
# Purpose of script: network of flights available in dataset
#
# Author: Hugo Soubrier
#
# Date Created: 2024-03-20
#
# ---------------------------
# Notes:
#   
#
#
# ---------------------------

# Load packages ---------------------------

pacman::p_load(
  
  rio,          # import funcs
  fs,           # work with path
  here,         # create relative paths
  janitor,      # data cleaning
  lubridate,    # date handling
  sfnetworks, 
  tidyverse     # data science 
  
)

source(here::here("R", "set_paths.R"))
source(here::here("R", "utils.R"))

# Set paths -------------------------------------------------------------

paths <- set_paths()

sharepoint_path <- paths$sharepoint_path

raw_path <- fs::path(sharepoint_path, "Maelle CHARRIER - TOOL", "data", "raw")
clean_path <- fs::path(sharepoint_path, "Maelle CHARRIER - TOOL", "data", "clean")




