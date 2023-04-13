#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# File: Web Scraping for Health Facilities
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will webscrape through data on Ethiopian health facilities through the following steps:
# 1. Obtain the list of Health Facility IDs.
# 2. Iterate over that ID list to open each unique ID's webpage and extract the relevant information.

rm(list = ls())

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  
  rvest, httr, xml2, rjson
  
)

## Setting file paths for each user

# Bhavya Srivastava

if (Sys.getenv("USERNAME")== "wb598052"){
  
  main_dp_path  <- "C:/Users/wb598052/Dropbox/Ethiopia Health Facility/"
  code_filepath <- paste0(main_dp_path,"Code/")
  data_filepath <- paste0(main_dp_path,"Data/")
  
}

#%%%%%%%%%%%%%%%%%%%%%
# 1. Get IDS 
#%%%%%%%%%%%%%%%%%%%%%

id_url <- "https://mfr-be.k8s.sandboxaddis.com/api/Facility/GetFacilities"

# This will get 1000 IDs from each page
get_nth_set_of_1000_ids <- function(n){
  response = POST(
    url = id_url, 
    body = list(pageNumber = n, showPerPage = 1000), 
    encode = "json",
    accept_json(), content_type_json(),
    verbose()
  )
  
  # This parses the response as an R object, it is also possible to get raw text and save it.
  id_data = httr::content(response, as = "parsed")
  ids <- purrr::map_chr(id_data[[3]], ~as.character(.x$id))
  return(ids)
}

# This gets all the IDs by iterating as mentioned above
all_ids <- purrr::map(
  1:47,
  function(n){
    # Sleep a random number of seconds
    Sys.sleep(sample(5:15, 1)) 
    return(get_nth_set_of_1000_ids(n))
  }
)

all_ids <- unlist(all_ids)

# Checking for duplicated ids
if (any(duplicated(all_ids))) {
  print("There are duplicates in the ID vector.")
} else {
  print("There are no duplicates in the ID vector.")
}

#confirming length of the vector
length(all_ids)
#[1] 46276

# saving the list of IDs
saveRDS(all_ids,paste0(data_filepath,"list_of_facility_ids.RDS"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Get Facility Information 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

detail_url <- "https://mfr-be.k8s.sandboxaddis.com/api/facility/?id="


# Function to get data for each facility ID
get_detail_data <- function(id){
  url = paste0(detail_url, id)
  # Parsed as R object
  detail_data = content(GET(url), as = "parsed")
  return(detail_data)
}

# Iterate over all IDs by breaking it into groups of 3000
lapply(seq(1,47,3),
       function (x) {
         
         # setting the range of IDs to extract
         if(x == 1) {
           min = 1
           max = 3000
         } else { min = ((x-1)*1000) + 1
         max = min((x+2)*1000,length(all_ids))}
         
         # extracting IDs
         all_data <- purrr::imap(
           all_ids[min:max],
           # Sleep a random number of seconds
           function(id, i){
             Sys.sleep(runif(1, 1, 3.5)) 
             return(get_detail_data(id))
           }
         )
         
         saveRDS(all_data, 
                 paste0(data_filepath,
                        "list_health_facility_data_",min,"_",max,".RDS"
                 )
         )
         
       }
)

