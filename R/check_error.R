
#' Check error in FieldbookApp data
#' 
#' @param dfr data frame. FieldbookApp data.
#' @description FieldbookApp data is captured by mobiles phones or tablets. After exporting this information, it should be read in R
#' in order to process, check and curate.
#' @author Omar Benites
#' @export


check_fbapp <- function(dfr){
  
  #Solgenomic headers
  sol_headers<- c("plot_name", "plot_id", "block_number", "plot_number", "rep_number" , "row_number", "col_number",
                  "accession_name",  "is_a_control", "synosyms", "trial_name", "location_name", "year", "pedigree",
                  "tier", "seedlot_name", "seed_transaction_operator", "num_seed_per_plot", "range_number", "plot_geo_json",
                  "timestamp",	"person"	,"location",	"number")
  
  #fieldbook headers
  fb_headers <- names(dfr)
  #Crop Ontology (CO) headers
  co_h_lg <- grepl(pattern = "CO", fb_headers) #logical exp. to detect co_headers
  co_cols <- dfr[co_h_lg] #detect Crop ontology columns 
  #Experiment columns: -get rid trait variables and retain experimental variables (plot, rep, year, etc)
  exp_cols <- dfr[!co_h_lg]
  #ToDo: create two functions for check ontology terms (exist) and exp sol_headers
  check_headers <- names(exp_cols) %in% sol_headers  
  
  
  if(nrow(dfr)==0){
    msg <- paste("There have been no changes in the dataset")
    status <- "error"
  } else if(!is.element("plot_id", fb_headers)){ #Check #1
    msg <- paste("The variable 'plot_id' is missing. Must be included in order to upload into the database")
    status <- "error"
  } else if(sum(names(exp_cols) %in% sol_headers) != length(names(exp_cols))) {
    #Check #2
    non_found<- names(exp_cols)[!check_headers]
    msg <- paste("The variable(s)", non_found, "was (were) not found in the database. Refine your file before processing.")
    status <- "error"
  } else { #Check #3
    msg <-  paste("Great! Dataset successfully uploaded in SweetPotatoBase. ")
    status <- "success"
  }
  
  out<- list(msg= msg, status=status)
  
}



#' Check credentials from sweetpotatobase users.
#' 
#' @param dbname character Database name. Currently, it only works with SOL genomics databases.
#' @param user character User name
#' @param password character Password
#' @param urltoken character \code{BRAPI} call URL  to  login in Sol Genomic databases.
#
#' @description FieldbookApp data is captured by mobiles phones or tablets. After exporting this information, it should be read in R
#' in order to process, check and curate.
#' @author Omar Benites
#' @export 
#' 
check_credentials <- function(dbname= "sweetpotatobase", user="obenites", password="dasdfsdgs",
                              urltoken= "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/token"){
  
  white_list <- brapi::ba_db()
  con <- white_list[[dbname]] #get list
  con[["user"]] <- user
  con[["password"]] <- password
  dat<- data.frame(username = con$user, password = con$password, 
                   grant_type = "password", client_id = "", stringsAsFactors = FALSE)
  jsondat <- RJSONIO::toJSON(dat)
  callurl <- urltoken
  resp <- httr::POST(url = callurl,
                     body = dat,
                     encode = ifelse(con$bms == TRUE, "json", "form"))
  xout <- httr::content(x = resp) 
  
  code <- xout$metadata$status[[3]]$code %>% as.numeric()
  
  if(code==200){
    msg <- paste("Login credentials are correct.")
    status <- "success"
  } else {  
    msg <- paste("Login credentials are incorrect. Please try again.")
    status <- "error" 
  } 
  out<- list(msg= msg, status=status)
}

