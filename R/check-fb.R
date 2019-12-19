#' Check duplicates entries in FieldBookApp data
#' 
#' @param dfr data frame. FieldBookApp data.
#' @param attribute character. Column name for checking whether duplicates values exist in FieldBookApp data 
#' @description Check duplicated entries in specific attributes, ex. \code{"plot_name"}, in FieldBookApp data 
#' @author Omar Benites
#' @examples 
#' dfr<- data.frame(plot_name= paste0("plot_","rep_",1:3), value= 1:3)
#' ck_duplicate(dfr,"plot_name")
#' dfr2<- data.frame(plot_name= rep(paste0("plot_","rep_",1:3),2), value= 1:6)
#' ck_duplicate(dfr2,"plot_name")
#' @export


ck_duplicate <-  function(dfr=data.frame(), attribute){

  if(nrow(dfr)>0){
      #Check frequency
      tb_dup <- data.frame(table(dfr[,attribute]))
      if(nrow(tb_dup)>0){
        dup <- as.character(tb_dup[tb_dup$Freq>1,1]) 
      } else {
        dup <- NULL
      }
  } else {
    dup <- NULL
  }
  
  
}

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
  co_h_lg <- grepl(pattern = "//|CO", fb_headers) #logical exp. to detect co_headers
  co_cols <- dfr[co_h_lg] #detect Crop ontology columns 
  #Experiment columns: -get rid trait variables and retain experimental variables (plot, rep, year, etc)
  #exp_cols <- dfr[!co_h_lg]
  #dtexp: table with experimental columns
  dtexp <- dfr[!co_h_lg]
  #ToDo: create two functions for check ontology terms (exist) and exp sol_headers
  check_headers <- names(dtexp) %in% sol_headers  
  
  #Non determined variables or headers( non-CO variables and non-experimental columns)
  nod_headers <- names(dfr[,!co_h_lg])
  nod_headers <- nod_headers[!stringr::str_detect(string =  nod_headers, pattern = names(dtexp))]
  print("non headers")
  print(nod_headers)
  
  if(nrow(dfr)==0){
    msg <- paste("There have been no changes in the dataset")
    status <- "error"
    styleclass <- "danger"
  } else if(!is.element("plot_id", fb_headers)){ #Check #1
    msg <- paste("The variable 'plot_id' is missing. Must be included in order to upload into the database")
    status <- "error"
    styleclass <- "danger"
  } else if(is.element( "plot_id", names(dt))){
    msg <- paste("Plot_id column was not found. Please add it to upload in the database")
    status <- "error"
    styleclass <- "danger"
  }  else if(sum(names(dtexp) %in% sol_headers) != length(names(dtexp))) {
    #Check #2
    non_found<- names(dtexp)[!check_headers]
    #msg <- paste("The variable(s)", non_found, "was (were) not found in the database. Refine your file before processing.")
    msg <- paste("Dataset successfully uploaded in SweetPotatoBase. But the variable(s) '", paste(non_found,collapse=", "), 
                 "' was (were) not found in the database. Only traits with CO idenfier will be updated.")
    status <- "success"
    styleclass <- "success"
  } else { #Check #3
    msg <-  paste("Great! Dataset successfully uploaded in SweetPotatoBase. ")
    status <- "success"
    styleclass <- "success"
  }
  
  out<- list(msg= msg, status=status, styleclass= styleclass)
  
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

