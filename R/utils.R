


# Form checker ------------------------------------------------------------

#' Organoleptic form checker
#' @param form organoleptic form
#' @param hot_file file path
#' @param sheet_name sheet name
#' @description organoleptic forms sometimes has unconsistencies or typos that we must be care before processing.
#' @author Omar Benites
#' @export
#' 

form_checker <- function(form, hot_file, sheet_name = NULL){
  
  # case 1: all the tables has missing values
  headers <- c("Number_of_panel", "Type_of_trial", "Name_of_Evaluator", "Sex", "APPEARANCE", "TASTE" ,"TEXTURE", NA, "NA")    
  #Variable <- NULL
  #remove all the header of each sub form.
  form <- dplyr::filter(form,  Variable %in% headers)
  form_bind <- dplyr::select(form, -1,-2,-3)
  form_bind <- form_bind %>% tidyr::gather(instn, value, 1:ncol(form_bind)) #tranforming all in one column
  
  if(all(is.na(form_bind$value))){ #check if all values are NA missing values (logical values)
    out <- NULL
    
  } 
  else if (all(form_bind$value=="NA")){ #check if all values are "NA" characters
    out <- NULL #
    
  } 
  else {

    wb <- openxlsx::loadWorkbook(hot_file) 
    pvs_sheet_in <- openxlsx::getSheetNames(hot_file)
    #form <- split_tidy_form(form = DF_f6) #deprecated. Just form outside function scope.
    form <- split_tidy_form(form = form) #DF_f6 was changed by form argument
    names_form <- names(form)
    
    out_table<- lapply(X = names_form, function(x) out_form_table(form[[x]])  )

    out_table <- data.table::rbindlist(out_table)
    print(out_table)
    out_table_fn <- as.data.frame(out_table)
    
    print(out_table_fn)
   
    out_table_fn <- out_table_fn %>% purrr::map_at(c(2,3,4), as.numeric) %>%  as.data.frame(.,stringsAsFactors =TRUE)
    

    
    if(is.element(sheet_name, pvs_sheet_in)){
      wb <- openxlsx::loadWorkbook(hot_file)
      openxlsx::removeWorksheet(wb, sheet = sheet_name)
      openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE)
      wb <- openxlsx::loadWorkbook(hot_file) 
    } 
    
    
    
    openxlsx::addWorksheet(wb = wb, sheetName = sheet_name, gridLines = TRUE)
    openxlsx::writeDataTable(wb,sheet = sheet_name, x = out_table_fn, colNames = TRUE, keepNA = FALSE, withFilter = FALSE)
    
    
    
    openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE)
    out <- out_table_fn
    
   }

 out
  
}



# Split the organoleptic forms into tidy forms structures -----------------

#' Split organoleptic forms
#' @param form organoleptic form
#' @description split in tiny data frames all the orgaleptic forms
#' @author Omar Benites
#' @export
#'
split_tidy_form <- function(form){
  
  #headers are used to validate the right values
  headers <- c("Number_of_panel", "Type_of_trial", "Name_of_Evaluator", "Sex", "APPEARANCE", "TASTE" ,"TEXTURE", NA, "NA")    
  form <- dplyr::filter(form,  Variable %in% headers)
  
  form_data <- form
  chunk <- 13
  n <- nrow(form_data)
  r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
  fieldbook_data_form <- split(form_data,r)
}


#' output of the organoletic form in a table
#' @param form organoleptic form
#' @description return an organized table
#' @author Omar Benites
#' @export
#'
out_form_table <- function(form){
  
  split_form <- split_tidy_form(form)
  form <- split_form[[1]]
  form <- tibble::as_data_frame(form)
  
  #Tranform data to tabular form
  ngen <- ncol(form) #number of evaluated genotyoes (cipnumber or variety) evaluated in organoleptic form
  res <- tidyr::gather(form, "INSTN", "Marks", 4:ngen)
  
  #---- Extraction of the following parameters:  (1) Name of evaluator
  # (1) Name of evaluator, # (2) Type_of_trial , # (3) Name_of_Evaluator and (4) Sex
  org_params<- res[1:4,] %>%  dplyr::select(Variable, Attributes)
  
  #Transform the long table in a spread table (line table) [variables as headers and parameters as values]
  org_params <-  org_params %>% tidyr::spread(Variable, Attributes) %>% as.list() #organoletpic params
  
  #Number of Panelist and Sex of the panelist
  PanelNo <- org_params$Number_of_panel
  Sex <- org_params$Sex
  
  #---- Extract x mark data (organoleptic votes for each variety)
  # Se agrego "NA" y NA para que filtre con esos valores. Hay algunos vectores que continene NA en forma de caracter o logico 
  # (sin comillas)
  org_marks <- res %>% dplyr::filter(Variable %in% c("APPEARANCE","TASTE","TEXTURE","NA",NA))
  
  #the number of genotypes gives us the number of repetation per block
  nrow_org_marks <- dplyr::n_distinct(org_marks$INSTN)
  
  #Filling the NA character values with the name of the variables
  org_vars <- c("APPEARANCE","TASTE","TEXTURE") %>% #vector
    rep(., each= 3 ) %>% #each 3 each attributes
    rep(., nrow_org_marks) #number of repetition for each block
  
  ##### BEGIN  TEST  Add test: number of "x" in organoleptic form number '#'
  org_marks <- dplyr::mutate(org_marks, Marks = tolower(Marks))
  
  #number of real and hipotetical x marks counted in organoleptic forms.
  real_n_xmarks <- org_marks %>% dplyr::select(Marks) %>% stringr::str_count(pattern = "x") 
  hipo_n_xmarks <- nrow_org_marks*3 
  if(real_n_xmarks == hipo_n_xmarks) {
    message <- paste("continue")
  } else {
    message <- paste("One value(s) is missing in the organoleptic form")
  }
  ##### END OF TEST 
  
  #extracting genotype names
  geno_names <- unique(org_marks$INSTN)
  
  #Replace the older variable name by org_vars values
  org_marks <- dplyr::mutate(org_marks, Variable = org_vars) %>%
                dplyr::filter(Marks %in% c('x',"X")) %>%
                dplyr::select(-Marks,-Attributes)  
  
  #Data transformation for analysis
  org_marks_table <- org_marks %>% tidyr::spread(Variable, Grade) %>% dplyr::mutate(PanelNo, Sex)
  
  
  #If one genotypes have missing data, this code automatically auto-complete the orgaleptic tidy form
  if(any(geno_names %in% org_marks$INSTN == FALSE)){
    out_geno <- setdiff(geno_names, org_marks_table$INSTN)
    out_geno <- data.frame(INSTN = out_geno, APPEARANCE=NA, TASTE=NA, TEXTURE=NA, PanelNo = PanelNo, Sex = Sex )
    org_marks_table<- rbind(org_marks_table, out_geno)
  }
  
  org_marks_table
  
  
}


# list_form <- split_tidy_form(form) -----------------

#' Return 'x' values (marks made by farmers in organoleptic forms)
#' @param vec vector
#' @param values categorical values or scales
#' @description get the x values from organoleptic forms
#' @author Omar Benites
#' @export
#' 
x_values <- function(vec,values){values[!is.na(vec)]}



#' Get x values from PVS forms
#'
#' @param form pvs form
#' @param genotypes genotypes
#' @param name_panel the panelist name
#' @param n_panel the number of panelist
#' @param sex_panel the sex of the panelist
#' @author Omar Benites
#' @description Return the form with x marks
#' @export
#' 
x_form <- function(form, genotypes = NA, name_panel=NA, n_panel=NA, sex_panel=NA){ 
  
  val <- c(5,3,1,5,3,1,5,3,1)
  appearance <- apply(form[5:7,], 1, x_values,values = val[1:3]) 
  appearance <- appearance[4:length(appearance)]
  appearance <- data.frame(appearance = unlist((appearance)))
  #rownames(appearance) <- 1:nrow(appearance)
  
  taste <- apply(form[8:10,], 2, x_values, values = val[4:6])
  taste <- taste[4:length(taste)] 
  taste <- data.frame(taste = unlist((taste)))
  #rownames(taste) <- 1:nrow(taste)
  
  texture <- apply(form[11:13,], 2, x_values, values = val[7:9])
  texture <- texture[4:length(texture)] 
  texture <- data.frame(texture = unlist((texture)))
  #rownames(texture) <- 1:nrow(texture)
  
  MAT <- cbind(appearance, taste, texture)
  #INSTN <- rownames(MAT)
  
  #MAT <- data.frame(INSTN,MAT)
  rownames(MAT) <- 1:nrow(MAT)
  
  MAT  <- as.data.frame(MAT)
  #MAT <- MAT[-c(1:3),]
  #table_form <- data.frame(INSTN = genotypes, MAT) 
  MAT <- data.frame(INSTN = genotypes, REP = name_panel, NAME = n_panel, SEX = sex_panel, MAT)
  return(MAT)
}



#' Get form parameters from PVS forms
#'
#' @param list_form list of forms
#' @author Omar Benites
#' @description Return pvs form parameters
#' @export
#' 
form_parameters <- function(list_form) {
  
  list(
    genotypes = lapply(X= 1:length(list_form), function(x) out <- as.character(names(list_form[[x]])[4:length(names(list_form[[x]]))])),
    Number_of_panel =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][1,2])),
    Name_of_Evaluator =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][3,2])),
    Sex =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][4,2]))
  )
  
}


#' FieldBookApp Data Processing --------------------------------------------
#'
#' @param fieldbook fieldbook from FieldBookApp to HIDAP
#' @author Omar Benites
#' @description Return pvs form parameters
#' @export
#'
fbapp2hidap <- function(fieldbook){

    #ToDo: warning: there is no  plot_name
    # dt <- fieldbook
    # dtPlotName_temp <- stringr::str_split_fixed(dt$plot_name, "_", 4) %>% as.data.frame() #split by first three "_"
    # names(dtPlotName_temp) <- c("abbr_user", "plot_number", "rep", "accesion_name")
    # dt$plot_name <- NULL #remove plot_name
    # dt2 <- cbind(dtPlotName_temp, dt) #Bind factors with other variables
    # 
    # ## composition of database headers or atributtes
    # library(dplyr)
    # library(tidyr)
    # dt2 <- dt2 %>% tidyr::separate(trait , c("Header", "CO_ID"), sep = "\\|")
    # library(stringr)
    # dt2$Header <- stringr::str_trim(dt2$Header, side = "both")
    # dt2$CO_ID <- stringr::str_trim(dt2$CO_ID, side = "both")
    # dt3 <- dt2 %>% tidyr::unite(TRAIT, Header, CO_ID, sep = "-")
    # 
    # #Get column numbers
    # colTr_index <- which(names(dt3) %in% c("TRAIT","value") )#tranpuesta fb
    # colOther_index <- setdiff(1:ncol(dt3), colTr_index) #the rest of columns
    # dt3 <- dt3 [, c(colOther_index, colTr_index)]
    # dt4 <- dt3 %>% tidyr::spread(TRAIT, value) #tranpose data or gather data
    # 
    # out <- dt4 %>% as.data.frame(stringsAsFactors=FALSE)
    # out
    dt <- fieldbook
    dtPlotName_temp <- stringr::str_split_fixed(dt$plot_name, "_", 4) %>% as.data.frame() #split by first three "_"
    names(dtPlotName_temp) <- c("abbr_user", "plot_number", "rep", "accesion_name")
    dt$plot_name <- NULL #remove plot_name
    dt2 <- cbind(dtPlotName_temp, dt) #Bind factors with other variables
    
    ## composition of database headers or atributtes
    #abbre_user_give + #plot_number+ #rep/block+ #accesion_name(germoplasm_name)
    #library(dplyr)
    #library(tidyr)
    #dt2 <- data.frame(trait = dt$trait)
    dt2 <- dt2 %>% tidyr::separate(trait , c("Header", "CO_ID"), sep = "\\|")
    
    #ToDo 1: remove white spaces in values for all columns.
    
    dt2$Header <- stringr::str_trim(dt2$Header, side = "both")
    dt2$CO_ID <- stringr::str_trim(dt2$CO_ID, side = "both")
    
    #dt3 <- dt2 %>% mutate(TRAIT = paste(Header, "_", CO_ID, sep = ""))
    #ToDo: after create TRAIT column, remove: Header and CO_ID
    dt3 <- dt2 %>% tidyr::unite(TRAIT, Header, CO_ID, sep = "-")
    
    dt3<- dt3 %>% unite(super_plot_name, abbr_user, plot_number, rep, accesion_name , timestamp, person ,location ,number, sep = "--")
    dt4<- dt3 %>% dplyr::group_by(super_plot_name, TRAIT) %>% 
                  dplyr::mutate(id= 1:n() ) %>%
                  data.table::melt(id=c("super_plot_name", "id", "TRAIT")) %>%
                  data.table::dcast(... ~ TRAIT + variable, value.var="value")
    col_names <- gsub(pattern =  "_value", replacement = "", names(dt4))
    colnames(dt4) <- col_names
    dt5<- dt4 %>% tidyr::separate( super_plot_name, c("abbr_user", "plot_number", "rep", "accesion_name" , "timestamp", "person" ,"location" ,"number"), sep= "--")
    out <- dt5

}


#' FieldBookApp Data Processing --------------------------------------------
#'
#' @param fieldbook fieldbook from HIDAP to FieldBookApp
#' @author Omar Benites
#' @description Return pvs form parameters
#' @export
#'
hidap2fbApp <- function(fieldbook) {
    #ToDo: warning: there is no  abbr_user, plot_number, rep, accesion_name columns
      
     fbdb <- fieldbook 
    # fbdb1 <- fbdb %>% tidyr::unite(plot_name, abbr_user, plot_number, rep, accesion_name, sep = "_")
    # trait_names <- names(fbdb1)[grepl("CO", x = names(fbdb1))]
    # fbdb2 <- fbdb1 %>% tidyr::gather_("trait", "value", names(fbdb1)[grepl("CO", x = names(fbdb1))])
    # fbdb2$trait <-  str_replace_all(fbdb2$trait, pattern = "-", "|" )
    # fbdb2
     fbdb1 <- fbdb %>% tidyr::unite(super_plot_name, abbr_user, plot_number, rep, accesion_name , timestamp, person ,location ,number, sep = "--")
     
     #fbdb1 <- fbdb %>% tidyr::unite(plot_name, abbr_user, plot_number, rep, accesion_name, sep = "_")
     trait_names <- names(fbdb1)[grepl("CO", x = names(fbdb1))]
     fbdb2 <- fbdb1 %>% tidyr::gather_("trait", "value", names(fbdb1)[grepl("CO", x = names(fbdb1))])
     fbdb2$trait <-  stringr::str_replace_all(fbdb2$trait, pattern = "-", "|" )
     #head(fbdb2)
     fbdb3 <- fbdb2 %>% tidyr::separate( super_plot_name, c("abbr_user", "plot_number", "rep", "accesion_name" , "timestamp", "person" ,"location" ,"number"), sep= "--")
     fbdb3 <- fbdb3 %>% tidyr::unite(plot_name, abbr_user, plot_number, rep ,accesion_name)   
     fbdb3<- dplyr::filter(fbdb3, value!="NA") 
     out <- fbdb3  
}

#' Update rhandsontable
#' @param fieldbook field data trough rhandsontable
#' @description get updates from rhandonstable after user modifications
#' @author Omar Benites 
#' @export

rhandsontable_update<- function(fieldbook){
  fb <- as.data.frame(fieldbook)
  temp <-fb
  out <- temp 
}


#' Convert FieldbookApp data to json structures
#' 
#' @param dfr data.frame 
#' @description FieldbookApp files (csv) should be transformed into json files in order to upload into Sol genomics databases.
#' @author Omar Benites
#' @export
# @param database character Choose a database at which you are extracting data.

fbapp2json <- function(dfr){
  
  headers<- c("plot_name", "plot_id", "block_number", "plot_number", "rep_number" , "row_number", "col_number",
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
  
  #continue ensemble the exp_cols and co_cols : fieldbook
  fb_h<- c("plot_id", names(co_cols)) #fb_headers
  fb<- cbind(exp_cols, co_cols)
  fb<- fb[fb_h]
  fb <- as.data.frame(fb, stringsAsFactors =FALSE)
  names(fb) <- gsub(pattern = ".*\\|",replacement = "", x = names(fb) )
  
  #tranpose data
  tfb<- fb %>% tidyr::gather(observationVariableDbId, value, 2:ncol(fb))
  tfb[,"value"]<- as.character(tfb[, "value"]) #Brapi format
  
  #Bryan says: remove Values equal to NA. Only upload complete cases.
  tfb <- tfb %>% dplyr::filter(complete.cases(.))
  
  #rename first column for: "observationUnitDbID" (brapi standard)
  names(tfb)[1] <- "observationUnitDbID"
  tfb[,"observationUnitDbID"]<- as.character(tfb[, "observationUnitDbID"]) #Brapi format
  
  #Include access_token and Observations in the json format
  tfb2list <- list(access_token= "RbgKDBRxmkdopsa2f40", Observations = tfb)#pass data.frame as element of the list
  #tfb2list <- list(observations = tfb)#pass data.frame as element of the list
  
  #list To Json
  list2json <- jsonlite::toJSON(tfb2list)
  
}


