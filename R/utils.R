# # fp <- file.choose()
# # #fp <- 
# # sheets <- readxl::excel_sheets(fp)
# # form <- readxl::read_excel(fp,"F6_organoleptic_mother")
# # # headers <- c("Number_of_panel", "Type_of_trial", "Name_of_Evaluator", "Sex", "APPEARANCE", "TASTE" ,"TEXTURE", NA)    
# # # form <- dplyr::filter(form,  filter = Variable %in% headers)
# # 
# # ##dplyr labour
# # 
# # form <- form <- readxl::read_excel(fp,"F7_organoleptic_baby")
# # 
# # 
# 
# #fn <- list_form[[1]] 
# 
# # Number_of_panel<- as.character(fn[1,2])
# # Name_of_Evaluator <- as.character(fn[3,2])
# # Sex <- as.character(fn[4,2])
# # m <- names(fn) %>% length()
# # genotypes <- names(fn)[4:m]
# 
# # gather_fn <-  fn %>% gather( INSTN, VALUE, CIP395112.32:CIP800048)
# # val <- c(5,3,1,5,3,1,9,6,3) %>% as.character()
# # mark <- c("x","X")
# # res <- dplyr::filter(gather_fn,  Grade %in% val, VALUE %in% marks)
# 
#   
# #out <- lapply(x in 1:length(list_form), function(x) x_form(form = list_form[[i]]),genotypes = NA, name_panel=NA, n_panel=NA, sex_panel=NA    )
# 
# # out <- lapply(1:length(list_form), function(x) out <- x_form(form = list_form[[x]], genotypes = genotypes[[x]], 
# #                                                             name_panel = Name_of_Evaluator[[x]],
# #                                                             n_panel = Number_of_panel[[x]] , 
# #                                                             sex_panel = Sex[[x]]))
# 
# #full_table_form <- rbindlist(out, use.names=TRUE, fill=TRUE) %>% as.data.frame()
# 
#  
# #fp <- file.choose()
# 
#  
# split_tidy_form <- function(form){
#    
#    #headers are used to validate the right values
#    headers <- c("Number_of_panel", "Type_of_trial", "Name_of_Evaluator", "Sex", "APPEARANCE", "TASTE" ,"TEXTURE", NA)    
#    form <- dplyr::filter(form,  filter = Variable %in% headers)
#    
#    form_data <- form
#    chunk <- 13
#    n <- nrow(form_data)
#    r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
#    fieldbook_data_form <- split(form_data,r)
#  }
#  
# list_form <- split_tidy_form(form) 
#  
# x_values <- function(vec,values){values[!is.na(vec)]}
# 
# x_form <- function(form, genotypes = NA, name_panel=NA, n_panel=NA, sex_panel=NA)
# { 
#   val <- c(5,3,1,5,3,1,9,6,3)
#   appearance <- apply(form[5:7,], 2, x_values,values = val[1:3]) 
#   appearance <- appearance[4:length(appearance)]
#   appearance <- data.frame(appearance = unlist((appearance)))
#   #rownames(appearance) <- 1:nrow(appearance)
#   
#   
#   taste <- apply(form[8:10,], 2, x_values, values = val[4:6])
#   taste <- taste[4:length(taste)] 
#   taste <- data.frame(taste = unlist((taste)))
#   #rownames(taste) <- 1:nrow(taste)
#   
#   
#   texture <- apply(form[11:13,], 2, x_values, values = val[7:9])
#   texture <- texture[4:length(texture)] 
#   texture <- data.frame(texture = unlist((texture)))
#   #rownames(texture) <- 1:nrow(texture)
#   
#  
#   
#   MAT <- cbind(appearance,taste,texture)
#   INSTN <- rownames(MAT)
#   
#   MAT <- data.frame(INSTN,MAT)
#   rownames(MAT) <- 1:nrow(MAT)
#   
#   MAT  <- as.data.frame(MAT)
#   #MAT <- MAT[-c(1:3),]
#   #table_form <- data.frame(INSTN = genotypes, MAT) 
#   MAT <- data.frame(INSTN = genotypes, REP = n_panel, NAME = name_panel, SEX = sex_panel, MAT)
#   return(MAT)
# }
# 
# form_parameters <- function(list_form) {
#   list(
#     genotypes = lapply(X= 1:length(list_form), function(x) out <- as.character(names(list_form[[x]])[4: length(names(list_form[[x]]))])),
#     Number_of_panel =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][1,2])),
#     Name_of_Evaluator =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][3,2])),
#     Sex =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][4,2]))
#   )
# }
# 
# 
# 
# out_form_table <- function(list_form,  list_genotypes, list_panel_name, list_panel_number, list_panel_sex){
# 
#     out <- lapply(1:length(list_form), function(x) out <- x_form(form = list_form[[x]], genotypes = list_genotypes[[x]], 
#                                                                  name_panel =list_panel_name[[x]],
#                                                                  n_panel = list_panel_number[[x]] , 
#                                                                  sex_panel = list_panel_sex[[x]]))
#     
#     full_table_form <- rbindlist(out, use.names=TRUE, fill=TRUE) %>% as.data.frame()
#     
#     full_table_form
# 
# }
# 
# out_table <- out_form_table(list_form = list_form, 
#                       list_genotypes = out$genotypes, 
#                       list_panel_name = out$Number_of_panel , 
#                       list_panel_number = out$Name_of_Evaluator,
#                       list_panel_sex =out$Sex) 
# 
# 
# 
# 



# Split the organoleptic forms into tidy forms structures -----------------

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

# Row-bind of organoleptic forms

out_form_table <- function(form){
  
  split_form <- split_tidy_form(form)
  form <- split_form[[1]]
  form <- tibble::as_data_frame(form)
  
  #Tranform data to tabular form
  res <- gather(form, "INSTN", "Marks", 4:8)
  
  #---- Extraction of the following parameters:  (1) Name of evaluator
  # (1) Name of evaluator, # (2) Type_of_trial , # (3) Name_of_Evaluator and (4) Sex
  org_params<- res[1:4,] %>% select(Variable, Attributes)
  
  #Transform the long table in a spread table (line table) [variables as headers and parameters as values]
  org_params <-  org_params %>% spread(Variable, Attributes) %>% as.list() #organoletpic params
  
  #Number of Panelist and Sex of the panelist
  PanelNo <- org_params$Number_of_panel
  Sex <- org_params$Sex
  
  #---- Extract x mark data (organoleptic votes for each variety)
  org_marks <- res %>% filter( Variable %in% c("APPEARANCE","TASTE","TEXTURE","NA") )
  
  #the number of genotypes gives us the number of repetation per block
  nrow_org_marks <- n_distinct(org_marks$INSTN)
  
  #Filling the NA character values with the name of the variables
  org_vars <- c("APPEARANCE","TASTE","TEXTURE") %>% #vector
    rep(., each= 3 ) %>% #each 3 each attributes
    rep(., nrow_org_marks) #number of repetition for each block
  
  ##### BEGIN  TEST  Add test: number of "x" in organoleptic form number '#'
  org_marks <- mutate(org_marks, Marks = tolower(Marks))
  #number of real and hipotetical x marks counted in organoleptic forms.
  real_n_xmarks <- org_marks %>% select(Marks) %>% str_count(pattern = "x") 
  hipo_n_xmarks <- nrow_org_marks*3 
  if(real_n_xmarks == hipo_n_xmarks) {
    print("continue")
  } else {
    print("One value(s) is missing in the organoleptic form")
  }
  ##### END OF TEST 
  
  #Replace the older variable name by org_vars values
  org_marks <- mutate(org_marks, Variable = org_vars) %>%
    filter(Marks %in% c('x',"X")) %>%
    select(-Marks,-Attributes)  
  
  org_marks <- org_marks %>% spread(Variable, Grade) %>% mutate(PanelNo, Sex)
  
  
}


# list_form <- split_tidy_form(form) -----------------

# Return 'x' values (marks made by farmers in organoleptic forms) -----------------
x_values <- function(vec,values){values[!is.na(vec)]}

# Return the form with marks -----------------
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

# Return the scale values related to "x mark" -----------------
# form_parameters <- function(list_form) {
#   list(
#     genotypes = lapply(X= 1:length(list_form), function(x) out <- as.character(names(list_form[[x]])[4: length(names(list_form[[x]]))])),
#     Number_of_panel =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][1,2])),
#     Name_of_Evaluator =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][3,2])),
#     Sex =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][4,2]))
#   )
# }

# Return all the parameters values which are included into organoleptic forms -----------------
form_parameters <- function(list_form) {
  
  list(
    genotypes = lapply(X= 1:length(list_form), function(x) out <- as.character(names(list_form[[x]])[4:length(names(list_form[[x]]))])),
    Number_of_panel =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][1,2])),
    Name_of_Evaluator =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][3,2])),
    Sex =  lapply(X= 1:length(list_form), function(x) out <- as.character(list_form[[x]][4,2]))
  )
  
}

# out <- form_parameters(list_form = list_form)

# A data frame with every form formattted and tabulated -----------------
# out_form_table <- function(list_form,  list_genotypes, list_panel_name, list_panel_number, list_panel_sex){
#   
#   out <- lapply(1:length(list_form), function(x) out <- x_form(form = list_form[[x]], genotypes = list_genotypes[[x]], 
#                                                                name_panel =list_panel_name[[x]],
#                                                                n_panel = list_panel_number[[x]] , 
#                                                                sex_panel = list_panel_sex[[x]]))
#   
#   full_table_form <- rbindlist(out,fill=TRUE) #%>% as.data.frame()
#   
#   full_table_form <- as.data.frame(full_table_form)
#   full_table_form
#   
# }
# 
