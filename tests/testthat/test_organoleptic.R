context("Organoleptic forms test")
library(tidyr)
library(dplyr)
library(stringr)
library(fbcheck)
#fp <- file.choose()
# fp <- "PTPVS032013_Jumla_exp3_VACIO.xlsx"
# form <- readxl::read_excel(fp, "F6_organoleptic_mother")
# saveRDS(datos, "F6_organoleptic_mother.rds")


test_that("When F6 and F7 organolepticps NA logical", {

  
  nodata <- readxl::read_excel("excel/PTPVS032013_Jumla_exp3_VACIO.xlsx", "F6_organoleptic_mother")
  nodata2 <- readxl::read_excel("excel/PTPVS032013_Jumla_exp3_VACIO.xlsx", "F7_organoleptic_baby")
  # form_na <-  readxl::read_excel("PTPVS032013_Jumla_exp3_VACIO.xlsx" , "F6_organoleptic_mother")
  # headers <- c("Number_of_panel", "Type_of_trial", "Name_of_Evaluator", "Sex", "APPEARANCE", "TASTE" ,"TEXTURE", NA, "NA")    
  #remove all the header of each sub form.
  
  testthat::expect_equal(form_checker(form = nodata, hot_file =  "excel/PTPVS032013_Jumla_exp3_VACIO.xlsx"), NULL)
  testthat::expect_equal(form_checker(form = nodata2, hot_file = "excel/PTPVS032013_Jumla_exp3_VACIO.xlsx"), NULL)

})


test_that("When F6 and F7 organolepticps NA character", {
  
  
  nodata  <- readxl::read_excel("excel/PTPVS032013_Jumla_exp3_NA_character.xlsx", "F6_organoleptic_mother")
  nodata2 <- readxl::read_excel("excel/PTPVS032013_Jumla_exp3_NA_character.xlsx", "F7_organoleptic_baby")
  # form_na <-  readxl::read_excel("PTPVS032013_Jumla_exp3_VACIO.xlsx" , "F6_organoleptic_mother")
  # headers <- c("Number_of_panel", "Type_of_trial", "Name_of_Evaluator", "Sex", "APPEARANCE", "TASTE" ,"TEXTURE", NA, "NA")    
  #remove all the header of each sub form.
  
  testthat::expect_equal(form_checker(form = nodata, hot_file = "excel/PTPVS032013_Jumla_exp3_NA_character.xlsx"), NULL)
  testthat::expect_equal(form_checker(form = nodata2, hot_file = "excel/PTPVS032013_Jumla_exp3_NA_character.xlsx"), NULL)

})



testthat::test_that("Missing values in some clones/varities",{
  
  #data <- readxl::read_excel(path = "")
  missdata  <- readxl::read_excel("excel/missingval_form18.xlsx", "F6_organoleptic_mother")
  datos <- as.data.frame(missdata)
  form <- datos
  form <- split_tidy_form(form = form) #DF_f6 was changed by form argument
  names_form <- names(form)
  #extracting the eighteen form which has missing values in one genotype
  form18 <- form[[18]]
  form_table <- out_form_table(form = form18)
  testthat::expect_equal(nrow(form_table), 20)
  
  
})


testthat::test_that("Organoleptic Baby with just one empty form",{

  #data <- readxl::read_excel(path = "")
  f7_one_emptyform  <- readxl::read_excel("excel/F7_OneEmptyForm_QUILCAS.xlsx", "F7_organoleptic_baby")
  testthat::expect_equal(form_checker(form = f7_one_emptyform, hot_file = "excel/PTPVS032013_Jumla_exp3_NA_character.xlsx"), NULL)
  
})


# test_that("Organoleptic form with some panelist evaluating some genotypes", {
# 
#   f6_evalxpanelgroup  <- readxl::read_excel(path = "excel/PTPVS112016_CANAYPATA_exp1.xlsx", sheet = "F6_organoleptic_mother")
#   datos <- as.data.frame(f6_evalxpanelgroup)
#   form <- datos
#   form <- split_tidy_form(form = form) #DF_f6 was changed by form argument
#   
#   names_form <- names(form)
#   library(tidyr)
#   library(dplyr)
#   library(stringr)
#   out_table<- lapply(X = names_form, function(x) out_form_table(form[[x]])  )
#   
#   out_table <- data.table::rbindlist(out_table)
#   print(out_table)
#   out_table_fn <- as.data.frame(out_table)
#   
#   print(out_table_fn)
#   
#   out_table_fn <- out_table_fn %>% purrr::map_at(c(2,3,4), as.numeric) %>%  as.data.frame(.,stringsAsFactors =TRUE)
#   
#   
#   
# })

