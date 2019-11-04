library(tidyr)
library(dplyr)
library(stringr)
library(fbcheck)
library(fbdesign)
context("Test ")

test_that("Detect traits in fiel book files", {
  
  file <- rprojroot::find_testthat_root_file("excel/29-10-2019","19AGGSerere_layout.csv")
  fb <- readr::read_csv(file) 
  res<- all(grepl("CO_", names(fb))!=TRUE)
  
  testthat::expect_true(object = res)
  
})