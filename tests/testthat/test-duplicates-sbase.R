library(tidyr)
library(dplyr)
library(stringr)
library(fbcheck)
library(fbdesign)
library(data.table)
context("Test for checking consistency in FieldBookApp data")

test_that("Test to check duplicates in FieldBookApp data", {
  
  file1 <- rprojroot::find_testthat_root_file("excel","18BAYT_KaZARDI_harvest_tablet1.csv")
  file2 <- rprojroot::find_testthat_root_file("excel","18BAYT_KaZARDI_harvest_tablet2.csv")
  df1 <- readr::read_csv(file1)
  df2 <- readr::read_csv(file2)
  dfr <- rbindlist(list(df1,df2),fill = TRUE) %>% as.data.frame(stringsAsFactors=FALSE)
  dup <- ck_duplicate(dfr, "plot_name")  
  #case 2
  dfr2 <- data.frame()
  dup2 <- ck_duplicate(dfr2, "plot_name")  
  #case 3
  dfr3 <- data.frame(plot_name= rep(paste0("plot_","rep_",1:3),2), value= 1:6)
  dup3 <- ck_duplicate(dfr3, "plot_name")  
  
  testthat::expect_equal(length(dup), 0)
  testthat::expect_equal(length(dup2), 0)
  testthat::expect_equal(length(dup3), 3)
})