library(tidyr)
library(dplyr)
library(stringr)
library(fbcheck)
library(fbdesign)
library(data.table)

context("Test combination of multiple fieldbook App files")

test_that("Combination of two files with same headers", {
  
  file <- rprojroot::find_testthat_root_file("excel","combine_fb_1.rds")
  dfr <- readRDS(file=file)  
  fb <- fbapp2json(dfr, token="lfsermmo93;3r")
  testthat::expect_equal(class(fb),"json")
  
})

test_that("Combination of two files with same headers", {
  
  file1 <- rprojroot::find_testthat_root_file("excel/29-10-2019","2019-10-28-04-02-34_19AGGSerere_layout_table.csv")
  file2 <- rprojroot::find_testthat_root_file("excel/29-10-2019","2019-10-28_AGGSerere_layout_table.csv")
  dfr1<- readr::read_csv(file1)
  dfr2<- readr::read_csv(file2)
  dl<- list(dfr1, dfr2)
  fb <- data.table::rbindlist(dl)
  class(fb)<- "data.frame"
  dup_values <- paste(ck_duplicate(fb,"plot_id"),collasep=", ")
  testthat::expect_equal(57,object = length(dup_values) )
  
})