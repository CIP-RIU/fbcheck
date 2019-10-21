library(tidyr)
library(dplyr)
library(stringr)
library(fbcheck)
library(fbdesign)
context("Test combination of multiple fieldbook App files")

test_that("Combination of two files with same headers", {
  
  file <- rprojroot::find_testthat_root_file("excel","combine_fb_1.rds")
  dfr <- readRDS(file=file)  
  fb <- fbapp2json(dfr, token="lfsermmo93;3r")
  testthat::expect_equal(class(fb),"json")
  
})