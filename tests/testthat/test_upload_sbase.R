# # # # 
# # library(tidyr)
# # library(dplyr)
# # library(stringr)
# # library(fbcheck)
# # library(fbdesign)
# # # # context("Organoleptic forms test")
# # # # 
# # # # test_that("When F6 and F7 organolepticps NA logical", {
# # # # 
# # # # 
#  dbname<- "sweetpotatobase"
#  user <- "obenites"
#  password <- ";c8U:G&z:X"
#  fb <- readr::read_csv("/home/obenites/HIDAP_SB_1.0.0/fbcheck/tests/testthat/excel/18AMDPNaCRRI_final - Copy.csv")
#  names(fb) <- gsub("[[:space:]]", "", names(fb))
#  fb <- tbl_df(fb)
# # #user <- "iperezm"
# # #password <- "41954776"
# # 
# # out <- fbcheck::upload_studies(dbname= "sweetpotatobase",
# #                                # urltoken = "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/token",
# #                                # urlput=  "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/observations",
# #                                urltoken = "https://sweetpotatobase.org/brapi/v1/token",
# #                                urlput=  "https://sweetpotatobase.org/brapi/v1/observations",
# #                                user= user, password=password, dfr=fb)
# 
# 
# 
#  urltoken <- "https://sweetpotatobase.org/brapi/v1/token"
#  urlput <- "https://sweetpotatobase.org/brapi/v1/observations"
#  fb <- readr::read_csv("/home/obenites/HIDAP_SB_1.0.0/fbcheck/tests/testthat/excel/18AMDPNaCRRI_final - Copy.csv")
#  names(fb) <- gsub("[[:space:]]", "", names(fb))
#  fb <- tbl_df(fb)
#  
#  # dfr <- readr::read_csv("tests/testthat/excel/18AMDPNaCRRI_final - Copy.csv")
#  # names(dfr) <- gsub("[[:space:]]", "", names(dfr))
#  # dfr <- tbl_df(dfr)
#  # urltoken <- "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/token"
# 
# # out <- fbcheck::upload_studies(dbname= "sweetpotatobase",
# #                                # urltoken = "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/token",
# #                                # urlput=  "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/observations",
# #                                urltoken = "https://sweetpotatobase.org/brapi/v1/token",
# #                                urlput=  "https://sweetpotatobase.org/brapi/v1/observations",
# #                                user= "obenites", password=password, dfr=";c8U:G&z:X")
# 
# 
# white_list <- brapi::ba_db()
# con <- white_list[[dbname]] #get list
# con[["user"]] <- user
# con[["password"]] <- password
# dat<- data.frame(username = con$user, password = con$password,
#                  grant_type = "password", client_id = "", stringsAsFactors = FALSE)
# jsondat <- RJSONIO::toJSON(dat)
# callurl <- urltoken
# resp <- httr::POST(url = callurl,
#                    body = dat,
#                    encode = ifelse(con$bms == TRUE, "json", "form"))
# xout <- httr::content(x = resp)
# 
# #### TEST 1 SHOW  INCORRECT PASSWORD
# xout$metadata$status[[3]]$message
# #"Incorrect Password"
# ### END TEST 1
# 
# token <- xout$access_token
# 
# fbjson <- fbcheck::fbapp2json(dfr = dfr, token = con$token)
# #jsonview::json_tree_view(fbjson)
# #-----  PUT to sweetpotatobase --------------------------------------------------------------
# url <- urlput #"sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/observations"
# body<- fbjson #from fb2json
# h <- c(con$token)
# tokenName <-  'X-Auth-Token'
# names(h) <- tokenName
# res <- httr::PUT(url = url, body = body, encode = "json", timeout(450000), #timeout:3 minutes, in case of having big data frames
#                  httr::add_headers(`X-AUTH-TOKEN` = con$token))
# #xout <- httr::content(x = res)
# #txt <- ifelse(res$status == 200, " ok!", " problem!")
# out <- httr::content(res)
# # # # 
# # # # 
# # # # })