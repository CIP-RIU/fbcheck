#' Server component for traittools and data quality for HIDAP-SweetPotatoBase
#'
#' Returns server side components for HIDAP-SweetPotatoBase
#' @author Omar Benites
#' @param input shinyserver input
#' @param output nameo of the output element
#' @param session shinyserver session
#' @param values The reactive values
#' @export

fbcheck_server_sbase <- function(input, output, session, values) {
  
  ################################ R. ARIAS###############################################
  # db <- reactiveValues()
  # db$constUserDB <- "hidap_sbase"
  # db$constPassDB <- "cKqrrEhTHLh3V2Fm70719"
  # db$constDBName <- "hidap_sbase"
  # db$constDBHost <- "176.34.248.121"
  ################################ R. ARIAS###############################################
  
  #Catch the file path for reading fieldbook sheets
  volumes <- shinyFiles::getVolumes()
  
  #----Return the type of crop in Minimal sheet -------------
  hot_crop_sbase <- reactive({
    
    #formatFile <- hot_formatFile_sbase()
    
    # if(formatFile =="HIDAP"){
    #   
    #   hot_file <- hot_path_sbase()
    #   if(length(hot_file)==0){return (NULL)}
    #   if(length(hot_file)>0){
    #     hot_param <- readxl::read_excel(path=hot_file , sheet = "Minimal")
    #     hot_crop <- get_fb_param(hot_param,"Crop")
    #   }
    # }
    
    #if(formatFile =="FieldBookApp-SPBase"){hot_crop <- "sweetpotato"}
    hot_crop <- "sweetpotato"
    hot_crop
    
  })
  
  #----FieldbookApp Path  -----------------------------------
  hot_fbapp_path <- reactive({
    
    file_fbapp <- input$file_fbapp_sbase
    out<- file_fbapp$datapath 
    
  })
  
  #---- Format of the file -----------------------------------
  fileNameExtFile <- reactive({
    
    servName <- "fbappdatapath.rds"
    uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
    ranStr <-  stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
    servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
    dirNameExtFile <- fbglobal::get_base_dir() #get directory of the file with fileName
    fileNameExtFile <-  paste0(dirNameExtFile, servName)
    
  })
  
  # Load dataset ---------------------------------------------
  fb_sbase <- function(){
    ####### Import CSV data #######
    #file_fbapp <- input$file_fbapp_sbase
    if (is.null(input$file_fbapp_sbase)) {
      #shinyjs::disable("saveData")  # Codigo R.ARIAS  SAVE
      return(NULL)
    } else {
      dt <- readr::read_csv(input$file_fbapp_sbase$datapath)  # Codigo R.ARIAS SAVE
      #shinyjs::enable("saveData")
    }
  
  }
  
  #hot_btable represents fieldbook data ----------------------
  output$hot_btable_fbapp_sbase <-  renderRHandsontable({
    
    req(input$file_fbapp_sbase)
   
    dt<- fb_sbase()
    flag <<- FALSE
    
    
    ####### Show Warnings to users   #######
    #ToDo: Include plot_id
    if(!is.element("plot_name", names(dt))){ 
      shinysky::showshinyalert(session, "alert_fbapp_warning_sbase", paste("ERROR: The file imported does not has 'plot_name' header."), styleclass = "danger")  
    } 
    else if(nrow(dt)==1){
      shinysky::showshinyalert(session, "alert_fbapp_warning_sbase", paste("ERROR: Your data file has only one row of data. Please upload the right one. "), styleclass = "danger")  
    } 
    else {
      hot_bdata_sbase2 <- dt #fbapp2hidap(fieldbook = dt)
      names(hot_bdata_sbase2) <- gsub("[[:space:]]", "", names(hot_bdata_sbase2)) #remove whitespaces
      hot_bdata_sbase2
    }
    
    ####### Create Unique ID ######## 
    servName <- "fbappdatapath.rds"
    uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
    ranStr <-  stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
    servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
    dirNameExtFile <- fbglobal::get_base_dir() #get directory of the file with fileName
    fileNameExtFile <-  paste0(dirNameExtFile, servName)
    
    fileNameExtFile<- fileNameExtFile()
    
    ####### Reactive values  #######
    hot_bdata_sbase <- hot_bdata_sbase2
    
    values <-  shiny::reactiveValues(
      hot_btable_fbapp_sbase = hot_bdata_sbase#()
    )
    DF <- NULL

    if(!is.null(input$hot_btable_fbapp_sbase)) {
      DF = hot_to_r(input$hot_btable_fbapp_sbase)
      print("if 2")
      if(file.exists(fileNameExtFile)) {
        former_datapath <- readRDS(file = fileNameExtFile)
        if(hot_fbapp_path()!= former_datapath){
          #if(!identical(hot_bdata_sbase2, DF)){
          flag <<- TRUE
          print("entro")
          DF <- hot_bdata_sbase2
        }
      }
      ###  end important note
      values[["hot_btable_fbapp_sbase"]] = DF
    } 
    else if (!is.null(values[["hot_btable_fbapp_sbase"]])) {
      print("if 2.1")
      DF = values[["hot_btable_fbapp_sbase"]]
    } 
    
    #if(!is.null(DF)){
      
      print("if 3")
    
      
      dsource <- 2
      traits <- traittools::get_trait_fb(DF, dsource = dsource)
      
      file_fbapp <- input$file_fbapp_sbase
      value_datapath <- file_fbapp$datapath 
      fileNameExtFile <- paste0(dirNameExtFile, servName) #file.path(fbglobal::get_base_dir(), "fbappdatapath.rds")
      
      saveRDS(value_datapath, file =  fileNameExtFile())
      
      crop <- hot_crop_sbase()
      trait_dict <- get_crop_ontology(crop = crop, dsource = dsource)
      traittools::col_render_trait(fieldbook = DF, trait = traits , trait_dict = trait_dict, dsource = dsource) 
      
    #}
  })
  
  #-----Upload to SweetPotatoBase------------------------------
  observeEvent(input$uploadSbase, {
    showModal(modalDialog(
      title = "HIDAP-SweetPotatoBase",
      "Submit data from HIDAP to SweetPotatoBase",
      fluidRow(
        column(
          12, br(),
          #column(6, align = "left", fluidRow(
          textInput(inputId="fbchecksbaseUser", label="",value="", 
                    placeholder = "SweetPotatoBase User", width = NULL),#)
          #       ),
          # column(6, align = "right", fluidRow( 
          passwordInput(inputId="fbchecksbasePass", label="", value = "", width = NULL,
                        placeholder = "SweetPotatoBase Password"),
          
          shinysky::shinyalert("alert_fbappsbase_upload", FALSE, auto.close.after = 4)
          
          # column(6, align = "left", fluidRow(actionButton("submitsbase", "Submit"),
          #                                    shinysky::shinyalert("alert_fbappsbase_upload", FALSE, auto.close.after = 4))
          #)
          #shinysky::shinyalert("alert_fbappsbase_upload", FALSE, auto.close.after = 4)
          #        )
        )
      ),
      easyClose = FALSE,
      footer = tagList(
        actionButton("submitsbase", "Submit"),
        modalButton("Cancel")
      )
    ))
  })
  
  
  #-----Submit data to SweetPotatoBase ---------
  observeEvent(input$submitsbase, {
    
    ### User and Password ##############################################################################
    user<- stringr::str_trim(input$fbchecksbaseUser)
    password <- stringr::str_trim(input$fbchecksbasePass)
    
    ### Load Data ######################################################################################
    if(is.null(input$hot_btable_fbapp_sbase)){
      fb <- data.frame() #there are not changes
    }else {
      fb<-  hot_to_r(input$hot_btable_fbapp_sbase)
      fb<- dplyr::tbl_df(fb)
    }
    
    if(isTRUE(flag)){
    # when users change datasets files (input$fileInput), but they do not modify the file  
      fb <- fb_sbase() 
    }
    
    ### Checking data ##################################################################################
    
   
    #print(head(fb,4))
    res<- fbcheck::check_fbapp(dfr=fb)
    
    shiny::withProgress(message = "Uploading file...", value = 0,
                        {
                          incProgress(1/6, detail = paste("Checking data..."))
                         
                          if(res$status=="error"){
                            shinysky::showshinyalert(session, "alert_fbappsbase_upload", paste(res$msg), styleclass = "danger")
                            incProgress(6/6, detail = paste("Errors detected"))
                          } 
                          else {
                            incProgress(2/6, detail = paste("Checking data..."))
                        
                            print(head(fb,n = 4))
                            print(user)
                            print(password)
                            
                           res2<- fbcheck::check_credentials(dbname= "sweetpotatobase", user=user, password=password,
                                                             #urltoken= "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/token")
                                                             urltoken= "sweetpotatobase.org/brapi/v1/token") 
                              
                           if(res2$status=="error"){
                                shinysky::showshinyalert(session, "alert_fbappsbase_upload", paste(res2$msg), styleclass = "danger")
                                incProgress(6/6, detail = paste("Errors detected"))
                           } else {
                                out <- fbcheck::upload_studies(dbname= "sweetpotatobase",
                                                     # urltoken = "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/token",
                                                     # urlput=  "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu/brapi/v1/observations",
                                                     urltoken = "https://sweetpotatobase.org/brapi/v1/token",
                                                     urlput=  "https://sweetpotatobase.org/brapi/v1/observations",
                                                     user= user, password=password, dfr=fb)
                                print("4")
                                if(out$metadata$status[[6]]$code=="200"){
                                  shinysky::showshinyalert(session, "alert_fbappsbase_upload", paste(res$msg), styleclass = "success")  
                                  print("5") 
                                  incProgress(5/6, detail = paste("Finishing upload to SweetPotatoBase..."))
                                  incProgress(6/6, detail = paste("Refreshing page..."))
                                  
                                  session$reload()
                                  
                            } 
                                else {
                                  shinysky::showshinyalert(session, "alert_fbappsbase_upload", paste("Error trying to upload the fieldbook file. Please check it."), styleclass = "danger")  
                                  incProgress(6/6, detail = paste(""))
                            }
                           
                           } #end else (ok case)
                           
                          } #else else (all cases)
                          
                        })
    
  })
  
  
  #------Export button --------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
     
     shiny::withProgress(message = 'Downloading file', value = 0, {
        incProgress(1/6, detail = paste("Reading table data..."))
        
        #DF <- hot_to_r(input$hot_btable_fbapp_sbase) # Important note: run server
        incProgress(2/6, detail = paste("Formatting hidap file..."))
        
        if(is.null(input$hot_btable_fbapp_sbase)){
          fb <- fb_sbase()
        } else {
          fb <- hot_to_r(input$hot_btable_fbapp_sbase) # Important note: run server
        }
          
      exportFormat <- input$fbcheck_fbapp_ExportFormat_sbase
        if(exportFormat=="SPBase Format"){
          names(fb)[1] <-  "observationunit_name"
          #Remove unncesary columns for simple format
          #ToDo: ask if user need 'plot_id' column in 'simple' format for sweetpotatobase
          fb$accession_name <- fb$plot_id <- 	fb$plot_number <- fb$block_number <- 	fb$is_a_control	<- fb$rep_number	<- fb$row_number <- 	fb$col_number <- NULL
          fb <- fb
          
        } 
        else {
          fb
        }
        
        incProgress(3/6, detail = paste("Downloading FieldBookApp-SPBase file..."))
        incProgress(4/6, detail = paste("Refreshing HIDAP..."))
        Sys.sleep(3)
        incProgress(5/6, detail = paste("Refreshing HIDAP..."))
        
        write.csv(fb, con, row.names = FALSE)
        
        incProgress(6/6, detail = paste("Refreshing HIDAP..."))
        Sys.sleep(5)
        #shinyjs::js$downloadData()
      })
    }
  )
  ################################## R .ARIAS ################
  # observeEvent(input$saveData,{
  #   if(!session$userData$logged){
  #     showModal(modalShowMessage("You must log in to save your data"))
  #     return()
  #   }
  #   showModal(modalEnterFileName())
  # })
  
  # observeEvent(input$btSaveModal, {
  #   saveFile()
  # })
  ################################## R .ARIAS ################
  
  # modalEnterFileName <- function(){
  #   modalDialog(
  #     title = HTML("<center><font color='#f7941d'><h2> Saving Study </h2></font></center>"),
  #     div(
  #       textInput("fileName", "File name:"),
  #       textInput("breederName", "Breeder Name:")
  #     ),
  #     
  #     easyClose = T,
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton("btSaveModal", "Save", width = 120, icon = icon("save")),
  #       actionButton("btModalSaveNUpdload", "Save & Upload", width = 120, icon=icon("upload"))
  #       )
  #   )
  # } 
  # 
  # modalShowMessage <- function(str){
  #   modalDialog(
  #     title = HTML("<center><font color='#f7941d'><h2> HiDAP says: </h2></font></center>"),
  #     div(
  #       HTML(paste0("<h5>",str, "<h5>"))
  #     ),
  #     easyClose = T,
  #     footer = tagList(
  #       modalButton("Ok")
  #     )
  #   )
  # } 
  #   
  # ### to chek if name is correct and available
  # ### for testing, we will asume correct and no duplicate names are entered
  # checkNewFileName <- function (fname){
  #   
  #   ### TO DO ###
  #   
  #   return(T)
  # }
  # 
  # saveFile <- function(){
  #   
  #   fileName <- trimws(input$fileName)
  #   breederName <- trimws(input$breederName)
  #   removeModal()
  #   
  #   message <- ""
  #   
  #   xdate <- Sys.time()
  #   
  #   v_study <- input$file_fbapp_sbase$name
  #   v_study <- gsub(".csv", "", v_study)
  #   
  #   uploadDate  <- as.character(xdate, "%Y%m%d%H%M%S")
  #   uploadDate_s <- as.character(xdate, "%Y-%m-%d %H:%M:%S")
  #   ranStr <-  stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
  #   servName <- paste(uploadDate, ranStr , sep= "-") #nombre sin extensions!!!!
  #   servName <- paste0(servName, ".csv")
  #   
  # 
  #   
  #   shiny::withProgress(message = 'Saving file', value = 0, {
  #     incProgress(1/4, detail = paste("Reading table data..."))
  #     
  #     
  #     DF <- hot_to_r(input$hot_btable_fbapp_sbase) # Important note: run server
  #     
  #     
  #     incProgress(2/4, detail = paste("Generating file..."))
  #     
  #     pathGlobal <- fbglobal::get_base_dir()
  #     servPath <-  file.path(pathGlobal,servName)
  #     
  #     write.csv(DF, file=servPath)
  #     
  #     incProgress(3/4, detail = paste("Saving into database"))
  #     Sys.sleep(3)
  #     
  #     params <- list(
  #       dataRequest = "uploadFile",
  #       fileServerName = servName,
  #       filedata=upload_file(servPath, "text/csv")
  #     )
  #     
  #     var <- POST("https://research.cip.cgiar.org/gtdms/hidap/script/hidap_sbase/getFileUpload.php", body=params)
  #     code <- content(var, "text")
  #     
  #    
  #   
  #     if (file.exists(servPath)) file.remove(servPath)
  #     
  #     if (code == "200"){
  #       
  #       message <- paste0(message, fileName, " was successfully saved <br>")
  #       saveFileToDb("Sweetpotato", servName, paste0(fileName, ".csv"), breederName, v_study ,uploadDate_s )
  #       
  #       # saveFileToDB <- function(crop, server_book_name, book_name, breeder_name, study, upload_date){
  #         
  #     }
  #     else{
  #       message = paste0( message, "Error while sharing ", fileName , ". Please Try again. <br>")
  #     }
  #     incProgress(4/4, detail = paste("Finishing"))
  #     Sys.sleep(5)
  #     
  #   })
  #   
  #   shinyalert("Success", message, type = "success")
  #     
  # }
  # 
  # saveFileToDb <- function(crop, server_book_name, book_name, breeder_name, study, upload_date){
  #   
  #   mydb = dbConnect(MySQL(), user=db$constUserDB, password=db$constPassDB, dbname=db$constDBName, host=db$constDBHost)
  #   strQry <- paste0("insert into files (owner_id, crop, breeder_name, book_name, server_book_name, study, status, date_created) values(", session$userData$userId, ",")
  #   strQry <- paste0(strQry, "'",crop ,"','",breeder_name,"','",book_name,"','", server_book_name, "','", study, "','", "In review", "','",upload_date,"')")
  #   qryUser = dbSendQuery(mydb, strQry)
  #   dbDisconnect(mydb)
  #   
  # }
  ################################## R .ARIAS ################
  
}

