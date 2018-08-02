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
  db <- reactiveValues()
  db$constUserDB <- "hidap_sbase"
  db$constPassDB <- "cKqrrEhTHLh3V2Fm70719"
  db$constDBName <- "hidap_sbase"
  db$constDBHost <- "176.34.248.121"
  
  #Catch the file path for reading fieldbook sheets
  volumes <- shinyFiles::getVolumes()
  
 
  #Return the type of crop in Minimal sheet
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
  
  hot_fbapp_path <- reactive({
   
    file_fbapp <- input$file_fbapp_sbase
    out<- file_fbapp$datapath 
    
  })

  fileNameExtFile <- reactive({
    
    servName <- "fbappdatapath.rds"
    uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
    ranStr <-  stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
    servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
    dirNameExtFile <- fbglobal::get_base_dir() #get directory of the file with fileName
    fileNameExtFile <-  paste0(dirNameExtFile, servName)
    
  })
  
  #hot_btable represents fieldbook data
  output$hot_btable_fbapp_sbase <-  renderRHandsontable({
    
    ####### Import CSV data #######
    file_fbapp <- input$file_fbapp_sbase
    #print(file_fbapp)
    if (is.null(file_fbapp)) {
      shinyjs::disable("saveData")
      return(NULL)
    } else {  
      dt <- readr::read_csv(file_fbapp$datapath)
      shinyjs::enable("saveData")
    }
 
    ####### Show Warnings to users   #######
    #ToDo: Include plot_id
    if(!is.element("plot_name", names(dt))){ 
      shinysky::showshinyalert(session, "alert_fbapp_warning_sbase", paste("ERROR: The file imported does not has 'plot_name' header."), styleclass = "danger")  
    } else if(nrow(dt)==1){
      shinysky::showshinyalert(session, "alert_fbapp_warning_sbase", paste("ERROR: Your data file has only one row of data. Please upload the right one. "), styleclass = "danger")  
    } else {
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

#     ####### Detect if hot_btable_fbapp_sbase has data  #######
#     if (!is.null(input$hot_btable_fbapp_sbase)) {
#       print("if 1")
#       DF = hot_to_r(input$hot_btable_fbapp_sbase)
#       #values[["hot_btable_fbapp_sbase"]] = DF
#       
#     ## Important Note: in case users upload different files, they will see:
#     dirNameExtFile <- fbglobal::get_base_dir()
#     #fileNameExtFile <-  paste(dirNameExtFile, "fbappdatapath.rds")
#     fileNameExtFile <-  paste0(dirNameExtFile, servName)
#       
#     #if(file.exists(file.path(dirNameExtFile, "fbappdatapath.rds") )){
#     if(file.exists(fileNameExtFile)) {    
#         former_datapath <- readRDS(fle = fileNameExtFile)
#         if(hot_fbapp_path()!= former_datapath){
#           DF <- hot_bdata_sbase2
#         } 
#     }

    if(!is.null(input$hot_btable_fbapp_sbase)) {
       DF = hot_to_r(input$hot_btable_fbapp_sbase)
       
      
          if(file.exists(fileNameExtFile)) {    
            former_datapath <- readRDS(file = fileNameExtFile)
            if(hot_fbapp_path()!= former_datapath){
            #if(!identical(hot_bdata_sbase2, DF)){
            #if(flag1) {
             DF <- hot_bdata_sbase2
            } 
          }

      ###  end important note
      values[["hot_btable_fbapp_sbase"]] = DF
    } else if (!is.null(values[["hot_btable_fbapp_sbase"]])) {
      DF = values[["hot_btable_fbapp_sbase"]]
    } 

    if(!is.null(DF)){
 
      dsource <- 2
      traits <- traittools::get_trait_fb(DF, dsource = dsource)

      file_fbapp <- input$file_fbapp_sbase
      value_datapath <- file_fbapp$datapath 
      fileNameExtFile <- paste0(dirNameExtFile, servName) #file.path(fbglobal::get_base_dir(), "fbappdatapath.rds")

      saveRDS(value_datapath, file =  fileNameExtFile())
      
      crop <- hot_crop_sbase()
      trait_dict <- get_crop_ontology(crop = crop, dsource = dsource)
      traittools::col_render_trait(fieldbook = DF, trait = traits , trait_dict = trait_dict, dsource = dsource)
    }
  })
  
  #Export button: This event export and show the excel file for FieldBookApp-SPBase connection
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      path <- fbglobal::get_base_dir()
      
      shiny::withProgress(message = 'Downloading file', value = 0, {
      incProgress(1/6, detail = paste("Reading table data..."))
        
      path <-  file.path(path,"hot_fieldbook_sbase.rds")
      
      DF <- hot_to_r(input$hot_btable_fbapp_sbase) # Important note: run server
      
      incProgress(2/6, detail = paste("Formatting hidap file..."))
      fb<- DF #hidap2fbApp(fieldbook = DF)
      
      exportFormat <- input$fbcheck_fbapp_ExportFormat_sbase
      if(exportFormat=="Simple"){
        names(fb)[1] <-  "observationunit_name"
        #Remove unncesary columns for simple format
        #ToDo: ask if user need 'plot_id' column in 'simple' format for sweetpotatobase
        fb$accession_name <- fb$plot_id <- 	fb$plot_number <- fb$block_number <- 	fb$is_a_control	<- fb$rep_number	<- fb$row_number <- 	fb$col_number <- NULL
        fb <- fb
        
      } else {
        fb
      }
      
      
      
      incProgress(3/6, detail = paste("Downloading FieldBookApp-SPBase file..."))
      incProgress(4/6, detail = paste("Refreshing HIDAP..."))
      Sys.sleep(3)
      incProgress(5/6, detail = paste("Refreshing HIDAP..."))
    
      write.csv(fb, con,row.names = FALSE)
      
      incProgress(6/6, detail = paste("Refreshing HIDAP..."))
      Sys.sleep(5)
      #shinyjs::js$downloadData()
      })
    }
  )
  
  observeEvent(input$saveData,{
    if(!session$userData$logged){
      showModal(modalShowMessage("You must log in to save your data"))
      return()
    }
    showModal(modalEnterFileName())
  })
  
  observeEvent(input$btSaveModal, {
    saveFile()
  })

  modalEnterFileName <- function(){
    modalDialog(
      title = HTML("<center><font color='#f7941d'><h2> Saving Study </h2></font></center>"),
      div(
        textInput("fileName", "File name:"),
        textInput("breederName", "Breeder Name:")
      ),
      
      easyClose = T,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btSaveModal", "Save", width = 120, icon = icon("save")),
        actionButton("btModalSaveNUpdload", "Save & Upload", width = 120, icon=icon("upload"))
        )
    )
  } 
  
  modalShowMessage <- function(str){
    modalDialog(
      title = HTML("<center><font color='#f7941d'><h2> HiDAP says: </h2></font></center>"),
      div(
        HTML(paste0("<h5>",str, "<h5>"))
      ),
      easyClose = T,
      footer = tagList(
        modalButton("Ok")
      )
    )
  } 
    
  ### to chek if name is correct and available
  ### for testing, we will asume correct and no duplicate names are entered
  checkNewFileName <- function (fname){
    
    ### TO DO ###
    
    return(T)
  }
  
  saveFile <- function(){
    
    fileName <- trimws(input$fileName)
    breederName <- trimws(input$breederName)
    removeModal()
    
    message <- ""
    
    xdate <- Sys.time()
    
    v_study <- input$file_fbapp_sbase$name
    v_study <- gsub(".csv", "", v_study)
    
    uploadDate  <- as.character(xdate, "%Y%m%d%H%M%S")
    uploadDate_s <- as.character(xdate, "%Y-%m-%d %H:%M:%S")
    ranStr <-  stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
    servName <- paste(uploadDate, ranStr , sep= "-") #nombre sin extensions!!!!
    servName <- paste0(servName, ".csv")
    
  
    
    shiny::withProgress(message = 'Saving file', value = 0, {
      incProgress(1/4, detail = paste("Reading table data..."))
      
      
      DF <- hot_to_r(input$hot_btable_fbapp_sbase) # Important note: run server
      
      
      incProgress(2/4, detail = paste("Generating file..."))
      
      pathGlobal <- fbglobal::get_base_dir()
      servPath <-  file.path(pathGlobal,servName)
      
      write.csv(DF, file=servPath)
      
      incProgress(3/4, detail = paste("Saving into database"))
      Sys.sleep(3)
      
      params <- list(
        dataRequest = "uploadFile",
        fileServerName = servName,
        filedata=upload_file(servPath, "text/csv")
      )
      
      var <- POST("https://research.cip.cgiar.org/gtdms/hidap/script/hidap_sbase/getFileUpload.php", body=params)
      code <- content(var, "text")
      
     
    
      if (file.exists(servPath)) file.remove(servPath)
      
      if (code == "200"){
        
        message <- paste0(message, fileName, " was successfully saved <br>")
        saveFileToDb("Sweetpotato", servName, paste0(fileName, ".csv"), breederName, v_study ,uploadDate_s )
        
        # saveFileToDB <- function(crop, server_book_name, book_name, breeder_name, study, upload_date){
          
      }
      else{
        message = paste0( message, "Error while sharing ", fileName , ". Please Try again. <br>")
      }
      incProgress(4/4, detail = paste("Finishing"))
      Sys.sleep(5)
      
    })
    
    shinyalert("Success", message, type = "success")
      
  }
  
  saveFileToDb <- function(crop, server_book_name, book_name, breeder_name, study, upload_date){
    
    mydb = dbConnect(MySQL(), user=db$constUserDB, password=db$constPassDB, dbname=db$constDBName, host=db$constDBHost)
    strQry <- paste0("insert into files (owner_id, crop, breeder_name, book_name, server_book_name, study, status, date_created) values(", session$userData$userId, ",")
    strQry <- paste0(strQry, "'",crop ,"','",breeder_name,"','",book_name,"','", server_book_name, "','", study, "','", "In review", "','",upload_date,"')")
    qryUser = dbSendQuery(mydb, strQry)
    dbDisconnect(mydb)
    
  }
  
  
}


