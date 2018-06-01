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
  
  #Catch the file path for reading fieldbook sheets
  volumes <- shinyFiles::getVolumes()
  
  # shinyFileChoose(input, 'file_sbase', roots=volumes, session=session,
  #                 restrictions=system.file(package='base'),filetypes=c('xlsx'))
  # 
  #Type of format file
  # hot_formatFile_sbase <- reactive({ 
  #   
  #   dsource <- input$fbdesign_dsource_sbase
  #   if(dsource=="HIDAP") tff <- "HIDAP" 
  #   if(dsource=="FieldBookApp-SPBase") tff <- "FieldBookApp-SPBase" 
  #   tff
  # })
  
  #Return the file path (Excel file path)
  # hot_path_sbase <- reactive ({
  #   #req(input$file_sbase)
  #   if(is.null(input$file_sbase)){return(NULL)}
  #   
  #   validate(
  #     need(input$file_sbase != "", label = "Please enter an XLSX file. XLS files are forbidden")
  #   )
  #   
  #   if(length(input$file_sbase)==0){return (NULL)}
  #   if(length(input$file_sbase)>0){
  #     hot_file <- as.character(parseFilePaths(volumes, input$file_sbase)$datapath)
  #   }
  # })
  # 
  #Read the fieldbook data
  # hot_bdata_sbase <- reactive({
  #   
  #   #file_type <- hot_formatFile_sbase()
  #   
  #   #1. Fieldbook from fieldbookapp
  #   #if(file_type == "FieldBookApp-SPBase"){
  #     file_fbapp <- input$file_fbapp_sbase
  #     if (is.null(file_fbapp))  return(NULL)
  #     dt <- readr::read_csv(file_fbapp$datapath)
  #     
  #     if(!is.element("plot_name", names(dt))){ 
  #       shinysky::showshinyalert(session, "alert_fbapp_warning_sbase", paste("ERROR: The file imported does not has 'plot_name' header."), styleclass = "danger")  
  #     } else if(nrow(dt)==1){
  #       shinysky::showshinyalert(session, "alert_fbapp_warning_sbase", paste("ERROR: Your data file has only one row of data. Please upload the right one. "), styleclass = "danger")  
  #     } else {
  #       hot_bdata_sbase <- fbapp2hidap(dt) 
  #     }
  #     
  #     #dt <- readr::read_csv(file ="D:\\HIDAP_DOCUMENTATION_AND_EXAMPLES\\HIDAP-SweetPotatoBase\\FieldBookApp\\formato para subir a la base de datos\\fbapp_trial1_sbase_bryanEllerbrock.csv")
  #     # Data wrangling ----------------------------------------------------------
  #     
  #   #}
  #   
  #     hot_bdata_sbase
  #   
  #   
  # })
  
  #Return Installation sheet parameters
  # hot_params <- reactive({
  #   hot_file <- hot_path()
  #   if(length(hot_file)==0){return (NULL)}
  #   if(length(hot_file)>0){
  #     
  #     hot_param <- readxl::read_excel(path=hot_file , sheet = "Installation")
  #     #hot_design <- get_fb_param(hot_param,"Experimental design")
  #     #hot_design <- get_fb_param(hot_param,"Experimental_design")
  #     hot_design <- get_fb_param(hot_param,"Experimental_design_abbreviation")
  #     
  #     
  #     #hot_design <- get_fb_param(hot_param,"Experimental_design") #early version of HiDAP
  #     
  #     #hot_plot_size <- get_fb_param(hot_param,"Plot size (m2)")
  #     hot_plot_size <- get_fb_param(hot_param,"Plot_size_(m2)")
  #     
  #     #hot_plant_den <- get_fb_param(hot_param,"Planting density (plants/Ha)")
  #     hot_plant_den <- get_fb_param(hot_param,"Planting_density_(plants/Ha)")
  #     
  #     hot_factor_lvl1 <- get_fb_param( hot_param, "Factor_name_1")
  #     
  #     hot_factor_lvl2 <- get_fb_param( hot_param, "Factor_name_2")
  #     
  #     
  #     hot_psize_mother <- get_pvs_param(pvs_data = hot_param, col_param = "Mother", row_param = "Plot_size_(m2)")
  #     hot_psize_baby <- get_pvs_param(pvs_data = hot_param, col_param = "Baby_1", row_param = "Plot_size_(m2)")
  #     
  #     hot_pden_mother <- get_pvs_param(pvs_data = hot_param, col_param = "Mother", row_param = "Planting_density_(plants/Ha)")
  #     hot_pden_baby <- get_pvs_param(pvs_data = hot_param, col_param = "Baby_1", row_param = "Planting_density_(plants/Ha)")
  #     
  #     
  #     
  #     hot_params_list <- list(hot_design = hot_design, hot_plot_size = hot_plot_size,
  #                             hot_plant_den =  hot_plant_den,  hot_factor_lvl1 = hot_factor_lvl1,
  #                             hot_factor_lvl2 =  hot_factor_lvl2, 
  #                             hot_psize_mother = hot_psize_mother, hot_pden_mother = hot_pden_mother,
  #                             hot_psize_baby   = hot_psize_baby,   hot_pden_baby   = hot_pden_baby
  #     )
  #   }
  # })
  #### temp

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
      return(NULL)
    } else {  
      dt <- readr::read_csv(file_fbapp$datapath)
    }
   
   
    ####### Show Warnings to users   #######
    if(!is.element("plot_name", names(dt))){ 
      shinysky::showshinyalert(session, "alert_fbapp_warning_sbase", paste("ERROR: The file imported does not has 'plot_name' header."), styleclass = "danger")  
    } else if(nrow(dt)==1){
      shinysky::showshinyalert(session, "alert_fbapp_warning_sbase", paste("ERROR: Your data file has only one row of data. Please upload the right one. "), styleclass = "danger")  
    } else {
      hot_bdata_sbase2 <- fbapp2hidap(fieldbook = dt)
    }
  
    #print(head(hot_bdata_sbase2))
    
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
      #print(path)
      shiny::withProgress(message = 'Downloading file', value = 0, {
      
      # print("datos directos")
      #   
      # print(hot_to_r(input$hot_btable_fbapp_sbase))  
      # 
      # print("datos values")
          
        incProgress(1/6, detail = paste("Reading HIDAP data..."))
      path <-  file.path(path,"hot_fieldbook_sbase.rds")
      
      
      #print(path)
      #DF <- readRDS(path) # Important note: run local 
      
      DF <- hot_to_r(input$hot_btable_fbapp_sbase) # Important note: run server
      
      incProgress(2/6, detail = paste("Formatting hidap file..."))
      
      fb<- hidap2fbApp(fieldbook = DF)
      
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
  
  
}


