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
  
  shinyFileChoose(input, 'file_sbase', roots=volumes, session=session,
                  restrictions=system.file(package='base'),filetypes=c('xlsx'))
  
  #Type of format file
  # hot_formatFile_sbase <- reactive({ 
  #   
  #   dsource <- input$fbdesign_dsource_sbase
  #   if(dsource=="HIDAP") tff <- "HIDAP" 
  #   if(dsource=="FieldBookApp-SPBase") tff <- "FieldBookApp-SPBase" 
  #   tff
  # })
  
  #Return the file path (Excel file path)
  hot_path_sbase <- reactive ({
    req(input$file_sbase)
    if(is.null(input$file_sbase)){return(NULL)}
    
    validate(
      need(input$file_sbase != "", label = "Please enter an XLSX file. XLS files are forbidden")
    )
    
    if(length(input$file_sbase)==0){return (NULL)}
    if(length(input$file_sbase)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file_sbase)$datapath)
    }
  })
  
  #Read the fieldbook data
  hot_bdata_sbase <- reactive({
    
    #file_type <- hot_formatFile_sbase()
    
    #1. Fieldbook from fieldbookapp
    #if(file_type == "FieldBookApp-SPBase"){
      file_fbapp <- input$file_fbapp_sbase
      if (is.null(file_fbapp))  return(NULL)
      dt <- readr::read_csv(file_fbapp$datapath)
      #dt <- readr::read_csv(file ="D:\\HIDAP_DOCUMENTATION_AND_EXAMPLES\\HIDAP-SweetPotatoBase\\FieldBookApp\\formato para subir a la base de datos\\fbapp_trial1_sbase_bryanEllerbrock.csv")
      # Data wrangling ----------------------------------------------------------
      hot_bdata <- fbapp2hidap(dt) 
    #}
    
    hot_bdata
    
    
  })
  
  #Return Installation sheet parameters
  hot_params <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Installation")
      #hot_design <- get_fb_param(hot_param,"Experimental design")
      #hot_design <- get_fb_param(hot_param,"Experimental_design")
      hot_design <- get_fb_param(hot_param,"Experimental_design_abbreviation")
      
      
      #hot_design <- get_fb_param(hot_param,"Experimental_design") #early version of HiDAP
      
      #hot_plot_size <- get_fb_param(hot_param,"Plot size (m2)")
      hot_plot_size <- get_fb_param(hot_param,"Plot_size_(m2)")
      
      #hot_plant_den <- get_fb_param(hot_param,"Planting density (plants/Ha)")
      hot_plant_den <- get_fb_param(hot_param,"Planting_density_(plants/Ha)")
      
      hot_factor_lvl1 <- get_fb_param( hot_param, "Factor_name_1")
      
      hot_factor_lvl2 <- get_fb_param( hot_param, "Factor_name_2")
      
      
      hot_psize_mother <- get_pvs_param(pvs_data = hot_param, col_param = "Mother", row_param = "Plot_size_(m2)")
      hot_psize_baby <- get_pvs_param(pvs_data = hot_param, col_param = "Baby_1", row_param = "Plot_size_(m2)")
      
      hot_pden_mother <- get_pvs_param(pvs_data = hot_param, col_param = "Mother", row_param = "Planting_density_(plants/Ha)")
      hot_pden_baby <- get_pvs_param(pvs_data = hot_param, col_param = "Baby_1", row_param = "Planting_density_(plants/Ha)")
      
      
      
      hot_params_list <- list(hot_design = hot_design, hot_plot_size = hot_plot_size,
                              hot_plant_den =  hot_plant_den,  hot_factor_lvl1 = hot_factor_lvl1,
                              hot_factor_lvl2 =  hot_factor_lvl2, 
                              hot_psize_mother = hot_psize_mother, hot_pden_mother = hot_pden_mother,
                              hot_psize_baby   = hot_psize_baby,   hot_pden_baby   = hot_pden_baby
      )
    }
  })
  
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
  

  #hot_btable represents fieldbook data
  output$hot_btable_fbapp_sbase <-  renderRHandsontable({
    
    req(input$file_fbapp_sbase)
    
    values<-  shiny::reactiveValues(
      hot_btable_fbapp_sbase = hot_bdata_sbase()
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_btable_fbapp_sbase)) {
      DF = hot_to_r(input$hot_btable_fbapp_sbase)
      values[["hot_btable_fbapp_sbase"]] = DF
    } else if (!is.null(values[["hot_btable_fbapp_sbase"]])) {
      DF = values[["hot_btable_fbapp_sbase"]]
    }
    
    # if(input$calculate_sbase>0){
    # 
    #   DF <- values[["hot_btable_fbapp_sbase"]]
    #   DF <- rhandsontable_update(DF)
    #   #DF <- temp
    # }

    
    if(!is.null(DF)){
      
      #dsource <- hot_formatFile_sbase()
      #if(dsource=="FieldBookApp-SPBase") dsource <- 2
      dsource <- 2
      traits <- traittools::get_trait_fb(DF, dsource = dsource)
      #print(traits)
      path <- fbglobal::get_base_dir() ##begin fbglobal
      #print(path)
      path <- file.path(path,"hot_fieldbook_sbase.rds")
      #print(path)
      saveRDS(DF, path)
      #enf fbglobal
      #print("checking with crop ontology")
      crop <- hot_crop_sbase()
      #trait_dict <- get_crop_ontology(crop = crop,trial = trial)
      trait_dict <- get_crop_ontology(crop = crop, dsource = dsource)
      traittools::col_render_trait(fieldbook = DF, trait = traits , trait_dict = trait_dict, dsource = dsource)
    }
  })
  
  
  
  # output$fbcheck_genofilter_sbase <- renderUI({
  #   #req(input$file)
  #   ifelse("INSTN" %in% names(hot_bdata_sbase()) , sel <- "INSTN", sel <- 1)
  #   
  #   selectInput(inputId = "sel_fbcheck_genofilter_sbase",label = "Select Genotypes",choices = names(hot_bdata_sbase()),multiple = TRUE,selected = sel)
  #   
  # })
  
  
  # output$fbcheck_factorfilter_sbase <- renderUI({
  #   #req(input$file)
  #   selectInput(inputId = "sel_fbcheck_factorfilter_sbase",label = "Summary by",choices = names(hot_bdata_sbase()),multiple = TRUE,selected = 1)
  #   
  # })

  
  #Download  
  # output$exportButton_fbapp_sbase <- downloadHandler(
  #   filename = function() {
  #     paste('data-', Sys.Date(), '.csv', sep='')
  #   },
  #   content = function(con) {
  #     
  #     path <- fbglobal::get_base_dir()
  #     path <- paste(path,"hot_fieldbook_sbase.rds", sep="\\")
  #     DF <- readRDS(path)
  #     fb <- hidap2fbApp(fieldbook = DF)
  #     
  #     write.csv(x = fb, con)
  #   }
  # )


  

  #Visualize the list of traits using web tables
  # output$hot_td_trait = renderRHandsontable({
  #   td_trait <- orderBy(~ABBR, td_trait)
  #   rhandsontable(data = td_trait)
  # })
  
  #Export button: This event export and show the excel file which has been checked out.
  # shiny::observeEvent(input$exportButton,{
  #   
  #   #Begin Try
  #   try({
  #     
  #     #For many fieldbooks
  #     
  #   })
  # }) #end try
  
  #Export button: This event export and show the excel file for FieldBookApp-SPBase connection
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      path <- fbglobal::get_base_dir()
      #print(path)
      path <-  file.path(path,"hot_fieldbook_sbase.rds")
      #print(path)
      DF <- readRDS(path)
      fb<- hidap2fbApp(fieldbook = DF)
      write.csv(fb, con,row.names = FALSE)
    }
  )
  
  
}
