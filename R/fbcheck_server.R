#' Server component for traittools
#' Returns server side components
#' @author Omar Benites
#' @param input shinyserver input
#' @param output nameo of the output element
#' @param session shinyserver session
#' @param values The reactive values
#' @export 

fbcheck_server <- function(input, output, session, values) {
  
  volumes <- shinyFiles::getVolumes()
  shinyFileChoose(input, 'file', roots=volumes, session=session,
                  restrictions=system.file(package='base'),filetypes=c('xlsx'))

  hot_path <- reactive ({
    req(input$file)
    if(is.null(input$file)){return(NULL)}
    
    validate(
      need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden")
    )
    
    if(length(input$file)==0){return (NULL)}
    if(length(input$file)>0){
      hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
    }
  })
  
  hot_bdata <- reactive({
    hot_file <- hot_path()
    print(hot_file)
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      
      #hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
      
      print(hot_trial())
      
      if(hot_trial()=="Participatory Varietal Selection"){
        
        pvs_sheet_list <- c("F1_selection_criteria", "F2_select_clones_flowering", "F3_select_clones_harvest", "F4_harvest_mother" , 
                            "F5_harvest_baby", "F6_organoleptic_mother", "F7_organoleptic_baby", "F8_postharvest_dormancy",
                            "F9_postharvest_clones_storage")
        
        fb_sheets <- readxl::excel_sheets(hot_file)
        fb_sheets <- fb_sheets[fb_sheets %in% pvs_sheet_list]
        
        hot_bdata <- lapply(X = fb_sheets, function(x) openxlsx::read.xlsx(xlsxFile = hot_file , sheet = x, na.strings = TRUE ))
        names(hot_bdata) <- fb_sheets
        hot_bdata
        
      } else {
        
        hot_bdata <- readxl::read_excel(path=hot_file , sheet = "Fieldbook")
        
      }  
      #saveRDS(object = hot_bdata, "temp.rda")
      hot_bdata 
      
      
    }
  })
  
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
      
      hot_params_list <- list(hot_design = hot_design, hot_plot_size = hot_plot_size,
                              hot_plant_den =  hot_plant_den)
    }
  })  
  
  hot_crop <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Minimal")
      hot_crop <- get_fb_param(hot_param,"Crop")
    }
  })
  
  hot_trial <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      hot_param <- readxl::read_excel(path=hot_file , sheet = "Minimal")
      #hot_crop <- get_fb_param(hot_param,"Type of Trial") #in DataCollector
      hot_trial <- get_fb_param(hot_param,"Type_of_Trial") #in HiDAP
      
    }
  })
  
  ###extra code
  hot_mgt <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){

      hot_mgt <- openxlsx::read.xlsx(xlsxFile= hot_file, sheet = "Crop_management", detectDates = TRUE)
      hot_mgt
      #print(hot_mgt)
    }
  })
  
  hot_mtl <- reactive({
    hot_file <- hot_path()
    if(length(hot_file)==0){return (NULL)}
    if(length(hot_file)>0){
      #hot_mtl <- reactive_excel_metadata(file_id =hot_file , "Material List")
      #hot_mtl <- openxlsx::read.xlsx(xlsxFile= hot_file, sheet = "Material List", detectDates = TRUE)
      hot_mtl <- openxlsx::read.xlsx(xlsxFile= hot_file, sheet = "Material_List", detectDates = TRUE)
      hot_mtl
      #print(hot_mtl)
    }
  })

  output$hot_btable  <-  renderRHandsontable({
    
    req(input$file)
    
    if(hot_trial()!="Participatory Varietal Selection"){
    
    values  <-  shiny::reactiveValues(
      hot_btable = hot_bdata()
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_btable)) {
      DF = hot_to_r(input$hot_btable)
      values[["hot_btable"]] = DF
    } else if (!is.null(values[["hot_btable"]])) {
      DF = values[["hot_btable"]]
    }
    
    if(input$calculate>0){
      
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)

      DF = values[["hot_btable"]]
      DF <- as.data.frame(DF)
      DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      #print(DF)
    }
    
    if(!is.null(DF)){
      
      traits <- get_trait_fb(DF)
      
      ##begin fbglobal
      path <- fbglobal::get_base_dir()
      path <- paste(path,"hot_fieldbook.rds", sep="\\")
      saveRDS(DF, path)
      
      #enf fbglobal
      
      #saveRDS(DF,"hot_fieldbook.rds")
      
      
      
      
      crop <- hot_crop()
      trial <- hot_trial()
      print(DF)
      #trait_dict <- get_crop_ontology(crop = crop,trial = trial)
      trait_dict <- get_crop_ontology(crop = crop)
      traittools::col_render_trait(fieldbook = DF, trait = traits , trait_dict = trait_dict)
      
    }
    #}
    }#fin del if de pvs
  })
  
  output$fbcheckSelect_criteria <- shinyTree::renderTree({
    
    out <- selcriteria
    out
  })
  
  output$hot_f1_btable  <-  renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_f1_btable = hot_bdata()$F1_selection_criteria
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_f1_btable)) {
      DF = hot_to_r(input$hot_f1_btable)
      values[["hot_f1_btable"]] = DF
    } else if (!is.null(values[["hot_f1_btable"]])) {
      DF = values[["hot_f1_btable"]]
    }
    
    if(input$calculate>0){
      
      #hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      #hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      DF = values[["hot_f1_btable"]]
      DF <- as.data.frame(DF)
      
      #calculate_form_sclones
      #DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
      #                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      DF <- calculate_form_scriteria(data = DF)
    }
    
    if(!is.null(DF)){
      # DF
                 traits <- get_trait_fb(DF)
       saveRDS(DF,"hot_f1_fieldbook.rds")
                 crop <- hot_crop()
      #           trial <- hot_trial()
      #           
      #           trait_dict <- get_crop_ontology(crop = crop,trial = trial)
                 #trait_dict <- get_crop_ontology(crop = crop)
                  #traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      rhandsontable::rhandsontable(data = DF,width = 2000)
    }
    
  })
  
  output$hot_f2_btable  <-  renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_f2_btable = hot_bdata()$F2_select_clones_flowering
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_f2_btable)) {
      DF = hot_to_r(input$hot_f2_btable)
      values[["hot_f2_btable"]] = DF
    } else if (!is.null(values[["hot_f2_btable"]])) {
      DF = values[["hot_f2_btable"]]
    }
    
    if(input$calculate>0){
      
      #hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      #hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      DF = values[["hot_f2_btable"]]
      DF <- as.data.frame(DF)
      
      #calculate_form_sclones
      #DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
      #                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      DF <- calculate_form_sclones(data = DF)
    }
    
        if(!is.null(DF)){
          #DF
           traits <- get_trait_fb(DF)
           saveRDS(DF,"hot_f2_fieldbook.rds")
           crop <- hot_crop()
#           trial <- hot_trial()
#           
           #trait_dict <- get_crop_ontology(crop = crop,trial = trial)
           trait_dict <- get_crop_ontology(crop = crop)
           traittools::col_render_trait(fieldbook = DF, trait = traits, trait_dict = trait_dict)
           #rhandsontable::rhandsontable(data = DF) %>%  rhandsontable::hot_cols("MSM",allowInvalid = TRUE)
        }
    
  })
 
  output$hot_f3_btable  <-  renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_f3_btable = hot_bdata()$F3_select_clones_harvest
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_f3_btable)) {
      DF = hot_to_r(input$hot_f3_btable)
      values[["hot_f3_btable"]] = DF
    } else if (!is.null(values[["hot_f3_btable"]])) {
      DF = values[["hot_f3_btable"]]
    }
    
    if(input$calculate>0){
      
      #hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      #hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      DF = values[["hot_f3_btable"]]
      DF <- as.data.frame(DF)
      
      #calculate_form_sclones
      #DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
      #                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      DF <- calculate_form_sclones(data = DF)
    }
    
    if(!is.null(DF)){
      #DF
                 traits <- get_trait_fb(DF)
      saveRDS(DF,"hot_f3_fieldbook.rds")
                  crop <- hot_crop()
      #           trial <- hot_trial()
      #           
                 #trait_dict <- get_crop_ontology(crop = crop,trial = trial)
                 trait_dict <- get_crop_ontology(crop = crop) 
                 traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      #rhandsontable::rhandsontable(data = DF) %>%  rhandsontable::hot_cols("MSM",allowInvalid = TRUE)
    }
    
  })
  
  output$hot_f4_btable  <-  renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_f4_btable = hot_bdata()$F4_harvest_mother
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_f4_btable)) {
      DF = hot_to_r(input$hot_f4_btable)
      values[["hot_f4_btable"]] = DF
    } else if (!is.null(values[["hot_f4_btable"]])) {
      DF = values[["hot_f4_btable"]]
    }
    
    if(input$calculate>0){
      
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      DF = values[["hot_f4_btable"]]
      DF <- as.data.frame(DF)
      
      #calculate_form_sclones
      #DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
      #                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      DF <- calculate_form_harvest(data = DF, plot_size = hot_plot_size, plant_den = hot_plant_den)
    }
    
    if(!is.null(DF)){
      #DF
                 traits <- get_trait_fb(DF)
      saveRDS(DF,"hot_f4_fieldbook.rds")
                  crop <- hot_crop()
      #           trial <- hot_trial()
      #           
                 #trait_dict <- get_crop_ontology(crop = crop,trial = trial)
                 trait_dict <- get_crop_ontology(crop = crop)
                 traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      #rhandsontable::rhandsontable(data = DF)
    }
    
  })
  
  output$hot_f5_btable  <-  renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_f5_btable = hot_bdata()$F5_harvest_baby
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_f5_btable)) {
      DF = hot_to_r(input$hot_f5_btable)
      values[["hot_f5_btable"]] = DF
    } else if (!is.null(values[["hot_f5_btable"]])) {
      DF = values[["hot_f5_btable"]]
    }
    
    if(input$calculate>0){
      
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      DF = values[["hot_f5_btable"]]
      DF <- as.data.frame(DF)
      
      #calculate_form_sclones
      #DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
      #                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      DF <- calculate_form_harvest(data = DF, plot_size = hot_plot_size, plant_den = hot_plant_den)
    }
    
    if(!is.null(DF)){
      #DF
                 traits <- get_trait_fb(DF)
       saveRDS(DF,"hot_f5_fieldbook.rds")
                  crop <- hot_crop()
      #           trial <- hot_trial()
      #           
      #           trait_dict <- get_crop_ontology(crop = crop,trial = trial)
                  trait_dict <- get_crop_ontology(crop = crop)
                  traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      # rhandsontable::rhandsontable(data = DF) 
    }
    
  })
   
  output$hot_f6_btable  <-  renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_f6_btable = hot_bdata()$F6_organoleptic_mother
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_f6_btable)) {
      DF = hot_to_r(input$hot_f6_btable)
      values[["hot_f6_btable"]] = DF
    } else if (!is.null(values[["hot_f6_btable"]])) {
      DF = values[["hot_f6_btable"]]
    }
    
    if(input$calculate>0){
      
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      DF = values[["hot_f6_btable"]]
      DF <- as.data.frame(DF)
      
      #calculate_form_sclones
      #DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
      #                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      #DF <- calculate_form_harvest(data = DF, plot_size = hot_plot_size, plant_den = hot_plant_den)
    }
    
    if(!is.null(DF)){
      #DF
      #traits <- get_trait_fb(DF)
       saveRDS(DF,"hot_f6_fieldbook.rds")
      #crop <- hot_crop()
      #           trial <- hot_trial()
      #           
      #           trait_dict <- get_crop_ontology(crop = crop,trial = trial)
      #trait_dict <- get_crop_ontology(crop = crop)
      #traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      rhandsontable::rhandsontable(data = DF) 
    }
    
  })
  
  output$hot_f7_btable  <-  renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_f7_btable = hot_bdata()$F7_organoleptic_baby
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_f7_btable)) {
      DF = hot_to_r(input$hot_f7_btable)
      values[["hot_f7_btable"]] = DF
    } else if (!is.null(values[["hot_f7_btable"]])) {
      DF = values[["hot_f7_btable"]]
    }
    
    if(input$calculate>0){
      
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      DF = values[["hot_f7_btable"]]
      DF <- as.data.frame(DF)
      
      #calculate_form_sclones
      #DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
      #                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      #DF <- calculate_form_harvest(data = DF, plot_size = hot_plot_size, plant_den = hot_plant_den)
    }
    
    if(!is.null(DF)){
      #DF
      #traits <- get_trait_fb(DF)
       saveRDS(DF,"hot_f7_fieldbook.rds")
      #crop <- hot_crop()
      #           trial <- hot_trial()
      #           
      #           trait_dict <- get_crop_ontology(crop = crop,trial = trial)
      #trait_dict <- get_crop_ontology(crop = crop)
      #traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      rhandsontable::rhandsontable(data = DF) 
    }
    
  })
  
  output$hot_f8_btable  <-  renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_f8_btable = hot_bdata()$F8_postharvest_dormancy
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_f8_btable)) {
      DF = hot_to_r(input$hot_f8_btable)
      values[["hot_f8_btable"]] = DF
    } else if (!is.null(values[["hot_f8_btable"]])) {
      DF = values[["hot_f8_btable"]]
    }
    
    if(input$calculate>0){
      
      #hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      #hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      DF = values[["hot_f8_btable"]]
      DF <- as.data.frame(DF)
      
      #calculate_form_sclones
      #DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
      #                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      DF <- calculate_form_dormancy(data = DF)
    }
    
    if(!is.null(DF)){
      #DF
      #           traits <- get_trait_fb(DF)
      saveRDS(DF,"hot_f8_fieldbook.rds")
      #           crop <- hot_crop()
      #           trial <- hot_trial()
      #           
      #           trait_dict <- get_crop_ontology(crop = crop,trial = trial)
      #           traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      rhandsontable::rhandsontable(data = DF) 
    }
    
  })
  
  output$hot_f9_btable  <-  renderRHandsontable({
    
    values  <-  shiny::reactiveValues(
      hot_f9_btable = hot_bdata()$F9_postharvest_clones_storage
    )
    
    DF <- NULL
    
    if (!is.null(input$hot_f9_btable)) {
      DF = hot_to_r(input$hot_f9_btable)
      values[["hot_f9_btable"]] = DF
    } else if (!is.null(values[["hot_f9_btable"]])) {
      DF = values[["hot_f9_btable"]]
    }
    
    if(input$calculate>0){
      
      #hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      
      #hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      
      DF = values[["hot_f9_btable"]]
      DF <- as.data.frame(DF)
      
      #calculate_form_sclones
      #DF <- calculate_trait_variables(fb = DF,plot_size = hot_plot_size,
      #                                      plant_den = hot_plant_den,mgt = hot_mgt(),mtl=hot_mtl(),trial_type=hot_trial())
      DF <- calculate_form_postharvest(data = DF)
    }
    
    if(!is.null(DF)){
      #DF
      #           traits <- get_trait_fb(DF)
      saveRDS(DF,"hot_f9_fieldbook.rds")
      #           crop <- hot_crop()
      #           trial <- hot_trial()
      #           
      #           trait_dict <- get_crop_ontology(crop = crop,trial = trial)
      #           traittools::col_render_trait(fieldbook = DF,trait = traits ,trait_dict = trait_dict)
      rhandsontable::rhandsontable(data = DF) 
    }
    
  })
   
  output$hot_td_trait = renderRHandsontable({ 
    td_trait <- orderBy(~ABBR, td_trait)
    rhandsontable(data = td_trait) 
  })
  
  shiny::observeEvent(input$exportButton,{
    
    #For single Fieldbooks
    withProgress(message = "Downloading Fieldbook and Applying Format...",value= 0,
                 {
                  
                   crop <- hot_crop()
                   trial <- hot_trial()
                   
                   if(crop == "potato"){
                      trait_dict <- td_potato
                   }
                   
                   if(crop == "sweetpotato"){ 
                      trait_dict <- td_sweetpotato
                   }
                   
                   trait_dict <- td_crop
                    #print("DF")

                   
                   if(hot_trial()=="Participatory Varietal Selection"){
                     
                     trait_dict <-  td_potato
                     
                     hot_file <- hot_path() 
                    # wb <- openxlsx::loadWorkbook(hot_file)
                     #sheets <- readxl::excel_sheets(path = hot_file)
                     
                     #special_data_files <- list.files()
                     
                     pvs_sheet_list <- c("F1_selection_criteria", "F2_select_clones_flowering", "F3_select_clones_harvest", 
                                         "F4_harvest_mother" , 
                                         "F5_harvest_baby", "F6_organoleptic_mother", 
                                         "F7_organoleptic_baby", "F8_postharvest_dormancy",
                                         "F9_postharvest_clones_storage")
                     
                     pvs_sheet <- readxl::excel_sheets(hot_file)
                     #fb_sheets <- fb_sheets[fb_sheets %in% pvs_sheet_list]
                    
                     if(is.element(pvs_sheet_list[1], pvs_sheet)){
                       wb <- openxlsx::loadWorkbook(hot_file)
                       DF_f1 <- readRDS("hot_f1_fieldbook.rds")
                       openxlsx::removeWorksheet(wb, pvs_sheet_list[1])
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "F1_selection_criteria",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "F1_selection_criteria", x = DF_f1,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                     }
                     
                     if(is.element(pvs_sheet_list[2], pvs_sheet)){
                       wb <- openxlsx::loadWorkbook(hot_file)
                       DF_f2 <- readRDS("hot_f2_fieldbook.rds")
                       trait_f2 <- get_trait_fb(DF_f2)
                       openxlsx::removeWorksheet(wb, pvs_sheet_list[2])
                       
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "F2_select_clones_flowering",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "F2_select_clones_flowering", x = DF_f2, colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                     }
                     
                     if(is.element(pvs_sheet_list[3], pvs_sheet)){
                       wb <- openxlsx::loadWorkbook(hot_file)
                       DF_f3 <- readRDS("hot_f3_fieldbook.rds")
                       trait_f3 <- get_trait_fb(DF_f3)
                       openxlsx::removeWorksheet(wb, pvs_sheet_list[3])
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "F3_select_clones_harvest",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "F3_select_clones_harvest", x = DF_f3, colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                     }
                     
                     
                     if(is.element(pvs_sheet_list[4], pvs_sheet)){
                       wb <- openxlsx::loadWorkbook(hot_file)
                       DF_f4 <- readRDS("hot_f4_fieldbook.rds")
                       trait_f4 <- get_trait_fb(DF_f4)
                       openxlsx::removeWorksheet(wb, pvs_sheet_list[4])
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "F4_harvest_mother",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "F4_harvest_mother", x = DF_f4, colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                     }
                     
                     
                     if(is.element(pvs_sheet_list[5], pvs_sheet)){
                       wb <- openxlsx::loadWorkbook(hot_file)
                       DF_f5 <- readRDS("hot_f5_fieldbook.rds")
                       trait_f5 <- get_trait_fb(DF_f5)
                       openxlsx::removeWorksheet(wb, pvs_sheet_list[5])
                       
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "F5_harvest_baby",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "F5_harvest_baby", x = DF_f5,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                     }
                     
                     
                     if(is.element(pvs_sheet_list[6], pvs_sheet)){
                       wb <- openxlsx::loadWorkbook(hot_file)
                       DF_f6 <- readRDS("hot_f6_fieldbook.rds")
                       #trait_f6 <- get_trait_fb(DF_f6)
                       openxlsx::removeWorksheet(wb, pvs_sheet_list[6])
                       
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "F6_organoleptic_mother",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "F6_organoleptic_mother", x = DF_f6,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                     }
                     
                     
                     if(is.element(pvs_sheet_list[7], pvs_sheet)){
                       wb <- openxlsx::loadWorkbook(hot_file)
                       DF_f7 <- readRDS("hot_f7_fieldbook.rds")
                       #trait_f2 <- get_trait_fb(DF_f2)
                       openxlsx::removeWorksheet(wb, pvs_sheet_list[7])
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "F7_organoleptic_baby",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "F7_organoleptic_baby", x = DF_f7,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                     }
                     
                     
                     if(is.element(pvs_sheet_list[8], pvs_sheet)){
                       wb <- openxlsx::loadWorkbook(hot_file)
                       DF_f8 <- readRDS("hot_f8_fieldbook.rds")
                       openxlsx::removeWorksheet(wb, pvs_sheet_list[8])
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "F8_postharvest_dormancy",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "F8_postharvest_dormancy", x = DF_f8,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                     }
                     
                     
                     if(is.element(pvs_sheet_list[9], pvs_sheet)){
                       wb <- openxlsx::loadWorkbook(hot_file)
                       DF_f9 <- readRDS("hot_f9_fieldbook.rds")
                       openxlsx::removeWorksheet(wb, pvs_sheet_list[9])
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "F9_postharvest_clones_storage",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "F9_postharvest_clones_storage", x = DF_f9,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                     }
                     
                     
                     if(!is.element("summary_organoleptic_mother", pvs_sheet)){
                       
                       wb <- openxlsx::loadWorkbook(hot_file)
                       #openxlsx::removeWorksheet(wb, pvs_sheet_list[9])
                       list_form <- split_tidy_form(form = DF_f6)
                       out <- form_parameters(list_form = list_form)
                       
                       out_table_f6 <- out_form_table(list_form = list_form, 
                                                   list_genotypes = out$genotypes, 
                                                   list_panel_name = out$Number_of_panel , 
                                                   list_panel_number = out$Name_of_Evaluator,
                                                   list_panel_sex =out$Sex)
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "summary_organoleptic_mother",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "summary_organoleptic_mother", x = out_table_f6,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                       
                       
                     }
                     
                     if(is.element("summary_organoleptic_mother", pvs_sheet)){
                       
                       wb <- openxlsx::loadWorkbook(hot_file)
                       openxlsx::removeWorksheet(wb, sheet ="summary_organoleptic_mother")
                       list_form <- split_tidy_form(form = DF_f6)
                       out <- form_parameters(list_form = list_form)
                       
                       out_table_f6 <- out_form_table(list_form = list_form, 
                                                   list_genotypes = out$genotypes, 
                                                   list_panel_name = out$Number_of_panel , 
                                                   list_panel_number = out$Name_of_Evaluator,
                                                   list_panel_sex =out$Sex)
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "summary_organoleptic_mother",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "summary_organoleptic_mother", x =  out_table_f6,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                       
                       
                       
                     }
                     
                     if(!is.element("summary_organoleptic_baby", pvs_sheet)){
                       
                       wb <- openxlsx::loadWorkbook(hot_file)
                       #openxlsx::removeWorksheet(wb, pvs_sheet_list[9])
                       list_form <- split_tidy_form(form = DF_f7)
                       out <- form_parameters(list_form = list_form)
                       
                       out_table_f7 <- out_form_table(list_form = list_form, 
                                                   list_genotypes = out$genotypes, 
                                                   list_panel_name = out$Number_of_panel , 
                                                   list_panel_number = out$Name_of_Evaluator,
                                                   list_panel_sex =out$Sex)
                       
                       openxlsx::addWorksheet(wb = wb,sheetName = "summary_organoleptic_baby",gridLines = TRUE)
                       openxlsx::writeDataTable(wb,sheet = "summary_organoleptic_baby", x =  out_table_f7,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                       
                       
                     }
                     
                     if(is.element("summary_organoleptic_baby", pvs_sheet)){
                       
                       wb <- openxlsx::loadWorkbook(hot_file)
                       openxlsx::removeWorksheet(wb, sheet ="summary_organoleptic_baby")
                       list_form <- split_tidy_form(form = DF_f7)
                       out <- form_parameters(list_form = list_form)
                       
                       out_table_f7 <- out_form_table(list_form = list_form, 
                                                   list_genotypes = out$genotypes, 
                                                   list_panel_name = out$Number_of_panel , 
                                                   list_panel_number = out$Name_of_Evaluator,
                                                   list_panel_sex =out$Sex)
                       
                       openxlsx::addWorksheet(wb = wb, sheetName = "summary_organoleptic_baby", gridLines = TRUE)
                       openxlsx::writeDataTable(wb, sheet = "summary_organoleptic_baby", x = out_table_f7,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                  
                     }
                     
                     if(!is.element("summary_global", pvs_sheet)){
                     
                       wb <- openxlsx::loadWorkbook(hot_file)
                       
                       sum_f2 <- trait_summary_join(fieldbook = DF_f2, genotype = "INSTN",trait = trait_f2, 
                                                    trait_dict = trait_dict)
                       
                       sum_f3 <- trait_summary_join(fieldbook = DF_f3, genotype = "INSTN", trait = trait_f3, 
                                                    trait_dict = trait_dict)
                       
                       sum_f4 <- trait_summary_join(fieldbook = DF_f4, genotype = "INSTN", trait = trait_f4, 
                                                    trait_dict = trait_dict)
                         
                       sum_f5 <- trait_summary_join(fieldbook = DF_f5, genotype = "INSTN", trait = trait_f5, 
                                                    trait_dict = trait_dict)
                       
                       sum_f6 <- trait_summary_join(fieldbook = out_table_f6, genotype = "INSTN",
                                                    trait = c("texture","taste","appearance"), 
                                                    trait_dict = trait_dict)
                       
                       sum_f7 <- trait_summary_join(fieldbook = out_table_f7, genotype = "INSTN",
                                                    trait = c("texture","taste","appearance"),
                                                    trait_dict = trait_dict)
                       
                       
                       res <- Reduce(function(...) merge(..., all=T), list(sum_f2, sum_f3 , sum_f4, 
                                                                           sum_f5, sum_f6 , sum_f7
                                                                           ))
                       
                       openxlsx::addWorksheet(wb = wb, sheetName = "summary_global", gridLines = TRUE)
                       openxlsx::writeDataTable(wb, sheet = "summary_global", x = res,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                       
                    
                     }
                     
                     if(is.element("summary_global", pvs_sheet)){
                       
                       wb <- openxlsx::loadWorkbook(hot_file)
                       openxlsx::removeWorksheet(wb, sheet ="summary_global")
                       
                       sum_f2 <- trait_summary_join(fieldbook = DF_f2, genotype = "INSTN",trait = trait_f2, 
                                                    trait_dict = trait_dict)
                       
                       sum_f3 <- trait_summary_join(fieldbook = DF_f3, genotype = "INSTN", trait = trait_f3, 
                                                    trait_dict = trait_dict)
                       
                       sum_f4 <- trait_summary_join(fieldbook = DF_f4, genotype = "INSTN", trait = trait_f4, 
                                                    trait_dict = trait_dict)
                       
                       sum_f5 <- trait_summary_join(fieldbook = DF_f5, genotype = "INSTN", trait = trait_f5, 
                                                    trait_dict = trait_dict)
                       
                       sum_f6 <- trait_summary_join(fieldbook = out_table_f6, genotype = "INSTN",
                                                    trait = c("texture","taste","appearance"), 
                                                    trait_dict = trait_dict)
                       
                       sum_f7 <- trait_summary_join(fieldbook = out_table_f7, genotype = "INSTN",
                                                    trait = c("texture","taste","appearance"),
                                                    trait_dict = trait_dict)
                       
                       
                       res <- Reduce(function(...) merge(..., all=T), list(sum_f2, sum_f3 , sum_f4, 
                                                                           sum_f5, sum_f6 , sum_f7
                       ))
                       
                       openxlsx::addWorksheet(wb = wb, sheetName = "summary_global", gridLines = TRUE)
                       openxlsx::writeDataTable(wb, sheet = "summary_global", x = res,colNames = TRUE, withFilter = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                       
                       
                      
                     }
                     
                     shell.exec(hot_file)
                     
                   } 
                   
                   else { 
                     
                     ##begin fbglobal
                     path <- fbglobal::get_base_dir()
                     path <- paste(path,"hot_fieldbook.rds", sep="\\")
                     DF <- readRDS(path)
                      #enf fbglobal
                     
                     
                     
                     
                     
                     #DF <- readRDS("hot_fieldbook.rds")
                     
                     
                     
                     
                     trait <- get_trait_fb(DF)
                   
                     
                   hot_design <- as.character(hot_params()$hot_design)
                   
                   if(is.element("FACTOR", names(DF))){
                     summary <- trait_summary_join(fieldbook = DF, genotype = "INSTN",factor="FACTOR",trait = trait, 
                                                   design = hot_design, trait_dict = trait_dict)
                   }
                   #print("4")
                   if(!is.element("FACTOR", names(DF))){
                     summary <- trait_summary_join(fieldbook = DF, genotype = "INSTN",trait = trait, 
                                                   design = hot_design, trait_dict = trait_dict)
                   }
                   
                    hot_file <- hot_path() 
                    wb <- openxlsx::loadWorkbook(hot_file)
                    sheets <- readxl::excel_sheets(path = hot_file)
                    
                   if(is.element("Fieldbook",sheets)){    
                     openxlsx::removeWorksheet(wb, "Fieldbook")
                   }
                    
                   if(is.element("Summary",sheets)){    
                     openxlsx::removeWorksheet(wb, "Summary")
                   }
                    
                   openxlsx::addWorksheet(wb = wb,sheetName = "Fieldbook",gridLines = TRUE)
                   openxlsx::writeDataTable(wb,sheet = "Fieldbook", x = DF,colNames = TRUE, withFilter = FALSE)
                   openxlsx::addWorksheet(wb = wb,sheetName = "Summary",gridLines = TRUE)
                   openxlsx::writeDataTable(wb,sheet = "Summary", x = summary ,colNames = TRUE, withFilter = FALSE)
                   openxlsx::saveWorkbook(wb = wb, file = hot_file, overwrite = TRUE) 
                   
                   traits <- traittools::get_trait_fb(DF)
                   traittools::col_validation_trait(file = hot_file,fbsheet = "Fieldbook",trait = traits,trait_dict = trait_dict)
                   traittools::col_trait_outlier(file = hot_file, sumsheet = "Summary",trait = trait)
                   shell.exec(hot_file)
                   
                   }
                 })
    #End of single fieldbooks
    
    
    #For many fieldbooks

    
    
  })  
  
}
