#' Server component for traittools in fbcheck
#'
#' Returns server side components
#' @author Omar Benites
#' @param input shinyserver input
#' @param output nameo of the output element
#' @param session shinyserver session
#' @param values The reactive values
#' @export

fbcheck_server <- function (input, output, session, values) 
{
  volumes <- shinyFiles::getVolumes()
  shinyFileChoose(input, "file", roots = volumes, session = session, 
                  restrictions = system.file(package = "base"), filetypes = c("xlsx"))
  hot_path <- reactive({
    req(input$file)
    if (is.null(input$file)) {
      return(NULL)
    }
    validate(need(input$file != "", label = "Please enter an XLSX file. XLS files are forbidden"))
    if (length(input$file) == 0) {
      return(NULL)
    }
    if (length(input$file) > 0) {
      hot_file <- as.character(parseFilePaths(volumes, 
                                              input$file)$datapath)
    }
  })
  hot_bdata <- reactive({
    hot_file <- hot_path()
    print(hot_file)
    if (length(hot_file) == 0) {
      return(NULL)
    }
    if (length(hot_file) > 0) {
      print(hot_trial())
      if (hot_trial() == "Participatory Varietal Selection") {
        pvs_sheet_list <- c("F1_selection_criteria", 
                            "F2_select_clones_flowering", "F3_select_clones_harvest", 
                            "F4_harvest_mother", "F5_harvest_baby", "F6_organoleptic_mother", 
                            "F7_organoleptic_baby", "F8_postharvest_dormancy", 
                            "F9_postharvest_clones_storage")
        fb_sheets <- readxl::excel_sheets(hot_file)
        fb_sheets <- fb_sheets[fb_sheets %in% pvs_sheet_list]
        hot_bdata <- lapply(X = fb_sheets, function(x) openxlsx::read.xlsx(xlsxFile = hot_file, 
                                                                           sheet = x, na.strings = TRUE))
        names(hot_bdata) <- fb_sheets
        hot_bdata
      }
      else {
        hot_bdata <- readxl::read_excel(path = hot_file, 
                                        sheet = "Fieldbook")
        if (is.element("DATESP", names(hot_bdata))) {
          hot_bdata$DATESP <- as.character(hot_bdata$DATESP)
        }
        if (is.element("EDATE", names(hot_bdata))) {
          hot_bdata$EDATE <- as.character(hot_bdata$EDATE)
        }
        hot_bdata
      }
      hot_bdata
    }
  })
  hot_params <- reactive({
    hot_file <- hot_path()
    if (length(hot_file) == 0) {
      return(NULL)
    }
    if (length(hot_file) > 0) {
      hot_param <- readxl::read_excel(path = hot_file, 
                                      sheet = "Installation")
      hot_design <- get_fb_param(hot_param, "Experimental_design_abbreviation")
      hot_plot_size <- get_fb_param(hot_param, "Plot_size_(m2)")
      hot_plant_den <- get_fb_param(hot_param, "Planting_density_(plants/Ha)")
      hot_factor_lvl1 <- get_fb_param(hot_param, "Factor_name_1")
      hot_factor_lvl2 <- get_fb_param(hot_param, "Factor_name_2")
      hot_psize_mother <- get_pvs_param(pvs_data = hot_param, 
                                        col_param = "Mother", row_param = "Plot_size_(m2)")
      hot_psize_baby <- get_pvs_param(pvs_data = hot_param, 
                                      col_param = "Baby_1", row_param = "Plot_size_(m2)")
      hot_pden_mother <- get_pvs_param(pvs_data = hot_param, 
                                       col_param = "Mother", row_param = "Planting_density_(plants/Ha)")
      hot_pden_baby <- get_pvs_param(pvs_data = hot_param, 
                                     col_param = "Baby_1", row_param = "Planting_density_(plants/Ha)")
      hot_params_list <- list(hot_design = hot_design, 
                              hot_plot_size = hot_plot_size, hot_plant_den = hot_plant_den, 
                              hot_factor_lvl1 = hot_factor_lvl1, hot_factor_lvl2 = hot_factor_lvl2, 
                              hot_psize_mother = hot_psize_mother, hot_pden_mother = hot_pden_mother, 
                              hot_psize_baby = hot_psize_baby, hot_pden_baby = hot_pden_baby)
    }
  })
  hot_crop <- reactive({
    hot_file <- hot_path()
    if (length(hot_file) == 0) {
      return(NULL)
    }
    if (length(hot_file) > 0) {
      hot_param <- readxl::read_excel(path = hot_file, 
                                      sheet = "Minimal")
      hot_crop <- get_fb_param(hot_param, "Crop")
    }
  })
  hot_trial <- reactive({
    hot_file <- hot_path()
    if (length(hot_file) == 0) {
      return(NULL)
    }
    if (length(hot_file) > 0) {
      hot_param <- readxl::read_excel(path = hot_file, 
                                      sheet = "Minimal")
      hot_trial <- get_fb_param(hot_param, "Type_of_Trial")
    }
  })
  hot_mgt <- reactive({
    hot_file <- hot_path()
    if (length(hot_file) == 0) {
      return(NULL)
    }
    if (length(hot_file) > 0) {
      hot_mgt <- openxlsx::read.xlsx(xlsxFile = hot_file, 
                                     sheet = "Crop_management", detectDates = TRUE)
      hot_mgt
    }
  })
  hot_mtl <- reactive({
    hot_file <- hot_path()
    if (length(hot_file) == 0) {
      return(NULL)
    }
    if (length(hot_file) > 0) {
      hot_mtl <- openxlsx::read.xlsx(xlsxFile = hot_file, 
                                     sheet = "Material_List", detectDates = TRUE)
      hot_mtl
    }
  })
  shiny::observeEvent(input$calculate, {
    fb <- hot_bdata()
    fb_names <- names(fb)
    trial <- hot_trial()
    print(trial)
    if (trial == "Late blight" && !is.element("REP", fb_names)) {
      shinysky::showshinyalert(session, "alert_fb_warning", 
                               paste("Warning: Scale AUDPC can not be calculated because your design does not have repetition column (Ex. Westcott design)"), 
                               styleclass = "warning")
    }
  })
  output$hot_btable <- renderRHandsontable({
    req(input$file)
    if (hot_trial() != "Participatory Varietal Selection") {
      values <- shiny::reactiveValues(hot_btable = hot_bdata())
      DF <- NULL
      if (!is.null(input$hot_btable)) {
        DF = hot_to_r(input$hot_btable)
        values[["hot_btable"]] = DF
      }
      else if (!is.null(values[["hot_btable"]])) {
        DF = values[["hot_btable"]]
      }
      if (input$calculate > 0) {
        hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
        hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
        DF = values[["hot_btable"]]
        DF <- as.data.frame(DF)
        DF <- traittools::calculate_trait_variables(fb = DF, 
                                                    plot_size = hot_plot_size, plant_den = hot_plant_den, 
                                                    mgt = hot_mgt(), mtl = hot_mtl(), trial_type = hot_trial())
      }
      if (!is.null(DF)) {
        traits <- get_trait_fb(DF)
        path <- fbglobal::get_base_dir()
        path <- paste(path, "hot_fieldbook.rds", sep = "\\")
        saveRDS(DF, path)
        crop <- hot_crop()
        trial <- hot_trial()
        print("checking with crop ontology")
        trait_dict <- get_crop_ontology(crop = crop)
        traittools::col_render_trait(fieldbook = DF, 
                                     trait = traits, trait_dict = trait_dict)
      }
    }
  })
  output$fbcheckSelect_criteria <- shinyTree::renderTree({
    out <- selcriteria
    out
  })
  output$fbcheck_genofilter <- renderUI({
    ifelse("INSTN" %in% names(hot_bdata()), sel <- "INSTN", 
           sel <- 1)
    selectInput(inputId = "sel_fbcheck_genofilter", label = "Select Genotypes", 
                choices = names(hot_bdata()), multiple = TRUE, selected = sel)
  })
  output$fbcheck_factorfilter <- renderUI({
    selectInput(inputId = "sel_fbcheck_factorfilter", label = "Summary by", 
                choices = names(hot_bdata()), multiple = TRUE, selected = 1)
  })
  pvs_fb_sheets <- reactive({
    hot_file <- hot_path()
    pvs_sheet <- readxl::excel_sheets(hot_file)
    pvs_sheet
  })
  output$hot_f1_btable <- renderRHandsontable({
    values <- shiny::reactiveValues(hot_f1_btable = hot_bdata()$F1_selection_criteria)
    DF <- NULL
    if (!is.null(input$hot_f1_btable)) {
      DF = hot_to_r(input$hot_f1_btable)
      values[["hot_f1_btable"]] = DF
    }
    else if (!is.null(values[["hot_f1_btable"]])) {
      DF = values[["hot_f1_btable"]]
    }
    if (input$calculate > 0) {
      DF = values[["hot_f1_btable"]]
      DF <- as.data.frame(DF)
      DF <- calculate_form_scriteria(data = DF)
    }
    if (!is.null(DF)) {
      traits <- get_trait_fb(DF)
      if (!is.element("F1_selection_criteria", pvs_fb_sheets())) {
        DF <- data.frame()
        saveRDS(DF, "hot_f1_fieldbook.rds")
      }
      else {
        saveRDS(DF, "hot_f1_fieldbook.rds")
      }
      crop <- hot_crop()
      rhandsontable::rhandsontable(data = DF, width = 2000)
    }
  })
  output$hot_f2_btable <- renderRHandsontable({
    values <- shiny::reactiveValues(hot_f2_btable = hot_bdata()$F2_select_clones_flowering)
    DF <- NULL
    if (!is.null(input$hot_f2_btable)) {
      DF = hot_to_r(input$hot_f2_btable)
      values[["hot_f2_btable"]] = DF
    }
    else if (!is.null(values[["hot_f2_btable"]])) {
      DF = values[["hot_f2_btable"]]
    }
    if (input$calculate > 0) {
      DF = values[["hot_f2_btable"]]
      DF <- as.data.frame(DF)
      DF <- calculate_form_sclones(data = DF)
    }
    if (!is.null(DF)) {
      traits <- get_trait_fb(DF)
      if (!is.element("F2_select_clones_flowering", pvs_fb_sheets())) {
        DF <- data.frame()
        saveRDS(DF, "hot_f2_fieldbook.rds")
        rhandsontable::rhandsontable(data = DF, width = 2000)
      }
      else {
        saveRDS(DF, "hot_f2_fieldbook.rds")
        crop <- hot_crop()
        trait_dict <- get_crop_ontology(crop = crop)
        traittools::col_render_trait(fieldbook = DF, 
                                     trait = traits, trait_dict = trait_dict)
      }
    }
  })
  output$hot_f3_btable <- renderRHandsontable({
    values <- shiny::reactiveValues(hot_f3_btable = hot_bdata()$F3_select_clones_harvest)
    DF <- NULL
    if (!is.null(input$hot_f3_btable)) {
      DF = hot_to_r(input$hot_f3_btable)
      values[["hot_f3_btable"]] = DF
    }
    else if (!is.null(values[["hot_f3_btable"]])) {
      DF = values[["hot_f3_btable"]]
    }
    if (input$calculate > 0) {
      DF = values[["hot_f3_btable"]]
      DF <- as.data.frame(DF)
      DF <- calculate_form_sclones(data = DF)
    }
    if (!is.null(DF)) {
      traits <- get_trait_fb(DF)
      if (!is.element("F3_select_clones_harvest", pvs_fb_sheets())) {
        DF <- data.frame()
        saveRDS(DF, "hot_f3_fieldbook.rds")
        rhandsontable::rhandsontable(data = DF, width = 2000)
      }
      else {
        saveRDS(DF, "hot_f3_fieldbook.rds")
        crop <- hot_crop()
        trait_dict <- get_crop_ontology(crop = crop)
        traittools::col_render_trait(fieldbook = DF, 
                                     trait = traits, trait_dict = trait_dict)
      }
    }
  })
  output$hot_f4_btable <- renderRHandsontable({
    values <- shiny::reactiveValues(hot_f4_btable = hot_bdata()$F4_harvest_mother)
    DF <- NULL
    if (!is.null(input$hot_f4_btable)) {
      DF = hot_to_r(input$hot_f4_btable)
      values[["hot_f4_btable"]] = DF
    }
    else if (!is.null(values[["hot_f4_btable"]])) {
      DF = values[["hot_f4_btable"]]
    }
    if (input$calculate > 0) {
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      DF = values[["hot_f4_btable"]]
      DF <- as.data.frame(DF)
      DF <- calculate_form_harvest(data = DF, plot_size = hot_plot_size, 
                                   plant_den = hot_plant_den)
    }
    if (!is.null(DF)) {
      traits <- get_trait_fb(DF)
      if (!is.element("F4_harvest_mother", pvs_fb_sheets())) {
        DF <- data.frame()
        saveRDS(DF, "hot_f4_fieldbook.rds")
        rhandsontable::rhandsontable(data = DF, width = 2000)
      }
      else {
        saveRDS(DF, "hot_f4_fieldbook.rds")
        crop <- hot_crop()
        trait_dict <- get_crop_ontology(crop = crop)
        traittools::col_render_trait(fieldbook = DF, 
                                     trait = traits, trait_dict = trait_dict)
      }
    }
  })
  output$hot_f5_btable <- renderRHandsontable({
    values <- shiny::reactiveValues(hot_f5_btable = hot_bdata()$F5_harvest_baby)
    DF <- NULL
    if (!is.null(input$hot_f5_btable)) {
      DF = hot_to_r(input$hot_f5_btable)
      values[["hot_f5_btable"]] = DF
    }
    else if (!is.null(values[["hot_f5_btable"]])) {
      DF = values[["hot_f5_btable"]]
    }
    if (input$calculate > 0) {
      hot_plot_size <- as.numeric(hot_params()$hot_psize_baby)
      hot_plant_den <- as.numeric(hot_params()$hot_pden_baby)
      DF = values[["hot_f5_btable"]]
      DF <- as.data.frame(DF)
      DF <- calculate_form_harvest(data = DF, plot_size = hot_plot_size, 
                                   plant_den = hot_plant_den)
    }
    if (!is.null(DF)) {
      traits <- get_trait_fb(DF)
      if (!is.element("F5_harvest_baby", pvs_fb_sheets())) {
        DF <- data.frame()
        saveRDS(DF, "hot_f5_fieldbook.rds")
        rhandsontable::rhandsontable(data = DF, width = 2000)
      }
      else {
        saveRDS(DF, "hot_f5_fieldbook.rds")
        crop <- hot_crop()
        trait_dict <- get_crop_ontology(crop = crop)
        traittools::col_render_trait(fieldbook = DF, 
                                     trait = traits, trait_dict = trait_dict)
      }
    }
  })
  output$hot_f6_btable <- renderRHandsontable({
    values <- shiny::reactiveValues(hot_f6_btable = hot_bdata()$F6_organoleptic_mother)
    DF <- NULL
    if (!is.null(input$hot_f6_btable)) {
      DF = hot_to_r(input$hot_f6_btable)
      values[["hot_f6_btable"]] = DF
    }
    else if (!is.null(values[["hot_f6_btable"]])) {
      DF = values[["hot_f6_btable"]]
    }
    if (input$calculate > 0) {
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      DF = values[["hot_f6_btable"]]
      DF <- as.data.frame(DF)
    }
    if (!is.null(DF)) {
      if (!is.element("F6_organoleptic_mother", pvs_fb_sheets())) {
        DF <- data.frame()
        saveRDS(DF, "hot_f6_fieldbook.rds")
      }
      else {
        saveRDS(DF, "hot_f6_fieldbook.rds")
      }
      rhandsontable::rhandsontable(data = DF)
    }
  })
  output$hot_f7_btable <- renderRHandsontable({
    values <- shiny::reactiveValues(hot_f7_btable = hot_bdata()$F7_organoleptic_baby)
    DF <- NULL
    if (!is.null(input$hot_f7_btable)) {
      DF = hot_to_r(input$hot_f7_btable)
      values[["hot_f7_btable"]] = DF
    }
    else if (!is.null(values[["hot_f7_btable"]])) {
      DF = values[["hot_f7_btable"]]
    }
    if (input$calculate > 0) {
      hot_plot_size <- as.numeric(hot_params()$hot_plot_size)
      hot_plant_den <- as.numeric(hot_params()$hot_plant_den)
      DF = values[["hot_f7_btable"]]
      DF <- as.data.frame(DF)
    }
    if (!is.null(DF)) {
      if (!is.element("F7_organoleptic_baby", pvs_fb_sheets())) {
        DF <- data.frame()
        saveRDS(DF, "hot_f7_fieldbook.rds")
      }
      else {
        saveRDS(DF, "hot_f7_fieldbook.rds")
      }
      rhandsontable::rhandsontable(data = DF)
    }
  })
  output$hot_f8_btable <- renderRHandsontable({
    values <- shiny::reactiveValues(hot_f8_btable = hot_bdata()$F8_postharvest_dormancy)
    DF <- NULL
    if (!is.null(input$hot_f8_btable)) {
      DF = hot_to_r(input$hot_f8_btable)
      values[["hot_f8_btable"]] = DF
    }
    else if (!is.null(values[["hot_f8_btable"]])) {
      DF = values[["hot_f8_btable"]]
    }
    if (input$calculate > 0) {
      DF = values[["hot_f8_btable"]]
      DF <- as.data.frame(DF)
      DF <- calculate_form_dormancy(data = DF)
    }
    if (!is.null(DF)) {
      if (!is.element("F8_postharvest_dormancy", pvs_fb_sheets())) {
        DF <- data.frame()
        saveRDS(DF, "hot_f8_fieldbook.rds")
      }
      else {
        saveRDS(DF, "hot_f8_fieldbook.rds")
      }
      rhandsontable::rhandsontable(data = DF)
    }
  })
  output$hot_f9_btable <- renderRHandsontable({
    values <- shiny::reactiveValues(hot_f9_btable = hot_bdata()$F9_postharvest_clones_storage)
    DF <- NULL
    if (!is.null(input$hot_f9_btable)) {
      DF = hot_to_r(input$hot_f9_btable)
      values[["hot_f9_btable"]] = DF
    }
    else if (!is.null(values[["hot_f9_btable"]])) {
      DF = values[["hot_f9_btable"]]
    }
    if (input$calculate > 0) {
      DF = values[["hot_f9_btable"]]
      DF <- as.data.frame(DF)
      DF <- calculate_form_postharvest(data = DF)
    }
    if (!is.null(DF)) {
      if (!is.element("F9_postharvest_clones_storage", 
                      pvs_fb_sheets())) {
        DF <- data.frame()
        saveRDS(DF, "hot_f9_fieldbook.rds")
      }
      else {
        saveRDS(DF, "hot_f9_fieldbook.rds")
      }
      rhandsontable::rhandsontable(data = DF)
    }
  })
  output$hot_td_trait = renderRHandsontable({
    td_trait <- orderBy(~ABBR, td_trait)
    rhandsontable(data = td_trait)
  })
  shiny::observeEvent(input$exportButton, {
    try({
      withProgress(message = "Downloading Fieldbook and Applying Format...", 
                   value = 0, {
                     shiny::incProgress(amount = 1/30, message = "selecting trait dictionary")
                     crop <- hot_crop()
                     trial <- hot_trial()
                     if (crop == "potato") {
                       trait_dict2 <- table_module_potato
                       shiny::incProgress(2/30, message = "potato dictionary")
                     }
                     if (crop == "sweetpotato") {
                       trait_dict2 <- table_module_sweetpotato
                       shiny::incProgress(2/30, message = "sweet potato dictionary")
                     }
                     trait_dict <- trait_dict2
                     if (hot_trial() == "Participatory Varietal Selection") {
                       trait_dict <- table_module_potato
                       hot_design <- as.character(hot_params()$hot_design)
                       hot_file <- hot_path()
                       pvs_sheet_list <- c("F1_selection_criteria", 
                                           "F2_select_clones_flowering", "F3_select_clones_harvest", 
                                           "F4_harvest_mother", "F5_harvest_baby", 
                                           "F6_organoleptic_mother", "F7_organoleptic_baby", 
                                           "F8_postharvest_dormancy", "F9_postharvest_clones_storage")
                       print("f1")
                       pvs_sheet <- readxl::excel_sheets(hot_file)
                       print("f1")
                       if (is.element(pvs_sheet_list[1], pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         DF_f1 <- readRDS("hot_f1_fieldbook.rds")
                         openxlsx::removeWorksheet(wb, pvs_sheet_list[1])
                         openxlsx::addWorksheet(wb = wb, sheetName = "F1_selection_criteria", 
                                                gridLines = TRUE)
                         openxlsx::writeDataTable(wb, sheet = "F1_selection_criteria", 
                                                  x = DF_f1, colNames = TRUE, withFilter = FALSE)
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       else {
                         DF_f1 <- NULL
                       }
                       print("f2")
                       if (is.element(pvs_sheet_list[2], pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         DF_f2 <- readRDS("hot_f2_fieldbook.rds")
                         trait_f2 <- get_trait_fb(DF_f2)
                         openxlsx::removeWorksheet(wb, pvs_sheet_list[2])
                         openxlsx::addWorksheet(wb = wb, sheetName = "F2_select_clones_flowering", 
                                                gridLines = TRUE)
                         openxlsx::writeDataTable(wb, sheet = "F2_select_clones_flowering", 
                                                  x = DF_f2, colNames = TRUE, withFilter = FALSE)
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       else {
                         DF_f2 <- NULL
                       }
                       print("f3")
                       if (is.element(pvs_sheet_list[3], pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         DF_f3 <- readRDS("hot_f3_fieldbook.rds")
                         trait_f3 <- get_trait_fb(DF_f3)
                         openxlsx::removeWorksheet(wb, pvs_sheet_list[3])
                         openxlsx::addWorksheet(wb = wb, sheetName = "F3_select_clones_harvest", 
                                                gridLines = TRUE)
                         openxlsx::writeDataTable(wb, sheet = "F3_select_clones_harvest", 
                                                  x = DF_f3, colNames = TRUE, withFilter = FALSE)
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       else {
                         DF_f3 <- NULL
                       }
                       print("f4")
                       if (is.element(pvs_sheet_list[4], pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         DF_f4 <- readRDS("hot_f4_fieldbook.rds")
                         trait_f4 <- get_trait_fb(DF_f4)
                         openxlsx::removeWorksheet(wb, pvs_sheet_list[4])
                         openxlsx::addWorksheet(wb = wb, sheetName = "F4_harvest_mother", 
                                                gridLines = TRUE)
                         openxlsx::writeDataTable(wb, sheet = "F4_harvest_mother", 
                                                  x = DF_f4, colNames = TRUE, withFilter = FALSE)
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       else {
                         DF_f4 <- NULL
                       }
                       print("f5")
                       if (is.element(pvs_sheet_list[5], pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         DF_f5 <- readRDS("hot_f5_fieldbook.rds")
                         trait_f5 <- get_trait_fb(DF_f5)
                         openxlsx::removeWorksheet(wb, pvs_sheet_list[5])
                         openxlsx::addWorksheet(wb = wb, sheetName = "F5_harvest_baby", 
                                                gridLines = TRUE)
                         openxlsx::writeDataTable(wb, sheet = "F5_harvest_baby", 
                                                  x = DF_f5, colNames = TRUE, withFilter = FALSE)
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       else {
                         DF_f5 <- NULL
                       }
                       print("f6")
                       if (is.element(pvs_sheet_list[6], pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         DF_f6 <- readRDS("hot_f6_fieldbook.rds")
                         openxlsx::removeWorksheet(wb, pvs_sheet_list[6])
                         openxlsx::addWorksheet(wb = wb, sheetName = "F6_organoleptic_mother", 
                                                gridLines = TRUE)
                         openxlsx::writeDataTable(wb, sheet = "F6_organoleptic_mother", 
                                                  x = DF_f6, colNames = TRUE, withFilter = FALSE)
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       else {
                         DF_f6 <- NULL
                       }
                       print("f7")
                       if (is.element(pvs_sheet_list[7], pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         DF_f7 <- readRDS("hot_f7_fieldbook.rds")
                         openxlsx::removeWorksheet(wb, pvs_sheet_list[7])
                         openxlsx::addWorksheet(wb = wb, sheetName = "F7_organoleptic_baby", 
                                                gridLines = TRUE)
                         openxlsx::writeDataTable(wb, sheet = "F7_organoleptic_baby", 
                                                  x = DF_f7, colNames = TRUE, withFilter = FALSE)
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       else {
                         DF_f7 <- NULL
                       }
                       print("f8")
                       if (is.element(pvs_sheet_list[8], pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         DF_f8 <- readRDS("hot_f8_fieldbook.rds")
                         openxlsx::removeWorksheet(wb, pvs_sheet_list[8])
                         trait_f8 <- get_trait_fb(DF_f8)
                         openxlsx::addWorksheet(wb = wb, sheetName = "F8_postharvest_dormancy", 
                                                gridLines = TRUE)
                         openxlsx::writeDataTable(wb, sheet = "F8_postharvest_dormancy", 
                                                  x = DF_f8, colNames = TRUE, withFilter = FALSE)
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       else {
                         DF_f8 <- NULL
                       }
                       print("f9")
                       if (is.element(pvs_sheet_list[9], pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         DF_f9 <- readRDS("hot_f9_fieldbook.rds")
                         openxlsx::removeWorksheet(wb, pvs_sheet_list[9])
                         openxlsx::addWorksheet(wb = wb, sheetName = "F9_postharvest_clones_storage", 
                                                gridLines = TRUE)
                         openxlsx::writeDataTable(wb, sheet = "F9_postharvest_clones_storage", 
                                                  x = DF_f9, colNames = TRUE, withFilter = FALSE)
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       else {
                         DF_f9 <- NULL
                       }
                       print("sum mother")
                       if (is.element("summary_organoleptic_mother", 
                                      pvs_sheet)) {
                         print("remove sheet mother summ orgh")
                         wb <- openxlsx::loadWorkbook(hot_file)
                         openxlsx::removeWorksheet(wb, sheet = "summary_organoleptic_mother")
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       if (is.element(pvs_sheet_list[6], pvs_sheet)) {
                         out_table_f6 <- form_checker(form = DF_f6, 
                                                      hot_file = hot_file, sheet_name = "summary_organoleptic_mother")
                       }
                       else {
                         out_table_f6 <- NULL
                       }
                       print("sum baby")
                       if (is.element("summary_organoleptic_baby", 
                                      pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         openxlsx::removeWorksheet(wb, sheet = "summary_organoleptic_baby")
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       if (is.element(pvs_sheet_list[7], pvs_sheet)) {
                         out_table_f7 <- form_checker(form = DF_f7, 
                                                      hot_file = hot_file, sheet_name = "summary_organoleptic_baby")
                       }
                       else {
                         out_table_f7 <- NULL
                       }
                       print("sum global")
                       if (is.element("summary_global", pvs_sheet)) {
                         wb <- openxlsx::loadWorkbook(hot_file)
                         openxlsx::removeWorksheet(wb, sheet = "summary_global")
                         openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                overwrite = TRUE)
                       }
                       wb <- openxlsx::loadWorkbook(hot_file)
                       if (!is.null(DF_f2)) {
                         sum_f2 <- trait_summary_join(fieldbook = DF_f2, 
                                                      genotype = "INSTN", trait = trait_f2, 
                                                      design = "RCBD", trait_dict = trait_dict)
                         trait_f2_old <- as.vector(t(outer(trait_f2, 
                                                           c("_n", "_Mean", "_sd"), paste, sep = "")))
                         trait_f2_new <- paste(trait_f2_old, "_FS", 
                                               sep = "")
                         setnames(sum_f2, trait_f2_old, trait_f2_new)
                       }
                       else {
                         sum_f2 <- NULL
                       }
                       print("sum f3")
                       if (!is.null(DF_f3)) {
                         sum_f3 <- trait_summary_join(fieldbook = DF_f3, 
                                                      genotype = "INSTN", trait = trait_f3, 
                                                      design = "RCBD", trait_dict = trait_dict)
                         trait_f3_old <- as.vector(t(outer(trait_f3, 
                                                           c("_n", "_Mean", "_sd"), paste, sep = "")))
                         trait_f3_new <- paste(trait_f3_old, "_HS", 
                                               sep = "")
                         setnames(sum_f3, trait_f3_old, trait_f3_new)
                       }
                       else {
                         sum_f3 <- NULL
                       }
                       print("sum f4")
                       if (!is.null(DF_f4)) {
                         sum_f4 <- trait_summary_join(fieldbook = DF_f4, 
                                                      genotype = "INSTN", trait = trait_f4, 
                                                      design = "RCBD", trait_dict = trait_dict)
                         trait_f4_old <- as.vector(t(outer(trait_f4, 
                                                           c("_n", "_Mean", "_sd"), paste, sep = "")))
                         trait_f4_new <- paste(trait_f4_old, "_mother", 
                                               sep = "")
                         setnames(sum_f4, trait_f4_old, trait_f4_new)
                       }
                       else {
                         sum_f4 <- NULL
                       }
                       print("sum f5")
                       if (!is.null(DF_f5)) {
                         sum_f5 <- trait_summary_join(fieldbook = DF_f5, 
                                                      genotype = "INSTN", trait = trait_f5, 
                                                      design = "RCBD", trait_dict = trait_dict)
                         trait_f5_old <- as.vector(t(outer(trait_f5, 
                                                           c("_n", "_Mean", "_sd"), paste, sep = "")))
                         trait_f5_new <- paste(trait_f5_old, "_baby", 
                                               sep = "")
                         setnames(sum_f5, trait_f5_old, trait_f5_new)
                       }
                       else {
                         sum_f5 <- NULL
                       }
                       print("sum f6")
                       if (!is.null(out_table_f6)) {
                         sum_f6 <- trait_summary_join(fieldbook = out_table_f6, 
                                                      genotype = "INSTN", design = "RCBD", 
                                                      trait = c("TEXTURE", "TASTE", "APPEARANCE"), 
                                                      trait_dict = trait_dict)
                         trait_org_mother = c("TEXTURE", "TASTE", 
                                              "APPEARANCE")
                         trait_f6_old <- as.vector(t(outer(trait_org_mother, 
                                                           c("_n", "_Mean", "_sd"), paste, sep = "")))
                         trait_f6_new <- paste(trait_f6_old, "_mother", 
                                               sep = "")
                         setnames(sum_f6, trait_f6_old, trait_f6_new)
                       }
                       else {
                         sum_f6 <- NULL
                       }
                       print("sum f7")
                       if (!is.null(out_table_f7)) {
                         sum_f7 <- trait_summary_join(fieldbook = out_table_f7, 
                                                      genotype = "INSTN", design = "RCBD", 
                                                      trait = c("TEXTURE", "TASTE", "APPEARANCE"), 
                                                      trait_dict = trait_dict)
                         trait_org_baby = c("TEXTURE", "TASTE", 
                                            "APPEARANCE")
                         trait_f7_old <- as.vector(t(outer(trait_org_baby, 
                                                           c("_n", "_Mean", "_sd"), paste, sep = "")))
                         trait_f7_new <- paste(trait_f7_old, "_baby", 
                                               sep = "")
                         setnames(sum_f7, trait_f7_old, trait_f7_new)
                       }
                       else {
                         sum_f7 <- NULL
                       }
                       print("sum f8")
                       if (!is.null(DF_f8)) {
                         sum_f8 <- trait_summary_join(fieldbook = DF_f8, 
                                                      genotype = "INSTN", trait = trait_f8, 
                                                      design = "RCBD", trait_dict = trait_dict)
                         trait_f8_old <- as.vector(t(outer(trait_f8, 
                                                           c("_n", "_Mean", "_sd"), paste, sep = "")))
                         trait_f8_new <- paste(trait_f8_old, "_mother", 
                                               sep = "")
                         setnames(sum_f8, trait_f8_old, trait_f8_new)
                       }
                       else {
                         sum_f8 <- NULL
                       }
                       print("sum f9")
                       if (!is.null(DF_f9)) {
                         sum_f9 <- trait_summary_join(fieldbook = DF_f9, 
                                                      genotype = "INSTN", trait = c("SCORE_MEN", 
                                                                                    "SCORE_WOMEN", "SCORE_GLOBAL", "PCT_MEN", 
                                                                                    "PCT_WOMEN", "PCT_GLOBAL"), design = "RCBD", 
                                                      trait_dict = trait_dict)
                         print(sum_f9)
                         trait_f9 <- c("SCORE_MEN", "SCORE_WOMEN", 
                                       "SCORE_GLOBAL", "PCT_MEN", "PCT_WOMEN", 
                                       "PCT_GLOBAL")
                         trait_f9_old <- as.vector(t(outer(trait_f9, 
                                                           c("_n", "_Mean", "_sd"), paste, sep = "")))
                         trait_f9_new <- paste(trait_f9_old, "_mother", 
                                               sep = "")
                         setnames(sum_f9, trait_f9_old, trait_f9_new)
                       }
                       else {
                         sum_f9 <- NULL
                       }
                       print("enter reduce")
                       out_sum_list <- list(sum_f2, sum_f3, sum_f4, 
                                            sum_f5, sum_f6, sum_f7, sum_f8, sum_f9)
                       out_sum_list <- out_sum_list[!sapply(out_sum_list, 
                                                            is.null)]
                       res <- Reduce(function(...) merge(..., all = T), 
                                     out_sum_list)
                       openxlsx::addWorksheet(wb = wb, sheetName = "summary_global", 
                                              gridLines = TRUE)
                       openxlsx::writeDataTable(wb, sheet = "summary_global", 
                                                x = res, colNames = TRUE, withFilter = FALSE, 
                                                keepNA = FALSE)
                       openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                              overwrite = TRUE)
                       print("shell exec")
                       shell.exec(hot_file)
                     }
                     else {
                       shiny::incProgress(3/30, message = "reading installation data")
                       path <- fbglobal::get_base_dir()
                       path <- paste(path, "hot_fieldbook.rds", 
                                     sep = "\\")
                       DF <- readRDS(path)
                       trait <- get_trait_fb(DF)
                       hot_design <- as.character(hot_params()$hot_design)
                       hot_design <- stringr::str_trim(hot_design, 
                                                       side = "both")
                       genofilter <- input$sel_fbcheck_genofilter
                       print(genofilter)
                       factorfilter <- input$sel_fbcheck_factorfilter
                       print(factorfilter)
                       if (is.na(hot_design)) {
                         hot_design <- "RCBD"
                       }
                       shiny::incProgress(4/30, message = "summarizing data")
                       print("doing summary")
                       try(summary <- trait_summary_join(fieldbook = DF, 
                                                         genotype = genofilter, trait = trait, design = hot_design, 
                                                         trait_dict = trait_dict))
                       shiny::incProgress(12/30, message = "checking for another factors")
                       print("checking factor filters to sum up")
                       if (length(factorfilter) > 0 || !is.null(factorfilter)) {
                         print(factorfilter)
                         try(summaryFactors <- trait_summary_join(fieldbook = DF, 
                                                                  genotype = genofilter, factor = factorfilter, 
                                                                  trait = trait, design = hot_design, trait_dict = trait_dict))
                       }
                       print("end checking factors")
                       if (all(is.element(c("FEMALE", "MALE"), names(DF)))) {
                         try(summary <- trait_summary_join(fieldbook = DF, 
                                                           genotype = "INSTN", factor = c("FEMALE", 
                                                                                          "MALE"), trait = trait, design = hot_design, 
                                                           trait_dict = trait_dict))
                       }
                       if (all(is.element(c("LINE", "TESTER"), names(DF)))) {
                         try(summary <- trait_summary_join(fieldbook = DF, 
                                                           genotype = "LINE", factor = c("TESTER"), 
                                                           trait = trait, design = hot_design, trait_dict = trait_dict))
                       }
                       print("end summary")
                       print("detection of sheets")
                       shiny::incProgress(13/30, message = "detection of sheets in field book file")
                       hot_file <- hot_path()
                       try(wb <- openxlsx::loadWorkbook(hot_file))
                       try(sheets <- readxl::excel_sheets(path = hot_file))
                       print("after loadworkbook")
                       shiny::incProgress(14/30, message = "checking for another factors")
                       if (is.element("Fieldbook", sheets)) {
                         try(openxlsx::removeWorksheet(wb, "Fieldbook"))
                       }
                       shiny::incProgress(15/30, message = "checking fieldbook sheet")
                       if (is.element("Summary", sheets)) {
                         try(openxlsx::removeWorksheet(wb, "Summary"))
                       }
                       shiny::incProgress(16/30, message = "checking summary sheet")
                       if (is.element("Summary_factor", sheets)) {
                         try(openxlsx::removeWorksheet(wb, "Summary_factor"))
                       }
                       shiny::incProgress(17/30, message = "checking summary by facto sheet")
                       shiny::incProgress(18/30, message = "adding sheets")
                       print("beggining of openxlsx functions")
                       shiny::incProgress(19/30, message = "writting fieldbook data")
                       try(openxlsx::addWorksheet(wb = wb, sheetName = "Fieldbook", 
                                                  gridLines = TRUE))
                       try(openxlsx::writeDataTable(wb, sheet = "Fieldbook", 
                                                    x = DF, colNames = TRUE, withFilter = FALSE))
                       if (hot_design != "UNDR") {
                         shiny::incProgress(11/15, message = "writting summary by genotype data")
                         try(openxlsx::addWorksheet(wb = wb, sheetName = "Summary", 
                                                    gridLines = TRUE))
                         try(openxlsx::writeDataTable(wb, sheet = "Summary", 
                                                      x = summary, colNames = TRUE, withFilter = FALSE))
                         if (length(factorfilter) > 0) {
                           shiny::incProgress(11/15, message = "writting summary by factor data")
                           try(openxlsx::addWorksheet(wb = wb, sheetName = "Summary_factor", 
                                                      gridLines = TRUE))
                           try(openxlsx::writeDataTable(wb, sheet = "Summary_factor", 
                                                        x = summaryFactors, colNames = TRUE, 
                                                        withFilter = FALSE))
                         }
                         try(openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                    overwrite = TRUE))
                       }
                       print("traittols functions")
                       try(traits <- traittools::get_trait_fb(DF))
                       shiny::incProgress(22/30, message = "checking outliers")
                       print("col validation")
                       try(traittools::col_validation_trait(file = hot_file, 
                                                            fbsheet = "Fieldbook", trait = traits, 
                                                            trait_dict = trait_dict))
                       print("col outkiers")
                       trial <- hot_trial()
                       trial <- stringr::str_trim(trial, side = "both")
                       shiny::incProgress(30/30, message = "exporting fieldbook file (excel)")
                       try(openxlsx::saveWorkbook(wb = wb, file = hot_file, 
                                                  overwrite = TRUE))
                       print("ejecucion")
                       try(shell.exec(hot_file))
                     }
                   })
    })
  })
  
  ##############  FieldBookApp  ############## 
  
  
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
    
    
    ####### Create Unique ID ######## 
    servName <- "fbappdatapath.rds"
    uploadDate  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
    ranStr <-  stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
    servName <- paste(uploadDate, ranStr, servName , sep= "-") #nombre sin extensions!!!!
    dirNameExtFile <- fbglobal::get_base_dir() #get directory of the file with fileName
    
    
    ####### Reactive values  #######
    hot_bdata_sbase <- hot_bdata_sbase2 
    values <-  shiny::reactiveValues(
      hot_btable_fbapp_sbase = hot_bdata_sbase#()
    )
    DF <- NULL
    
    ####### Detect if hot_btable_fbapp_sbase has data  #######
    if (!is.null(input$hot_btable_fbapp_sbase)) {
      print("if 1")
      DF = hot_to_r(input$hot_btable_fbapp_sbase)
      #values[["hot_btable_fbapp_sbase"]] = DF
      
      ## Important Note: in case users upload different files, they will see:
      dirNameExtFile <- fbglobal::get_base_dir()
      #fileNameExtFile <-  paste(dirNameExtFile, "fbappdatapath.rds")
      fileNameExtFile <-  paste0(dirNameExtFile, servName)
      
      #if(file.exists(file.path(dirNameExtFile, "fbappdatapath.rds") )){
      if(file.exists(fileNameExtFile)) {    
        former_datapath <- readRDS(file = fileNameExtFile)
        if(hot_fbapp_path()!= former_datapath){
          DF <- hot_bdata_sbase2
        } 
      }
      ###  end important note
      values[["hot_btable_fbapp_sbase"]] = DF
      
    } else if (!is.null(values[["hot_btable_fbapp_sbase"]])) {
      DF = values[["hot_btable_fbapp_sbase"]]
      print("if 2")
    } 
    
    
    if(!is.null(DF)){
      
      dsource <- 2
      traits <- traittools::get_trait_fb(DF, dsource = dsource)
      #print(traits)
      path <- fbglobal::get_base_dir() ##begin fbglobal
      path <- file.path(path,"hot_fieldbook_sbase.rds")
      saveRDS(DF, path)
      
      file_fbapp <- input$file_fbapp_sbase
      value_datapath <- file_fbapp$datapath 
      datapath <- file.path(fbglobal::get_base_dir(), "fbappdatapath.rds")
      saveRDS(value_datapath, file =  datapath)
      
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
  
  ############## end FieldBookApp ############## 
  
  
}
