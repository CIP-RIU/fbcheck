library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(fbcheck)

ui = dashboardPage(
  dashboardHeader(title = "rhandsontable Example"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fieldbook Check", tabName = "table", icon = icon("dashboard")),
      menuItem(text = "Data Quality", shiny::fileInput(inputId = "upload_fieldbook",accept = ".xlsx",
                                                       label = "Upload Fieldbook"), icon = icon("check-circle")
               #menuSubItem(text = "Checking Fieldbook",tabName = "checking_fieldbook")
      )
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table",
       
              fluidRow(
                shinydashboard::box(
                rHandsontableOutput("hot",width = 1000, height = 800),width = 2000,height = 500,collapsible = TRUE)
                )
       
      ),
      tabItem(tabName = "summary",
               fluidRow(rHandsontableOutput("hot_summary",width = 1000, height = 800),width = 2000,height = 500,collapsible = TRUE)
       
      )
    )
  )
)

server = function(input, output) {
  
  
  fieldbook <- reactive({
    fb_file <- input$upload_fieldbook
    if(is.null(fb_file)){return()}
    #if(!is.null(fb_file)){
    file.copy(fb_file$datapath, paste(fb_file$datapath, ".xlsx", sep=""))
    fieldbook <- readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""), sheet = "Fieldbook") 
    fieldbook <- as.data.frame(fieldbook)
    inst <- readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Installation") # reverted to xlsx so all formulas are read as values
    mgt  <-  readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Crop_management")
    mtl  <-  readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Material List")
    mml	 <-  readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Minimal") # reverted to xlsx so all formulas are read as values
    typ	 <-  as.character(mml[mml$Factor=="Type of Trial","Value"])
    plot_size <- as.numeric(inst[stringr::str_detect(inst$Factor,"Plot size"),"Value"])
    plant_den <- as.numeric(inst[stringr::str_detect(inst$Factor,"Planting density"),"Value"])
    
    fieldbook <-  sbformula::sbcalculate(fb = fieldbook, plot.size = plot_size,plant.den = plant_den)   
    print(fieldbook)
    
    fieldbook
    #}   
  })
  
  crop <- reactive({
    fb_file <- input$upload_fieldbook
    if(is.null(fb_file)){return()}
    file.copy(fb_file$datapath, paste(fb_file$datapath, ".xlsx", sep=""))
    crop <- get.fb.param(paste(fb_file$datapath, ".xlsx", sep=""),sheet = "Minimal","Crop")
    crop <- as.character(crop)
    crop
    
  })
  
  
  output$hot = renderRHandsontable ({
  
    fieldbook_dashboard <- as.data.frame(fieldbook())
    fieldbook_dashboard$PLOT <- as.integer(fieldbook_dashboard$PLOT)
    fieldbook_dashboard$REP <- as.integer(fieldbook_dashboard$REP)
    
    crop_fieldbook <- as.character(crop())
    if(length(crop_fieldbook)==0){return()}
  
    if(length(crop_fieldbook)>0){
          
        if(crop_fieldbook=="potato"){
          load("table_dictionary_potato.rda")
          datadict <- potato_ontology
        }
        
        if(crop_fieldbook=="sweetpotato"){
          load("table_dictionary_sweetpotato.rda")
          datadict <- sweetpotato_ontology
        }
    
    }  
    
    dict_trait <- datadict$ABBR  
    fb_header <- names(fieldbook_dashboard)
    fb_trait <- intersect(dict_trait,fb_header)
    nt <- length(fb_trait)
    out_temp <- list() 
    renderer_trait <-  list()
  
  fb_file <- input$upload_fieldbook  
  if(is.null(fb_file)){return()}
  if(!is.null(fb_file)){
      
  for(i in 1:nt){
    out_temp[[1]]<- rhandsontable::rhandsontable(data = fieldbook_dashboard,readOnly = FALSE,useTypes = TRUE) #%>%  
    renderer_trait[[i]] <- render_trait(fb_trait[i],datadict)
    j <- i+1
    #print(j)
    out_temp[[j]] <- hot_col(hot = out_temp[[i]],col = fb_trait[i] ,readOnly = FALSE,
                               allowInvalid = TRUE,copyable = TRUE, renderer = renderer_trait[[i]]) 
   }
  k <- nt+1
  out_temp[[k]] %>%
    hot_context_menu(
      customOpts = list(
        csv = list(name = "Download to CSV",
                   callback = htmlwidgets::JS(
                     "function (key, options) {
                     var csv = csvString(this);
                     var link = document.createElement('a');
                     link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                     encodeURIComponent(csv));
                     link.setAttribute('download', 'data.csv');
                     document.body.appendChild(link);
                     link.click();
                     document.body.removeChild(link);
  }"))))
  
  
  }
  
})
  
}

shinyApp(ui, server)