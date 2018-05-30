#' UI for traittools and data quality for HIDAP-SweetPotatoBase
#' Returns user friendly ui for HIDAP-SweetPotatoBase
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title display title name
#' @param name UI TabName
#' @export

fbcheck_ui_sbase <- function(type="tab", title="Data Quality and Processing", name="data_processing") {       
  
  #begin data_processing tabItem
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          shinyjs::useShinyjs(),
                          #shinyjs::extendShinyjs(text = "shinyjs.downloadData = function() { location.reload(); }"),
                          
                          # shinyWidgets::awesomeRadio(inputId = "fbdesign_dsource_sbase", 
                          #                            label = "Select data source", choices = c("HIDAP", 
                          #                                                                      "FieldBookApp-SPBase"), selected = "FieldBookApp-SPBase", 
                          #                            inline = TRUE, checkbox = TRUE),
                          # 
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            
                            # conditionalPanel(
                            #   condition = "input.fbdesign_dsource_sbase == 'HIDAP'",
                            #   
                            #   try(shinyFiles::shinyFilesButton('file_sbase', 'File select', 'Please select a file',FALSE)),
                               shiny::actionButton("calculate_sbase", "Calculate",icon("play-circle-o")),
                            #   HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                            #   shiny::actionLink('exportButton_sbase', 'Download data'),
                            #   HTML('</div>'),
                            #   br(),
                            #   br()#,  
                            # ),
                            
                            # conditionalPanel(
                            #   condition = "input.fbdesign_dsource_sbase == 'HIDAP'",
                            #   
                            #   # HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                            #   # #shiny::actionLink('exportButton_fbapp_sbase', 'Download data'),
                            #   # HTML('</div>')#,
                            #   
                            # ),
                            
                            #conditionalPanel(
                            #  condition = "input.fbdesign_dsource_sbase == 'FieldBookApp-SPBase'",

                                shiny::fileInput(inputId = "file_fbapp_sbase", label = "Choose CSV File", multiple = FALSE,
                                                 accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
                                #actionButton('reset', 'Reset Input'),
                               
                                HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                                shiny::downloadLink('downloadData', 'Download'),
                                HTML('</div>'),
                             # )
                            #),
                            
                            
                            
                            #tabsetPanel(
                            tabBox(width = 12,
                                   tabPanel("Standard Modules", #begin tabset "CHECK"
                                            
                                            # conditionalPanel(
                                            #   condition = "input.fbdesign_dsource_sbase == 'HIDAP'",
                                            #   
                                              #uiOutput("fbcheck_genofilter_sbase"),
                                              #uiOutput("fbcheck_factorfilter_sbase"),
                                              
                                            #),
                                            
                                            #                                      fluidRow(
                                            #                                        shinyFiles::shinyFilesButton('file', 'File select', 'Please select a file',FALSE),
                                            #                                        shiny::actionButton("calculate", "Calculate",icon("play-circle-o")),
                                            #                                        HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                                            #                                        shiny::actionLink('exportButton', 'Download data'),
                                            #                                        HTML('</div>'),
                                            shinysky::shinyalert("alert_fbapp_warning_sbase", FALSE, auto.close.after = 4),
                                            
                                            # conditionalPanel(
                                            #   condition = "input.fbdesign_dsource_sbase == 'HIDAP'",
                                            #   
                                            #   box(rHandsontableOutput("hot_btable_sbase",height = "1400px",width = "1000px"),
                                            #       height = "3400px",width ="2400px")#,
                                            # ),
                                            
                                            #conditionalPanel(
                                            #  condition = "input.fbdesign_dsource_sbase == 'FieldBookApp-SPBase'",
                                            
                                              box(rHandsontableOutput("hot_btable_fbapp_sbase",height = "100%",width = "100%"),
                                                  height = "3400px",width ="2400px")#,
                                            #),
                                            
                                            #                                      ),
                                            
                                            # tags$style(type='text/css', "#file_fbapp_sbase { width:150px; margin-top: 25px;}"),
                                            # tags$style(HTML('#file_fbapp_sbase {background-color:#0099cc; color: #ffffff}'))#,  
                                            #tags$style(type='text/css', "#calculate_sbase { width:150px; margin-top: 25px;}"),
                                            #tags$style(HTML('#calculate_sbase {background-color:#21b073; color: #ffffff}'))
                                            
                                   )
                                   
                                   #,#end tab Panel "CHECK"
                                   
                                   
                                   #### Hiden Special Modules during September Preview Release -----------------
                                   
                                   
                                  
                                   
                                   #### Hiden Special Modules during September Preview Release -----------------
                                   
                                   
                            )
                          ),
                          br(),
                          br(),
                          br()
                          
                          
  )#End data_processing tabItem
  
}

