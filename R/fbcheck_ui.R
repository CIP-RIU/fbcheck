#' UI for traittools
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title display title name
#' @param name UI TabName
#' @export

fbcheck_ui <- function(type="tab", title="Data Quality and Processing", name="data_processing") {       
  
  #begin data_processing tabItem
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          shinyWidgets::awesomeRadio(inputId = "fbdesign_dsource", 
                                       label = "Select data source", choices = c("HIDAP", 
                                                                            "FieldBookApp-SPBase"), selected = "HIDAP", 
                                       inline = TRUE, checkbox = TRUE),
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = NULL,
                            
                            conditionalPanel(
                              condition = "input.fbdesign_dsource == 'HIDAP'",
                              
                                  try(shinyFiles::shinyFilesButton('file', 'File select', 'Please select a file',FALSE)),
                                  shiny::actionButton("calculate", "Calculate",icon("play-circle-o")),
                                  HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                                  shiny::actionLink('exportButton', 'Download data'),
                                  HTML('</div>'),
                                  br(),
                                  br()#,  
                            ),
                            
                            conditionalPanel(
                              condition = "input.fbdesign_dsource == 'HIDAP'",
                              
                                HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                                shiny::actionLink('exportButton_fbapp', 'Download data'),
                                HTML('</div>')#,
                              
                            ),
                            
                            conditionalPanel(
                              condition = "input.fbdesign_dsource == 'FieldBookApp-SPBase'",
                              
                                  fileInput("file_fbapp", "Choose CSV File",accept = c("text/csv","text/comma-separated-values,text/plain",
                                              ".csv")
                                  )
                            ),
                            
                           
                            
                          #tabsetPanel(
                          tabBox(width = 12,
                            tabPanel("Standard Modules", #begin tabset "CHECK"
                                     
                                     conditionalPanel(
                                       condition = "input.fbdesign_dsource == 'HIDAP'",
                                     
                                      uiOutput("fbcheck_genofilter"),
                                      uiOutput("fbcheck_factorfilter")
                                     
                                      ),
                                     
#                                      fluidRow(
#                                        shinyFiles::shinyFilesButton('file', 'File select', 'Please select a file',FALSE),
#                                        shiny::actionButton("calculate", "Calculate",icon("play-circle-o")),
#                                        HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
#                                        shiny::actionLink('exportButton', 'Download data'),
#                                        HTML('</div>'),
                                        shinysky::shinyalert("alert_fb_warning", FALSE, auto.close.after = 4),

                                      conditionalPanel(
                                        condition = "input.fbdesign_dsource == 'HIDAP'",

                                        box(rHandsontableOutput("hot_btable",height = "1400px",width = "1000px"),
                                            height = "3400px",width ="2400px")#,
                                      ),

                                      conditionalPanel(
                                        condition = "input.fbdesign_dsource == 'FieldBookApp-SPBase'",

                                        box(rHandsontableOutput("hot_btable_fbapp",height = "1400px",width = "1000px"),
                                            height = "3400px",width ="2400px")#,
                                      ),

#                                      ),
                                     
                                     tags$style(type='text/css', "#file { width:150px; margin-top: 25px;}"),
                                     tags$style(HTML('#file {background-color:#0099cc; color: #ffffff}')),  
                                     tags$style(type='text/css', "#calculate { width:150px; margin-top: 25px;}"),
                                     tags$style(HTML('#calculate {background-color:#21b073; color: #ffffff}'))
                                     
                            )
                              
                             ,#end tab Panel "CHECK"
      

#### Hiden Special Modules during September Preview Release -----------------


                           tabPanel("Special Modules", #begin Trait List Panel"
                                     fluidRow(
                                         shinydashboard::tabBox(
                                           title = "PVS",
                                           # The id lets us use input$tabset1 on the server to find the current tab

                                           id = "tabset1", width = "2400px",

                                           tabPanel("Sel_Criteria","Selection Criteria",
                                                    br(),
                                                    shinyTree::shinyTree("fbcheckSelect_criteria",search = TRUE,checkbox = TRUE)
                                                    ),

                                           tabPanel("Form_1","Selection Criteria",
                                                   shinydashboard::box(rHandsontableOutput("hot_f1_btable",height = "1400px",width = "1400px"),
                                                                       height = "3400px",width ="3400px")),

                                           tabPanel("Form_2","Select Clones at Flowering Stage",
                                                    shinydashboard::box(rHandsontableOutput("hot_f2_btable",height = "1400px",width = "1400px"),
                                                                        height = "3400px",width ="2400px")),

                                           tabPanel("Form_3","Select Clones at Harvest Stage",
                                                    shinydashboard::box(rHandsontableOutput("hot_f3_btable",height = "1400px",width = "1400px"),
                                                                        height = "3400px",width ="2400px")),

                                           tabPanel("Form_4","Harvest Mother",
                                                    shinydashboard::box(rHandsontableOutput("hot_f4_btable",height = "1400px",width = "1400px"),
                                                                        height = "3400px",width ="2400px")),

                                           tabPanel("Form_5","Harvest Baby",
                                                    shinydashboard::box(rHandsontableOutput("hot_f5_btable",height = "1400px",width = "1400px"),
                                                                        height = "3400px",width ="2400px")),

                                           tabPanel("Form_6","Organoleptic_mother",
                                                    shinydashboard::box(rHandsontableOutput("hot_f6_btable",height = "1400px",width = "1400px"),
                                                                        height = "3400px",width ="2400px")),

                                           tabPanel("Form_7","Organoleptic_baby",
                                                    shinydashboard::box(rHandsontableOutput("hot_f7_btable",height = "1400px",width = "1400px"),
                                                                        height = "3400px",width ="2400px")),

                                           tabPanel("Form_8","Dormancy",
                                                    shinydashboard::box(rHandsontableOutput("hot_f8_btable",height = "1400px",width = "1400px"),
                                                                        height = "3400px",width ="2400px")),

                                           tabPanel("Form_9","Storage",
                                                    shinydashboard::box(rHandsontableOutput("hot_f9_btable",height = "1400px",width = "1400px"),
                                                                        height = "3400px",width ="2400px"))

                                         )
                                     )
#                                      fluidRow(
#                                        box(rHandsontableOutput("hot_td_trait",height = "1400px",width = "1400px"),
#                                            height = "3400px",width ="2400px")
#                                      )#end fluidRow
                            )

#### Hiden Special Modules during September Preview Release -----------------


                              )
                          ),
                        br(),
                        br(),
                        br()


  )#End data_processing tabItem
  
}

